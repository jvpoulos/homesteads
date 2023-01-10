##################################
# Matrix completion simulations  #
##################################

library(MCPanel)
library(matrixStats)
library(Matrix)
library(tictoc)
library(MASS)
library(data.table)
library(reshape)
library(reshape2)
library(emfactor)
library(glmnet)

source('code/utils.R')
source('code/IFE.R')

# Setup parallel processing
doMPI <- TRUE
if(doMPI){
  library(doMPI)
  
  # Start cluster
  cl <- startMPIcluster()
  
  # Register cluster
  registerDoMPI(cl)
  
  # Check cluster size
  print(paste0("cluster size: ", clusterSize(cl)))
  
} else{
  library(parallel)
  library(doParallel)
  library(foreach)
  
  cores <- parallel::detectCores()
  print(paste0("number of cores used: ", cores))
  
  cl <- parallel::makeCluster(cores, outfile="")
  
  doParallel::registerDoParallel(cl) # register cluster
}

MCsim <- function(N,T,R,T0,N_t,beta_sc,loading_sc,logi_sc,shift_sc,estimator=c("mc_plain","mc_weights","ADH","ENT","DID","IFE"),cores,n){
  
  # check inputs 
  if(N!=T){
    stop("Matrix must be square, N=T")
  }
  
  # set the seed
  print(paste0("run number: ", n))
  set.seed(n, "L'Ecuyer-CMRG")
  
  mask <- matrix(0, nrow=N, ncol=T) # treat matrix
  
  while(any(rowSums(mask)<=1) || max(rowSums(mask))<T){ # ensure that there are ST and NT units
    
    # make the mean and var of the potential outcomes matrix
    
    mean_vec_0_N <- rep(0,N)
    
    sigma_mat_N <- diag(N)
    sigma_mat_N[sigma_mat_N==0] <- 0.2
    
    mean_vec_1_R <- rep(1, R)
    sigma_mat_R <- diag(R)
    sigma_mat_R[sigma_mat_R==0] <- 0.2
    
    # Create Matrices
    U <- loading_sc*mvrnorm(N, mu=mean_vec_1_R, Sigma=sigma_mat_R) # factor loadings 
    V <- cbind(arima.sim(model = list(ar = 0.3), n = T)+0.25*(1:T),replicate(R-1,arima.sim(model = list(ar = 0.3), n = T)+0.05*(1:T))) # factors AR(1) time series process with ϕ=0.3 and linear trend
    X <- replicate(T,arima.sim(model = list(ar = 0.3), n = N)+0.1*(1:T))
    
    # True outcome model
    true_mat <- U%*%t(V)# potential outcomes under control
    
    # True missingness model
    
    e <-plogis(cbind(true_mat[,1:(T0-1)],replicate((T-T0+1),true_mat[,(T0-1)]))+X) # prob of being missing (treated/missing)
    e <- boundProbs(e) # winsorize extreme probabilities 
    mask <- stag_adapt(M = matrix(1, nrow=N, ncol=T) , N_t = N_t, T0 = T0, treat_indices = 0, weights = e[,T0]) # 0s missing and to be imputed; 1s are observed
  }
  
  fr_obs <- sum(mask)/(N*T) # store fraction observed entries
  print(paste0("fraction observed: ", fr_obs))
  
  # get vector of initial treatment periods for N_t treated units
  
  A <- aggregate(col ~ row,
                  data = which(mask == 0, arr.ind = TRUE),
                  FUN = function(x) x[1])$col # gives the intial T0s for treated units

  ST <- aggregate(col ~ row,
                  data = which(mask == 0, arr.ind = TRUE),
                  FUN = function(x) x[1])$row  # switch treated indices
  NT <- setdiff(1:N, ST) # control indices
  
  # Elapsed time weighted treatment
  
  z_weights <- matrix(0,N,T,byrow = TRUE)
  
  for(i in 1:length(ST)){
    z_weights[ST[i],] <- c(plogis(A[i]:1, scale=logi_sc),plogis(1:(ncol(mask)-A[i]),scale=logi_sc))
  }
  
  shift <- z_weights*shift_sc*(1-mask)
  
  shifted_mat <- true_mat - (shift*true_mat)
  
  shifted_X <- X - (shift*X) 
  
  # Calc. real ATT on the ST
  
  att.true <- mean(apply((shifted_mat-true_mat),1,nzmean)[ST]) # Y(ST) - Y(NT)
  
  # Observed outcome matrix
  
  obs_mat <- shifted_mat * mask 
  
  if(estimator%in%c("mc_weights")){
    
    ## Estimate propensity scores
    
    p.mod <- cv.glmnet(x=cbind(shifted_X,obs_mat[,1:T0]), y=(1-mask), family="mgaussian", alpha=1,nfolds=10,intercept=FALSE)
    W <- predict(p.mod, cbind(shifted_X,obs_mat[,1:T0]))[,,1]
    
    p.weights <- matrix(NA, nrow=nrow(mask), ncol=ncol(mask), dimnames = list(rownames(mask), colnames(mask)))
    p.weights <- boundProbs(W)/(1-boundProbs(W))
  }
  
  ## ------ ------ ------ ------ ------
  ## MC-NNM plain (no weighting, no covariate)
  ## ------ ------ ------ ------ ------
  
  est_mc_plain <- list()
  if(estimator=="mc_plain"){
    est_mc_plain <- mcnnm_cv(M = obs_mat, mask = mask, W = matrix(1, nrow(mask),ncol(mask)), to_estimate_u = 1, to_estimate_v = 1, rel_tol = 1e-05, is_quiet = 1)
    
    est_mc_plain$rankL <- rankMatrix(t(est_mc_plain$L), method="qr.R")[1]
    est_mc_plain$rank_error <- abs(est_mc_plain$rankL-R)
    print(paste("MC-NNM (Plain) rank error:", round(est_mc_plain$rank_error,3)))
    
    est_mc_plain$Mhat <- est_mc_plain$L + replicate(T,est_mc_plain$u) + t(replicate(N,est_mc_plain$v))
    est_mc_plain$tau <- (shifted_mat-est_mc_plain$Mhat) # estimated treatment effect, Y(ST) - Y(NT)
    est_mc_plain$err <- (est_mc_plain$tau - (shift*true_mat)) # error (wrt to ground truth)
    
    est_mc_plain$msk_err <- est_mc_plain$err*(1-mask) # masked error (wrt to ground truth)
    est_mc_plain$RMSE <- sqrt((1/sum(1-mask)) * sum(est_mc_plain$msk_err^2)) # RMSE on test set (wrt to ground truth)
    print(paste("MC-NNM (Plain) RMSE:", round(est_mc_plain$RMSE,3)))
    
    est_mc_plain$att <- apply(est_mc_plain$tau*(1-mask),1,nzmean)[ST]
    est_mc_plain$att.bar <- mean(est_mc_plain$att)
    est_mc_plain$abs.bias <- abs(est_mc_plain$att.bar-att.true)
    est_mc_plain$rel.abs.bias <- est_mc_plain$abs.bias/abs(att.true)
    print(paste("MC-NNM (Plain) abs. bias:", round(est_mc_plain$abs.bias,3)))
    
    # bootstrap variance estimation
    df_mc_plain <- widetoLong(Y= shifted_mat, mask = mask, X = NULL)
    est_mc_plain$boot_var <- clustered_bootstrap(current_data_realized_long=df_mc_plain, estimator="mc_plain", N=nrow(shifted_mat), T=ncol(shifted_mat), T0=T0, B = 399, est_weights = FALSE, ncores = cores)
    print(paste("MC-NNM (Plain) variance:", round(est_mc_plain$boot_var,3)))
    
    est_mc_plain$cp <- CI_test(est_coefficent=est_mc_plain$att.bar, real_coefficent=att.true, est_var=est_mc_plain$boot_var)
    print(paste("MC-NNM (Plain) CP:", round(est_mc_plain$cp,3)))
    
    est_mc_plain$CI_width <- abs(boot_CI(est_coefficent=est_mc_plain$att.bar, est_var=est_mc_plain$boot_var)$lb-boot_CI(est_coefficent=est_mc_plain$att.bar, est_var=est_mc_plain$boot_var)$ub)
    print(paste("MC-NNM (Plain) CI width:", round(est_mc_plain$CI_width,3)))
  }
  
  ## ------ ------ ------ ------ ------
  ## MC-NNM weights (no covariate)
  ## ------ ------ ------ ------ ------
  
  est_mc_weights <- list()
  if(estimator=="mc_weights"){
    est_mc_weights <- mcnnm_cv(M = obs_mat, mask = mask, W = p.weights, to_estimate_u = 1, to_estimate_v = 1, rel_tol = 1e-05, is_quiet = 1)
    
    est_mc_weights$rankL <- rankMatrix(t(est_mc_weights$L), method="qr.R")[1]
    est_mc_weights$rank_error <- abs(est_mc_weights$rankL-R)
    print(paste("MC-NNM (weights) rank error:", round(est_mc_weights$rank_error,3)))
    
    est_mc_weights$Mhat <- est_mc_weights$L + replicate(T,est_mc_weights$u) + t(replicate(N,est_mc_weights$v))
    est_mc_weights$tau <- (shifted_mat-est_mc_weights$Mhat) # estimated treatment effect, Y(AT) - Y(ST)
    est_mc_weights$err <- (est_mc_weights$tau - (shift*true_mat)) # error (wrt to ground truth)
    
    est_mc_weights$msk_err <- est_mc_weights$err*(1-mask) # masked error (wrt to ground truth)
    est_mc_weights$RMSE <- sqrt((1/sum(1-mask)) * sum(est_mc_weights$msk_err^2)) # RMSE on test set (wrt to ground truth)
    print(paste("MC-NNM (weights) RMSE:", round(est_mc_weights$RMSE,3)))
    
    est_mc_weights$att <- apply(est_mc_weights$tau*(1-mask),1,nzmean)[ST]
    est_mc_weights$att.bar <- mean(est_mc_weights$att)
    est_mc_weights$abs.bias <- abs(est_mc_weights$att.bar-att.true)
    est_mc_weights$rel.abs.bias <- est_mc_weights$abs.bias/abs(att.true)
    print(paste("MC-NNM (weights) abs. bias:", round(est_mc_weights$abs.bias,3)))
    
    # bootstrap variance estimation
    df_mc_weights <- widetoLong(Y= shifted_mat, mask = mask, X = shifted_X)
    est_mc_weights$boot_var <- clustered_bootstrap(current_data_realized_long=df_mc_weights, estimator="mc_weights", N=nrow(shifted_mat), T=ncol(shifted_mat), T0=T0, B = 399, est_weights = TRUE, ncores = cores)
    print(paste("MC-NNM (weights) variance:", round(est_mc_weights$boot_var,3)))
    
    est_mc_weights$cp <- CI_test(est_coefficent=est_mc_weights$att.bar, real_coefficent=att.true, est_var=est_mc_weights$boot_var)
    print(paste("MC-NNM (weights) CP:", round(est_mc_weights$cp,3)))
    
    est_mc_weights$CI_width <- abs(boot_CI(est_coefficent=est_mc_weights$att.bar, est_var=est_mc_weights$boot_var)$lb-boot_CI(est_coefficent=est_mc_weights$att.bar, est_var=est_mc_weights$boot_var)$ub)
    print(paste("MC-NNM (weights) CI width:", round(est_mc_weights$CI_width,3)))
  }
  
  ## -----
  ## ADH
  ## -----
  est_model_ADH <- list()
  if(estimator=="ADH"){
    est_model_ADH$Mhat <- adh_mp_rows(obs_mat, mask)
    est_model_ADH$tau <- (shifted_mat-est_model_ADH$Mhat) # estimated treatment effect
    est_model_ADH$err <- (est_model_ADH$tau - (shift*true_mat)) # error (wrt to ground truth)
    
    est_model_ADH$msk_err <- est_model_ADH$err*(1-mask) # masked error (wrt to ground truth)
    est_model_ADH$RMSE <- sqrt((1/sum(1-mask)) * sum(est_model_ADH$msk_err^2)) # RMSE on test set (wrt to ground truth)
    print(paste("ADH RMSE:", round(est_model_ADH$RMSE,3)))
    
    est_model_ADH$att <- apply(est_model_ADH$tau*(1-mask),1,nzmean)[ST]
    est_model_ADH$att.bar <- mean(est_model_ADH$att)
    est_model_ADH$abs.bias <- abs(est_model_ADH$att.bar-att.true)
    est_model_ADH$rel.abs.bias <- est_model_ADH$abs.bias/abs(att.true)
    print(paste("ADH abs. bias:", round(est_model_ADH$abs.bias,3)))
    
    # bootstrap variance estimation
    df_ADH <- widetoLong(Y= shifted_mat, mask = mask, X = NULL)
    est_model_ADH$boot_var <- clustered_bootstrap(current_data_realized_long=df_ADH, estimator="ADH", N=nrow(shifted_mat), T=ncol(shifted_mat), T0=T0, B = 399, est_weights = FALSE, ncores = cores)
    print(paste("ADH variance:", round(est_model_ADH$boot_var,3)))
    
    est_model_ADH$cp <- CI_test(est_coefficent=est_model_ADH$att.bar, real_coefficent=att.true, est_var=est_model_ADH$boot_var)
    print(paste("ADH CP:", round(est_model_ADH$cp,3)))
    
    est_model_ADH$CI_width <- abs(boot_CI(est_coefficent=est_model_ADH$att.bar, est_var=est_model_ADH$boot_var)$lb-boot_CI(est_coefficent=est_model_ADH$att.bar, est_var=est_model_ADH$boot_var)$ub)
    print(paste("ADH CI width:", round(est_model_ADH$CI_width,3)))
  }
  
  ## -----
  ## VT-EN : It does Not cross validate on alpha (only on lambda) and keep alpha = 1 (LASSO).
  ## -----
  
  est_model_ENT <- list()
  if(estimator=="ENT"){
    est_model_ENT$Mhat <- t(en_mp_rows(t(obs_mat), t(mask), num_alpha = 1))
    est_model_ENT$tau <- (shifted_mat-est_model_ENT$Mhat) # estimated treatment effect
    est_model_ENT$err <- (est_model_ENT$tau - (shift*true_mat)) # error (wrt to ground truth)
    
    est_model_ENT$msk_err <- est_model_ENT$err*(1-mask) # masked error (wrt to ground truth)
    est_model_ENT$RMSE <- sqrt((1/sum(1-mask)) * sum(est_model_ENT$msk_err^2)) # RMSE on test set (wrt to ground truth)
    print(paste("VT-EN RMSE:", round(est_model_ENT$RMSE,3)))
    
    est_model_ENT$att <- apply(est_model_ENT$tau*(1-mask),1,nzmean)[ST]
    est_model_ENT$att.bar <- mean(est_model_ENT$att)
    est_model_ENT$abs.bias <- abs(est_model_ENT$att.bar-att.true)
    est_model_ENT$rel.abs.bias <- est_model_ENT$abs.bias/abs(att.true)
    print(paste("VT-EN abs. bias:", round(est_model_ENT$abs.bias,3)))
    
    # bootstrap variance estimation
    df_ENT <- widetoLong(Y= shifted_mat, mask = mask, X = NULL)
    est_model_ENT$boot_var <- clustered_bootstrap(current_data_realized_long=df_ENT, estimator="ENT", N=nrow(shifted_mat), T=ncol(shifted_mat), T0=T0, B = 399, est_weights = FALSE, ncores = cores)
    print(paste("VT-EN variance:", round(est_model_ENT$boot_var,3)))
    
    est_model_ENT$cp <- CI_test(est_coefficent=est_model_ENT$att.bar, real_coefficent=att.true, est_var=est_model_ENT$boot_var)
    print(paste("VT-EN CP:", round(est_model_ENT$cp,3)))
    
    est_model_ENT$CI_width <- abs(boot_CI(est_coefficent=est_model_ENT$att.bar, est_var=est_model_ENT$boot_var)$lb-boot_CI(est_coefficent=est_model_ENT$att.bar, est_var=est_model_ENT$boot_var)$ub)
    print(paste("VT-EN CI width:", round(est_model_ENT$CI_width,3)))
  }
  
  ## -----
  ## DID
  ## -----
  est_model_DID <- list()
  if(estimator=="DID"){
    est_model_DID$Mhat <- DID(obs_mat, mask)
    est_model_DID$tau <- (shifted_mat-est_model_DID$Mhat) # estimated treatment effect
    est_model_DID$err <- (est_model_DID$tau  - (shift*true_mat)) # error (wrt to ground truth)
    
    est_model_DID$msk_err <- est_model_DID$err*(1-mask) # masked error (wrt to ground truth)
    est_model_DID$RMSE <- sqrt((1/sum(1-mask)) * sum(est_model_DID$msk_err^2)) # RMSE on test set (wrt to ground truth)
    print(paste("DID RMSE:", round(est_model_DID$RMSE,3)))
    
    est_model_DID$att <- apply(est_model_DID$tau*(1-mask),1,nzmean)[ST]
    est_model_DID$att.bar <- mean(est_model_DID$att)
    est_model_DID$abs.bias <- abs(est_model_DID$att.bar-att.true)
    est_model_DID$rel.abs.bias <- est_model_DID$abs.bias/abs(att.true)
    print(paste("DID abs. bias:", round(est_model_DID$abs.bias,3)))
    
    # bootstrap variance estimation
    df_DID <- widetoLong(Y= shifted_mat, mask = mask, X = NULL)
    est_model_DID$boot_var <- clustered_bootstrap(current_data_realized_long=df_DID, estimator="DID", N=nrow(shifted_mat), T=ncol(shifted_mat), T0=T0, B = 399, est_weights = FALSE, ncores = cores)
    print(paste("DID variance:", round(est_model_DID$boot_var,3)))
    
    est_model_DID$cp <- CI_test(est_coefficent=est_model_DID$att.bar, real_coefficent=att.true, est_var=est_model_DID$boot_var)
    print(paste("DID CP:", round(est_model_DID$cp,3)))
    
    est_model_DID$CI_width <- abs(boot_CI(est_coefficDID=est_model_DID$att.bar, est_var=est_model_DID$boot_var)$lb-boot_CI(est_coefficDID=est_model_DID$att.bar, est_var=est_model_DID$boot_var)$ub)
    print(paste("DID CI width:", round(est_model_DID$CI_width,3)))
  }
  
  ## ---------------
  ## IFEs
  ## ---------------
  
  est_model_IFE <- list()
  if(estimator=="IFE"){
    est_model_IFE$Mhat <- IFE(obs_mat, mask, k=2)
    est_model_IFE$tau <- (shifted_mat-est_model_IFE$Mhat) # estimated treatment effect
    est_model_IFE$err <- (est_model_IFE$tau - (shift*true_mat)) # error (wrt to ground truth)
    
    est_model_IFE$msk_err <- est_model_IFE$err*(1-mask) # masked error (wrt to ground truth)
    est_model_IFE$RMSE <- sqrt((1/sum(1-mask)) * sum(est_model_IFE$msk_err^2)) # RMSE on test set (wrt to ground truth)
    print(paste("IFE RMSE:", round(est_model_IFE$RMSE,3)))
    
    est_model_IFE$att <- apply(est_model_IFE$tau*(1-mask),1,nzmean)[ST]
    est_model_IFE$att.bar <- mean(est_model_IFE$att)
    est_model_IFE$abs.bias <- abs(est_model_IFE$att.bar-att.true)
    est_model_IFE$rel.abs.bias <- est_model_IFE$abs.bias/abs(att.true)
    print(paste("IFE abs. bias:", round(est_model_IFE$abs.bias,3)))
    
    # bootstrap variance estimation
    df_IFE <- widetoLong(Y= shifted_mat, mask = mask, X = NULL)
    est_model_IFE$boot_var <- clustered_bootstrap(current_data_realized_long=df_IFE, estimator="IFE", N=nrow(shifted_mat), T=ncol(shifted_mat), T0=T0, B = 399, est_weights = FALSE, ncores = cores)
    print(paste("IFE variance:", round(est_model_IFE$boot_var,3)))
    
    est_model_IFE$cp <- CI_test(est_coefficent=est_model_IFE$att.bar, real_coefficent=att.true, est_var=est_model_IFE$boot_var)
    print(paste("IFE CP:", round(est_model_IFE$cp,3)))
    
    est_model_IFE$CI_width <- abs(boot_CI(est_coefficIFE=est_model_IFE$att.bar, est_var=est_model_IFE$boot_var)$lb-boot_CI(est_coefficIFE=est_model_IFE$att.bar, est_var=est_model_IFE$boot_var)$ub)
    print(paste("IFE CI width:", round(est_model_IFE$CI_width,3)))
  }
  
  # cleanup
  cat(paste("Done with simulation run number",n, "\n"))
  return(list("N"=N, "T"=T, "R"=R, "T0"=T0, "N_t"=N_t,"beta_sc"=beta_sc,"loading_sc"=loading_sc, "logi_sc" = logi_sc, "shift_sc"=shift_sc,"estimator"=estimator,"fr_obs"= fr_obs,
              "est_mc_plain_RMSE"=est_mc_plain$RMSE,"est_mc_plain_abs_bias"=est_mc_plain$abs.bias,"est_mc_plain_rel_abs_bias"=est_mc_plain$rel.abs.bias,"est_mc_plain_cp"=est_mc_plain$cp,"est_mc_plain_boot_var"=est_mc_plain$boot_var,"est_mc_plain_CI_width"=est_mc_plain$CI_width,"est_mc_plain_rankL"=est_mc_plain$rankL, "est_mc_plain_rank_error"=est_mc_plain$rank_error,
              "est_mc_weights_RMSE"=est_mc_weights$RMSE,"est_mc_weights_abs_bias"=est_mc_weights$abs.bias,"est_mc_weights_rel_abs_bias"=est_mc_weights$rel.abs.bias,"est_mc_weights_cp"=est_mc_weights$cp,"est_mc_weights_boot_var"=est_mc_weights$boot_var,"est_mc_weights_CI_width"=est_mc_weights$CI_width,"est_mc_weights_rankL"=est_mc_weights$rankL, "est_mc_weights_rank_error"=est_mc_weights$rank_error,
              "est_model_ADH_RMSE"=est_model_ADH$RMSE,"est_model_ADH_abs_bias"=est_model_ADH$abs.bias,"est_model_ADH_rel_abs_bias"=est_model_ADH$rel.abs.bias,"est_model_ADH_cp"=est_model_ADH$cp,"est_model_ADH_boot_var"=est_model_ADH$boot_var,"est_model_ADH_width"=est_model_ADH$CI_width,
              "est_model_ENT_RMSE"=est_model_ENT$RMSE,"est_model_ENT_abs_bias"=est_model_ENT$abs.bias,"est_model_ENT_rel_abs_bias"=est_model_ENT$rel.abs.bias,"est_model_ENT_cp"=est_model_ENT$cp,"est_model_ENT_boot_var"=est_model_ENT$boot_var,"est_model_ENT_width"=est_model_ENT$CI_width,
              "est_model_DID_RMSE"=est_model_DID$RMSE,"est_model_DID_abs_bias"=est_model_DID$abs.bias,"est_model_DID_rel_abs_bias"=est_model_DID$rel.abs.bias,"est_model_DID_cp"=est_model_DID$cp,"est_model_DID_boot_var"=est_model_DID$boot_var,"est_model_DID_width"=est_model_DID$CI_width,
              "est_model_IFE_RMSE"=est_model_IFE$RMSE,"est_model_IFE_abs_bias"=est_model_IFE$abs.bias,"est_model_IFE_rel_abs_bias"=est_model_IFE$rel.abs.bias,"est_model_IFE_cp"=est_model_IFE$cp,"est_model_IFE_boot_var"=est_model_IFE$boot_var,"est_model_IFE_width"=est_model_IFE$CI_width))
}

# define settings for simulation
settings <- expand.grid("NT"=c(60**2,80**2),
                        "N_t" =c(0.5), # N_ST = 30,40,50
                        "shift_scale"=c(0.1),
                        "R"=c(10,20,40),
                        "estimator"=c("mc_plain","mc_weights","ADH","ENT","DID","IFE"))

args <- commandArgs(trailingOnly = TRUE) # command line arguments
thisrun <- settings[as.numeric(args[1]),] 

N <- sqrt(as.numeric(thisrun[1])) # Number of units
T <- sqrt(as.numeric(thisrun[1]))  # Number of time-periods
T0 <- ceiling(T*0.05)  # initial time period in staggered adoption setting
N_t <- ceiling(N*as.numeric(thisrun[2]))  # Number of ST units
shift_sc <- as.numeric(thisrun[3]) # shift scale
R <- as.numeric(thisrun[4])
estimator <- as.character(thisrun[5]$estimator)

beta_sc <- 0.1 # beta scale
loading_sc <- 0.1 # factor loading scale
logi_sc <- 8 # logistic scale

if(doMPI){
  cores <- parallel::detectCores()
  print(paste0("number of cores used: ", cores))
}

n.runs <- 500 # Num. simulation runs

output_dir <- './outputs/'
simulation_version <- paste0(format(Sys.time(), "%Y%m%d"),"/")
if(!dir.exists(output_dir)){
  print(paste0('create folder for outputs at: ', output_dir))
  dir.create(output_dir)
}
output_dir <- paste0(output_dir, simulation_version)
if(!dir.exists(output_dir)){
  print(paste0('create folder for outputs at: ', output_dir))
  dir.create(output_dir)
}

setting <- paste0("N = ", N, ", T = ", T, ", R = ",R, "T0 = " ,T0, "N_t", N_t, ", beta_sc = ", beta_sc, ", shift_sc = ", shift_sc, ", estimator = ", estimator)
tic(print(paste0("setting: ",setting)))

results <- foreach(i = 1:n.runs, .combine='cbind', .packages =c("MCPanel","matrixStats","Matrix","MASS","data.table","reshape","reshape2","emfactor"), .verbose = FALSE) %dopar% {
  MCsim(N,T,R,T0,N_t,beta_sc,loading_sc,logi_sc,shift_sc,estimator,cores,n=i)
}
results
saveRDS(results, paste0(output_dir,"results_","N_",N,"_T_",T,"_R_", R, "_T0_",T0, "_N_t_", N_t, "_shift_sc_",shift_sc,"_estimator_",estimator,"_n_",n.runs,".rds"))

print(toc())

if(doMPI){
  closeCluster(cl) # close down MPIcluster
  mpi.finalize()
}else{
  stopCluster(cl)
}