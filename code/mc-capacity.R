####################################
# State gov't spending simulations #
####################################

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
library(ggplot2)
library(latex2exp)
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

## Reading data
CapacitySim <- function(outcomes.missing,outcomes.imputed,covars.x,d,T0,sim,treated.indices,estimator=c("mc_plain","mc_weights","ADH","ENT","DID","IFE"),cores,n){
  
  # set the seed
  print(paste0("run number: ", n))
  set.seed(n, "L'Ecuyer-CMRG")
  
  # specify outcome and treatment matrix
  Y.missing <- outcomes.missing[[d]]$M # NxT  # to train on: 0s = missing
  Y <- outcomes.imputed[[d]]$M # NxT  # imputed outcomes
  missing.mat <- outcomes.imputed[[d]]$M.missing
  missing.mat[is.na(missing.mat)] <- 0 # 0s are missing/imputed
  treat <- outcomes.imputed[[d]]$mask # NxT masked matrix 
  
  ## working with control units
  treat <- treat[!rownames(treat)%in%treated.indices,] 
  missing.mat <- missing.mat[!rownames(missing.mat)%in%treated.indices,]
  Y.missing <- Y.missing[!rownames(Y.missing)%in%treated.indices,] 
  Y <- Y[!rownames(Y)%in%treated.indices,] 
  covars.x <- covars.x[!rownames(covars.x)%in%treated.indices,]
  
  ## Setting up the configuration
  N <- nrow(treat)
  T <- ncol(treat)
  number_T0 <- 5
  t0 <- ceiling(T*((1:number_T0)*2-1)/(2*number_T0))[T0]
  N_t <- ceiling(N*0.5) # no. treated units desired <=N
  
  att.true <- 0

  ## Simultaneuous (simul_adapt) or Staggered adoption (stag_adapt)
  
  covars.x <- scale(matrix(covars.x[,1],N,T) + matrix(covars.x[,2],N,T) + matrix(covars.x[,3],N,T) +matrix(covars.x[,4],N,T)) # need to combine covars in NxT matrix
  
  e <-plogis(scale(cbind(Y[,1:(t0-1)],replicate((T-t0+1),Y[,(t0-1)]))) + covars.x) # prob of being missing (treated/missing)
  e <- boundProbs(e) # winsorize extreme probabilities 
  
  if(sim == 1){ # masked matrix, 1= control units and treated units before treatment and 0 = treated units after treatment
    mask <- simul_adapt(Y, N_t, t0, treat_indices=0, weights = e[,t0])
  }else{
    mask <- stag_adapt(Y, N_t, t0, treat_indices=0, weights = e[,t0])
  }
  
  Y_obs <- Y * mask * missing.mat

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
  
  ## Estimate propensity scores
  
  if(estimator%in%c("mc_weights")){
    
    p.mod <- cv.glmnet(x=cbind(covars.x,Y_obs[,1:(t0-1)]), y=(1-mask*missing.mat), family="mgaussian", alpha=1,nfolds=10,intercept=FALSE)
    W <- predict(p.mod, cbind(covars.x,Y_obs[,1:(t0-1)]))[,,1]
    
    p.weights <- matrix(NA, nrow=nrow(mask), ncol=ncol(mask), dimnames = list(rownames(mask), colnames(mask)))
    p.weights <- boundProbs(W)/(1-boundProbs(W))
  }

  ## ------ ------ ------ ------ ------
  ## MC-NNM plain (no weighting, no covariate)
  ## ------ ------ ------ ------ ------
  
  est_mc_plain <- list()
  if(estimator=="mc_plain"){
    est_mc_plain <- mcnnm_cv(M = Y_obs, mask = mask, W = matrix(1, nrow(mask),ncol(mask)), to_estimate_u = 1, to_estimate_v = 1, rel_tol = 1e-05, is_quiet = 1)
    
    est_mc_plain$rankL <- rankMatrix(t(est_mc_plain$L), method="qr.R")[1]
    
    est_mc_plain$Mhat <- est_mc_plain$L + replicate(T,est_mc_plain$u) + t(replicate(N,est_mc_plain$v))
    est_mc_plain$tau <- (Y-est_mc_plain$Mhat) # estimated treatment effect, Y(ST) - Y(NT)
    est_mc_plain$err <- (est_mc_plain$tau - 0) # error (wrt to ground truth)
    
    est_mc_plain$msk_err <- est_mc_plain$err*(1-mask) # masked error (wrt to ground truth)
    est_mc_plain$RMSE <- sqrt((1/sum(1-mask)) * sum(est_mc_plain$msk_err^2)) # RMSE on test set (wrt to ground truth)
    print(paste("MC-NNM (Plain) RMSE:", round(est_mc_plain$RMSE,3)))
    
    est_mc_plain$att <- apply(est_mc_plain$tau*(1-mask),1,nzmean)[ST]
    est_mc_plain$att.bar <- mean(est_mc_plain$att)
    est_mc_plain$abs.bias <- abs(est_mc_plain$att.bar-att.true)
    print(paste("MC-NNM (Plain) abs. bias:", round(est_mc_plain$abs.bias,3)))
    
    # bootstrap variance estimation
    df_mc_plain <- widetoLong(Y= Y, mask = mask, X = NULL)
    df_mc_plain$person_id <- as.numeric(df_mc_plain$person_id)
    est_mc_plain$boot_var <- clustered_bootstrap(current_data_realized_long=df_mc_plain, estimator="mc_plain", N=nrow(Y), T=ncol(Y), T0=t0, B = 399, est_weights = FALSE, ncores = cores)
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
    est_mc_weights <- mcnnm_cv(M = Y_obs, mask = mask, W = p.weights, to_estimate_u = 1, to_estimate_v = 1, rel_tol = 1e-05, is_quiet = 1)
    
    est_mc_weights$rankL <- rankMatrix(t(est_mc_weights$L), method="qr.R")[1]
    
    est_mc_weights$Mhat <- est_mc_weights$L + replicate(T,est_mc_weights$u) + t(replicate(N,est_mc_weights$v))
    est_mc_weights$tau <- (Y-est_mc_weights$Mhat) # estimated treatment effect, Y(AT) - Y(ST)
    est_mc_weights$err <- (est_mc_weights$tau - 0) # error (wrt to ground truth)
    
    est_mc_weights$msk_err <- est_mc_weights$err*(1-mask) # masked error (wrt to ground truth)
    est_mc_weights$RMSE <- sqrt((1/sum(1-mask)) * sum(est_mc_weights$msk_err^2)) # RMSE on test set (wrt to ground truth)
    print(paste("MC-NNM (weights) RMSE:", round(est_mc_weights$RMSE,3)))
    
    est_mc_weights$att <- apply(est_mc_weights$tau*(1-mask),1,nzmean)[ST]
    est_mc_weights$att.bar <- mean(est_mc_weights$att)
    est_mc_weights$abs.bias <- abs(est_mc_weights$att.bar-att.true)
    print(paste("MC-NNM (weights) abs. bias:", round(est_mc_weights$abs.bias,3)))
    
    # bootstrap variance estimation
    df_mc_weights <- widetoLong(Y= Y, mask = mask, X = covars.x)
    df_mc_weights$person_id <- as.numeric(df_mc_weights$person_id)
    est_mc_weights$boot_var <- clustered_bootstrap(current_data_realized_long=df_mc_weights, estimator="mc_weights", N=nrow(Y), T=ncol(Y), T0=t0, B = 399, est_weights = TRUE, ncores = cores)
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
    est_model_ADH$Mhat <- adh_mp_rows(Y_obs, mask)
    est_model_ADH$tau <- (Y-est_model_ADH$Mhat) # estimated treatment effect
    est_model_ADH$err <- (est_model_ADH$tau - 0) # error (wrt to ground truth)
    
    est_model_ADH$msk_err <- est_model_ADH$err*(1-mask) # masked error (wrt to ground truth)
    est_model_ADH$RMSE <- sqrt((1/sum(1-mask)) * sum(est_model_ADH$msk_err^2)) # RMSE on test set (wrt to ground truth)
    print(paste("ADH RMSE:", round(est_model_ADH$RMSE,3)))
    
    est_model_ADH$att <- apply(est_model_ADH$tau*(1-mask),1,nzmean)[ST]
    est_model_ADH$att.bar <- mean(est_model_ADH$att)
    est_model_ADH$abs.bias <- abs(est_model_ADH$att.bar-att.true)
    print(paste("ADH abs. bias:", round(est_model_ADH$abs.bias,3)))
    
    # bootstrap variance estimation
    df_ADH <- widetoLong(Y= Y, mask = mask, X = NULL)
    df_ADH$person_id <- as.numeric(df_ADH$person_id)
    est_model_ADH$boot_var <- clustered_bootstrap(current_data_realized_long=df_ADH, estimator="ADH", N=nrow(Y), T=ncol(Y), T0=t0, B = 399, est_weights = FALSE, ncores = cores)
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
    est_model_ENT$Mhat <- t(en_mp_rows(t(Y_obs), t(mask), num_alpha = 1))
    est_model_ENT$tau <- (Y-est_model_ENT$Mhat) # estimated treatment effect
    est_model_ENT$err <- (est_model_ENT$tau - 0) # error (wrt to ground truth)
    
    est_model_ENT$msk_err <- est_model_ENT$err*(1-mask) # masked error (wrt to ground truth)
    est_model_ENT$RMSE <- sqrt((1/sum(1-mask)) * sum(est_model_ENT$msk_err^2)) # RMSE on test set (wrt to ground truth)
    print(paste("VT-EN RMSE:", round(est_model_ENT$RMSE,3)))
    
    est_model_ENT$att <- apply(est_model_ENT$tau*(1-mask),1,nzmean)[ST]
    est_model_ENT$att.bar <- mean(est_model_ENT$att)
    est_model_ENT$abs.bias <- abs(est_model_ENT$att.bar-att.true)
    print(paste("VT-EN abs. bias:", round(est_model_ENT$abs.bias,3)))
    
    # bootstrap variance estimation
    df_ENT <- widetoLong(Y= Y, mask = mask, X = NULL)
    df_ENT$person_id <- as.numeric(df_ENT$person_id)
    est_model_ENT$boot_var <- clustered_bootstrap(current_data_realized_long=df_ENT, estimator="ENT", N=nrow(Y), T=ncol(Y), T0=t0, B = 399, est_weights = FALSE, ncores = cores)
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
    est_model_DID$Mhat <- DID(Y_obs, mask)
    est_model_DID$tau <- (Y-est_model_DID$Mhat) # estimated treatment effect
    est_model_DID$err <- (est_model_DID$tau  - 0) # error (wrt to ground truth)
    
    est_model_DID$msk_err <- est_model_DID$err*(1-mask) # masked error (wrt to ground truth)
    est_model_DID$RMSE <- sqrt((1/sum(1-mask)) * sum(est_model_DID$msk_err^2)) # RMSE on test set (wrt to ground truth)
    print(paste("DID RMSE:", round(est_model_DID$RMSE,3)))
    
    est_model_DID$att <- apply(est_model_DID$tau*(1-mask),1,nzmean)[ST]
    est_model_DID$att.bar <- mean(est_model_DID$att)
    est_model_DID$abs.bias <- abs(est_model_DID$att.bar-att.true)
    print(paste("DID abs. bias:", round(est_model_DID$abs.bias,3)))
    
    # bootstrap variance estimation
    df_DID <- widetoLong(Y= Y, mask = mask, X = NULL)
    df_DID$person_id <- as.numeric(df_DID$person_id)
    est_model_DID$boot_var <- clustered_bootstrap(current_data_realized_long=df_DID, estimator="DID", N=nrow(Y), T=ncol(Y), T0=t0, B = 399, est_weights = FALSE, ncores = cores)
    print(paste("DID variance:", round(est_model_DID$boot_var,3)))
    
    est_model_DID$cp <- CI_test(est_coefficent=est_model_DID$att.bar, real_coefficent=att.true, est_var=est_model_DID$boot_var)
    print(paste("DID CP:", round(est_model_DID$cp,3)))
    
    est_model_DID$CI_width <- abs(boot_CI(est_coefficent =est_model_DID$att.bar, est_var=est_model_DID$boot_var)$lb-boot_CI(est_coefficent =est_model_DID$att.bar, est_var=est_model_DID$boot_var)$ub)
    print(paste("DID CI width:", round(est_model_DID$CI_width,3)))
  }
  
  ## ---------------
  ## IFEs
  ## ---------------
  
  est_model_IFE <- list()
  if(estimator=="IFE"){
    est_model_IFE$Mhat <- IFE(Y_obs, mask, k=2)
    est_model_IFE$tau <- (Y-est_model_IFE$Mhat) # estimated treatment effect
    est_model_IFE$err <- (est_model_IFE$tau - 0) # error (wrt to ground truth)
    
    est_model_IFE$msk_err <- est_model_IFE$err*(1-mask) # masked error (wrt to ground truth)
    est_model_IFE$RMSE <- sqrt((1/sum(1-mask)) * sum(est_model_IFE$msk_err^2)) # RMSE on test set (wrt to ground truth)
    print(paste("IFE RMSE:", round(est_model_IFE$RMSE,3)))
    
    est_model_IFE$att <- apply(est_model_IFE$tau*(1-mask),1,nzmean)[ST]
    est_model_IFE$att.bar <- mean(est_model_IFE$att)
    est_model_IFE$abs.bias <- abs(est_model_IFE$att.bar-att.true)
    print(paste("IFE abs. bias:", round(est_model_IFE$abs.bias,3)))
    
    # bootstrap variance estimation
    df_IFE <- widetoLong(Y= Y, mask = mask, X = NULL)
    df_IFE$person_id <- as.numeric(df_IFE$person_id)
    est_model_IFE$boot_var <- clustered_bootstrap(current_data_realized_long=df_IFE, estimator="IFE", N=nrow(Y), T=ncol(Y), T0=t0, B = 399, est_weights = FALSE, ncores = cores)
    print(paste("IFE variance:", round(est_model_IFE$boot_var,3)))
    
    est_model_IFE$cp <- CI_test(est_coefficent=est_model_IFE$att.bar, real_coefficent=att.true, est_var=est_model_IFE$boot_var)
    print(paste("IFE CP:", round(est_model_IFE$cp,3)))
    
    est_model_IFE$CI_width <- abs(boot_CI(est_coefficent =est_model_IFE$att.bar, est_var=est_model_IFE$boot_var)$lb-boot_CI(est_coefficent =est_model_IFE$att.bar, est_var=est_model_IFE$boot_var)$ub)
    print(paste("IFE CI width:", round(est_model_IFE$CI_width,3)))
  }
  
  # cleanup
  cat(paste("Done with simulation run number",n, "\n"))
  return(list("N"=N, "T"=T, "T0"=t0, "N_t"=N_t,"estimator"=estimator,"fr_obs"= fr_obs,
              "est_mc_plain_RMSE"=est_mc_plain$RMSE,"est_mc_plain_abs_bias"=est_mc_plain$abs.bias,"est_mc_plain_cp"=est_mc_plain$cp,"est_mc_plain_boot_var"=est_mc_plain$boot_var,"est_mc_plain_CI_width"=est_mc_plain$CI_width,"est_mc_plain_rankL"=est_mc_plain$rankL, 
              "est_mc_weights_RMSE"=est_mc_weights$RMSE,"est_mc_weights_abs_bias"=est_mc_weights$abs.bias,"est_mc_weights_cp"=est_mc_weights$cp,"est_mc_weights_boot_var"=est_mc_weights$boot_var,"est_mc_weights_CI_width"=est_mc_weights$CI_width,"est_mc_weights_rankL"=est_mc_weights$rankL,
              "est_model_ADH_RMSE"=est_model_ADH$RMSE,"est_model_ADH_abs_bias"=est_model_ADH$abs.bias,"est_model_ADH_cp"=est_model_ADH$cp,"est_model_ADH_boot_var"=est_model_ADH$boot_var,"est_model_ADH_width"=est_model_ADH$CI_width,
              "est_model_ENT_RMSE"=est_model_ENT$RMSE,"est_model_ENT_abs_bias"=est_model_ENT$abs.bias,"est_model_ENT_cp"=est_model_ENT$cp,"est_model_ENT_boot_var"=est_model_ENT$boot_var,"est_model_ENT_width"=est_model_ENT$CI_width,
              "est_model_DID_RMSE"=est_model_DID$RMSE,"est_model_DID_abs_bias"=est_model_DID$abs.bias,"est_model_DID_cp"=est_model_DID$cp,"est_model_DID_boot_var"=est_model_DID$boot_var,"est_model_DID_width"=est_model_DID$CI_width,
              "est_model_IFE_RMSE"=est_model_IFE$RMSE,"est_model_IFE_abs_bias"=est_model_IFE$abs.bias,"est_model_IFE_cp"=est_model_IFE$cp,"est_model_IFE_boot_var"=est_model_IFE$boot_var,"est_model_IFE_width"=est_model_IFE$CI_width))
}

# Load data
capacity.outcomes.none <- readRDS("data/capacity-outcomes-none.rds")
capacity.outcomes.mice <- readRDS("data/capacity-outcomes-mice-cart.rds") 
capacity.outcomes.linear <- readRDS("data/capacity-outcomes-linear.rds") # for covariates

# define settings for simulation
settings <- expand.grid("d"=c('rev.pc','exp.pc'),
                        "T0"= seq(1:5),  
                        "estimator"=c("mc_plain","mc_weights","ADH","ENT","DID","IFE"))

args <- commandArgs(trailingOnly = TRUE) # command line arguments
thisrun <- settings[as.numeric(args[1]),] 

d <- as.character(thisrun[1]$d)
T0 <- as.numeric(thisrun[2]$T0)
estimator <- as.character(thisrun[3]$estimator)

if(doMPI){
  cores <- parallel::detectCores()
  print(paste0("number of cores used: ", cores))
}

n.runs <- 1000 # Num. simulation runs

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

results <- foreach(i = 1:n.runs, .combine='cbind', .packages =c("MCPanel","matrixStats","Matrix","MASS","data.table","reshape","reshape2","emfactor"), .verbose = FALSE) %dopar% {
  
  # Transform covars to unit and time-specific inputs
  capacity.covars <- cbind(capacity.outcomes.linear[[d]]$faval[,c("1850","1860")][sort(rownames(capacity.outcomes.linear[[d]]$faval[,c("1850","1860")])),], 
                           capacity.outcomes.linear[[d]]$farmsize[,c("1860")][sort(names(capacity.outcomes.linear[[d]]$farmsize[,c("1860")]))],
                           c("AZ"=0, "NM"=0, capacity.outcomes.linear[[d]]$access[,c("1860")])[sort(c(names(capacity.outcomes.linear[[d]]$access[,c("1860")]),"AZ","NM"))]) # AZ and NM not in dataset
  
  colnames(capacity.covars) <- c("faval.1850","faval.1860","farmsize.1860", "access.1860")
  
  capacity.covars <-capacity.covars[match(rownames(capacity.outcomes.mice[[d]]$M), rownames(capacity.covars)), ] # same order
  capacity.covars[is.na(capacity.covars)] <- 0
  
  pub.states <- c("AK","AL","AR","AZ","CA","CO","FL","IA","ID","IL","IN","KS","LA","MI","MN","MO","MS","MT","ND","NE","NM","NV","OH","OK","OR","SD","UT","WA","WI","WY") # 30 public land states
  treat_indices_order <- row.names(capacity.outcomes.mice[[d]]$M)[row.names(capacity.outcomes.mice[[d]]$M)%in% pub.states]
  
  CapacitySim(outcomes.missing=capacity.outcomes.none,outcomes.imputed=capacity.outcomes.mice,covars.x=capacity.covars,d,T0,sim=0,treated.indices=treat_indices_order,estimator,cores,n=i)
}
results
saveRDS(results, paste0(output_dir,"mc_capacity_sim_results_","data_",d,"_T0_",T0,"_estimator_",estimator,"_n_",n.runs,".rds"))

if(doMPI){
  closeCluster(cl) # close down MPIcluster
  mpi.finalize()
}else{
  stopCluster(cl)
}