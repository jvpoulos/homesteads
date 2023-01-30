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
library(boot)

source('code/utils.R')
source('code/IFE.R')
source("code/MCEst.R")
source("code/PolitisWhite.R")

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
  
  cores <- parallel::detectCores()
  print(paste0("number of cores used: ", cores))
  
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
    
    e <-plogis(scale(cbind(true_mat[,1:(T0-1)],replicate((T-T0+1),true_mat[,(T0-1)]))+X)) # prob of being missing (treated/missing)
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
  
  # combine data back into list
  outcomes <- list("Y"=shifted_mat,
                   "Y_obs"=obs_mat,
                   "missing.mat"= matrix(1,nrow=nrow(shifted_mat), ncol=ncol(shifted_mat)),
                   "mask"=mask)
  
  if(estimator%in%c("mc_weights")){
    
    ## Estimate propensity scores
    
    p.mod <- cv.glmnet(x=cbind(shifted_X,obs_mat[,1:T0]), y=(1-mask), family="mgaussian", alpha=1,nfolds=10,intercept=FALSE)
    W <- predict(p.mod, cbind(shifted_X,obs_mat[,1:T0]))[,,1]
    
    p.weights <- matrix(NA, nrow=nrow(mask), ncol=ncol(mask), dimnames = list(rownames(mask), colnames(mask)))
    p.weights <- boundProbs(W)/(1-boundProbs(W))
  }

  rankL <- NULL  # only for MC
  rank_error <- NULL
  ## ------ ------ ------ ------ ------
  ## MC-NNM plain (no weighting, no covariate)
  ## ------ ------ ------ ------ ------
  
  if(estimator=="mc_plain"){
    est_mc_plain <- mcnnm_cv(M = obs_mat, mask = mask, W = matrix(1, nrow(mask),ncol(mask)), to_estimate_u = 1, to_estimate_v = 1, rel_tol = 1e-05, is_quiet = 1)
    
    rankL <- rankMatrix(t(est_mc_plain$L), method="qr.R")[1]
    rank_error <- abs(rankL-R)
    print(paste("MC-NNM (Plain) rank error:", round(rank_error,3)))
  }
  
  ## ------ ------ ------ ------ ------
  ## MC-NNM weights (no covariate)
  ## ------ ------ ------ ------ ------
  
  if(estimator=="mc_weights"){
    est_mc_weights <- mcnnm_cv(M = obs_mat, mask = mask, W = p.weights, to_estimate_u = 1, to_estimate_v = 1, rel_tol = 1e-05, is_quiet = 1)
    
    rankL <- rankMatrix(t(est_mc_weights$L), method="qr.R")[1]
    rank_error <- abs(rankL-R)
    print(paste("MC-NNM (weights) rank error:", round(rank_error,3)))
  }
  
  # bootstrap variance estimation
  bopt <- max(b.star(t(shifted_mat),round=TRUE))   # get optimal stationary bootstrap lengths
  boot.att.bar <- tsboot(tseries=t(shifted_mat), MCEst, outcomes=outcomes, covars.x=shifted_X, t0=T0, ST=ST, estimator=estimator, estimand="att.bar",
                         R = 399, parallel = "multicore", l = bopt, sim = "fixed")
  
  # evaluate
  boot_var <- apply(boot.att.bar$t, 2, var)
  print(paste("variance:", round(boot_var,3)))
  
  cp <- as.numeric(boot.ci(boot.att.bar, type="basic")$basic[4] <= att.true &
                     boot.ci(boot.att.bar, type="basic")$basic[5] >= att.true)
  print(paste("CP:", round(cp,3)))
  
  abs_bias <- abs(boot.att.bar$t0-att.true)
  print(paste("abs. bias:", round(abs_bias,3)))
  
  CI_width <- abs(boot.ci(boot.att.bar, type="basic")$basic[5]-boot.ci(boot.att.bar, type="basic")$basic[4])
  print(paste("CI width:", round(CI_width,3)))
  
  return(list("N"=N, "T"=T, "R"=R, "T0"=T0, "N_t"=N_t, "beta_sc"=beta_sc,"loading_sc"=loading_sc, "logi_sc" = logi_sc, "shift_sc"=shift_sc, 
              "estimator"=estimator, "data" = d, "fr_obs"= fr_obs, "att.true" = att.true, "rankL"=rankL, "rank_error"=rank_error,
              "bopt"=bopt, "boot.att.bar"=boot.att.bar, "boot_var"=boot_var,"cp"=cp, "abs_bias"=abs_bias, "CI_width"=CI_width))
}

# define settings for simulation
settings <- expand.grid("NT"=c(60**2),
                        "N_t" =c(0.5),
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

setting <- paste0("N = ", N, ", T = ", T, ", R = ",R, "T0 = " ,T0, "N_t", N_t, ", beta_sc = ", beta_sc, ", shift_sc = ", shift_sc, ", estimator = ", estimator)
tic(print(paste0("setting: ",setting)))

results <- foreach(i = 1:n.runs, .combine='cbind', .packages =c("MCPanel","matrixStats","Matrix","MASS","data.table","reshape","reshape2","emfactor","boot"), .verbose = FALSE) %dopar% {
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