###################################
# MC estimates #
###################################

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

source('code/utils.R')
source('code/IFE.R')

# Setup parallel processing 
library(parallel)
library(doParallel)
library(foreach)

# Setup parallel processing
doMPI <- FALSE
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

CapacityEst <- function(outcomes.missing,outcomes.imputed,covars.x,d,T0,sim,treated.indices,cores,estimator=c("mc_plain","mc_weights","ADH","ENT","DID","IFE")){
  
  # specify outcome and treatment matrix
  Y.missing <- outcomes.missing[[d]]$M  # to train on: 0s = missing
  Y <- outcomes.imputed[[d]]$M # NxT  # imputed outcomes
  missing.mat <- outcomes.imputed[[d]]$M.missing
  missing.mat[is.na(missing.mat)] <- 0 # 0s are missing/imputed
  treat <- outcomes.imputed[[d]]$mask# NxT masked matrix: 0 for control units and treated units before treatment and 1 for treated units after treatment
  
  N <- dim(treat)[1]
  T <- dim(treat)[2]
  
  covars.x <- scale(matrix(covars.x[,1],N,T) + matrix(covars.x[,2],N,T) + matrix(covars.x[,3],N,T) +matrix(covars.x[,4],N,T)) # need to combine covars in NxT matrix
  
  treat_indices <- which(rownames(Y) %in%treated.indices) # keep treated fixed to actual treated
  
  mask <- 1-treat # masked matrix, 1= control units and treated units before treatment and 0 = treated units after treatment
  rownames(mask) <- rownames(treat)
  colnames(mask) <- colnames(treat)
  
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

    est_mc_plain$att <- apply(est_mc_plain$tau*(1-mask),1,nzmean)[ST]
    est_mc_plain$att.bar <- mean(est_mc_plain$att)

        # bootstrap variance estimation
    df_mc_plain <- widetoLong(Y= Y, mask = mask, X = NULL)
    df_mc_plain$person_id <- as.numeric(df_mc_plain$person_id)
    est_mc_plain$boot_var <- clustered_bootstrap(current_data_realized_long=df_mc_plain, estimator="mc_plain", N=nrow(Y), T=ncol(Y), T0=t0, B = 999, est_weights = FALSE, ncores = cores)
    print(paste("MC-NNM (Plain) variance:", round(est_mc_plain$boot_var,3)))
    
    est_mc_plain$CI <- boot_CI(est_coefficent=est_mc_plain$att.bar, est_var=est_mc_plain$boot_var)
    print(paste("MC-NNM (Plain) CI lower:", round(est_mc_plain$CI$lb,3)))
    print(paste("MC-NNM (Plain) CI upper:", round(est_mc_plain$CI$ub,3)))
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
    
    est_mc_weights$att <- apply(est_mc_weights$tau*(1-mask),1,nzmean)[ST]
    est_mc_weights$att.bar <- mean(est_mc_weights$att)
    
    # bootstrap variance estimation
    df_mc_weights <- widetoLong(Y= Y, mask = mask, X = covars.x)
    df_mc_weights$person_id <- as.numeric(df_mc_weights$person_id)
    est_mc_weights$boot_var <- clustered_bootstrap(current_data_realized_long=df_mc_weights, estimator="mc_weights", N=nrow(Y), T=ncol(Y), T0=t0, B = 999, est_weights = TRUE, ncores = cores)
    print(paste("MC-NNM (weights) variance:", round(est_mc_weights$boot_var,3)))
    
    est_mc_weights$CI <- boot_CI(est_coefficent=est_mc_weights$att.bar, est_var=est_mc_weights$boot_var)
    print(paste("MC-NNM (weights) CI lower:", round(est_mc_weights$CI$lb,3)))
    print(paste("MC-NNM (weights) CI upper:", round(est_mc_weights$CI$ub,3)))
  }
  
  ## -----
  ## ADH
  ## -----
  est_model_ADH <- list()
  if(estimator=="ADH"){
    est_model_ADH$Mhat <- adh_mp_rows(Y_obs, mask)
    est_model_ADH$tau <- (Y-est_model_ADH$Mhat) # estimated treatment effect

    est_model_ADH$att <- apply(est_model_ADH$tau*(1-mask),1,nzmean)[ST]
    est_model_ADH$att.bar <- mean(est_model_ADH$att)
    
    # bootstrap variance estimation
    df_ADH <- widetoLong(Y= Y, mask = mask, X = NULL)
    df_ADH$person_id <- as.numeric(df_ADH$person_id)
    est_model_ADH$boot_var <- clustered_bootstrap(current_data_realized_long=df_ADH, estimator="ADH", N=nrow(Y), T=ncol(Y), T0=t0, B = 999, est_weights = FALSE, ncores = cores)
    print(paste("ADH variance:", round(est_model_ADH$boot_var,3)))
    
    est_model_ADH$CI <- boot_CI(est_coefficent=est_model_ADH$att.bar, est_var=est_model_ADH$boot_var)
    print(paste("ADH CI lower:", round(est_model_ADH$CI$lb,3)))
    print(paste("ADH CI upper:", round(est_model_ADH$CI$ub,3)))
  }
  
  ## -----
  ## VT-EN : It does Not cross validate on alpha (only on lambda) and keep alpha = 1 (LASSO).
  ## -----
  
  est_model_ENT <- list()
  if(estimator=="ENT"){
    est_model_ENT$Mhat <- t(en_mp_rows(t(Y_obs), t(mask), num_alpha = 1))
    est_model_ENT$tau <- (Y-est_model_ENT$Mhat) # estimated treatment effect
    
    est_model_ENT$att <- apply(est_model_ENT$tau*(1-mask),1,nzmean)[ST]
    est_model_ENT$att.bar <- mean(est_model_ENT$att)
    
    # bootstrap variance estimation
    df_ENT <- widetoLong(Y= Y, mask = mask, X = NULL)
    df_ENT$person_id <- as.numeric(df_ENT$person_id)
    est_model_ENT$boot_var <- clustered_bootstrap(current_data_realized_long=df_ENT, estimator="ENT", N=nrow(Y), T=ncol(Y), T0=t0, B = 999, est_weights = FALSE, ncores = cores)
    print(paste("VT-EN variance:", round(est_model_ENT$boot_var,3)))
    
    est_model_ENT$CI <- boot_CI(est_coefficent=est_model_ENT$att.bar, est_var=est_model_ENT$boot_var)
    print(paste("ENT CI lower:", round(est_model_ENT$CI$lb,3)))
    print(paste("ENT CI upper:", round(est_model_ENT$CI$ub,3)))
  }
  
  ## -----
  ## DID
  ## -----
  est_model_DID <- list()
  if(estimator=="DID"){
    est_model_DID$Mhat <- DID(Y_obs, mask)
    est_model_DID$tau <- (Y-est_model_DID$Mhat) # estimated treatment effect

    est_model_DID$att <- apply(est_model_DID$tau*(1-mask),1,nzmean)[ST]
    est_model_DID$att.bar <- mean(est_model_DID$att)
    
    # bootstrap variance estimation
    df_DID <- widetoLong(Y= Y, mask = mask, X = NULL)
    df_DID$person_id <- as.numeric(df_DID$person_id)
    est_model_DID$boot_var <- clustered_bootstrap(current_data_realized_long=df_DID, estimator="DID", N=nrow(Y), T=ncol(Y), T0=t0, B = 999, est_weights = FALSE, ncores = cores)
    print(paste("DID variance:", round(est_model_DID$boot_var,3)))
    
    est_model_DID$CI <- boot_CI(est_coefficent=est_model_DID$att.bar, est_var=est_model_DID$boot_var)
    print(paste("DID CI lower:", round(est_model_DID$CI$lb,3)))
    print(paste("DID CI upper:", round(est_model_DID$CI$ub,3)))
  }
  
  ## ---------------
  ## IFEs
  ## ---------------
  
  est_model_IFE <- list()
  if(estimator=="IFE"){
    est_model_IFE$Mhat <- IFE(Y_obs, mask, k=2)
    est_model_IFE$tau <- (Y-est_model_IFE$Mhat) # estimated treatment effect

    est_model_IFE$att <- apply(est_model_IFE$tau*(1-mask),1,nzmean)[ST]
    est_model_IFE$att.bar <- mean(est_model_IFE$att)
    
    # bootstrap variance estimation
    df_IFE <- widetoLong(Y= Y, mask = mask, X = NULL)
    df_IFE$person_id <- as.numeric(df_IFE$person_id)
    est_model_IFE$boot_var <- clustered_bootstrap(current_data_realized_long=df_IFE, estimator="IFE", N=nrow(Y), T=ncol(Y), T0=t0, B = 999, est_weights = FALSE, ncores = cores)
    print(paste("IFE variance:", round(est_model_IFE$boot_var,3)))
    
    est_model_IFE$CI <- boot_CI(est_coefficent=est_model_IFE$att.bar, est_var=est_model_IFE$boot_var)
    print(paste("IFE CI lower:", round(est_model_IFE$CI$lb,3)))
    print(paste("IFE CI upper:", round(est_model_IFE$CI$ub,3)))
  }
  
  # cleanup
  return(list("N"=N, "T"=T, "T0"=t0,"estimator"=estimator,"fr_obs"= fr_obs,
              "est_mc_plain"=est_mc_plain, 
              "est_mc_weights"=est_mc_weights,
              "est_model_ADH"=est_model_ADH,
              "est_model_ENT"=est_model_ENT,
              "est_model_DID"=est_model_DID,
              "est_model_IFE"=est_model_IFE))
}

# define settings 
settings <- expand.grid("d"=c('rev.pc','exp.pc'),
                        "estimator"=c("mc_plain","mc_weights","ADH","ENT","DID","IFE"))

args <- commandArgs(trailingOnly = TRUE) # command line arguments
thisrun <- settings[as.numeric(args[1]),] 

d <- as.character(thisrun[1]$d)
estimator <- as.character(thisrun[2]$estimator)

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

results <- foreach(imp_method %in% c("mice-cart","mice-pmm","mtsdi"), .combine='cbind', .packages =c("MCPanel","matrixStats","Matrix","MASS","data.table","reshape","reshape2","emfactor"), .verbose = FALSE) %dopar% {
  
  # Load data
  outcomes.missing <- readRDS("data/capacity-outcomes-none.rds")
  outcomes.imputed <- readRDS(paste0("data/capacity-outcomes-", imp_method,".rds")) 
  capacity.outcomes.linear <- readRDS("data/capacity-outcomes-linear.rds") # for covariates
  
  t0 <- which(colnames(outcomes.missing[[d]]$M)=="1869")
  
  # Transform covars to unit and time-specific inputs
  capacity.covars <- cbind(capacity.outcomes.linear[[d]]$faval[,c("1850","1860")][sort(rownames(capacity.outcomes.linear[[d]]$faval[,c("1850","1860")])),], 
                           capacity.outcomes.linear[[d]]$farmsize[,c("1860")][sort(names(capacity.outcomes.linear[[d]]$farmsize[,c("1860")]))],
                           c("AZ"=0, "NM"=0, capacity.outcomes.linear[[d]]$access[,c("1860")])[sort(c(names(capacity.outcomes.linear[[d]]$access[,c("1860")]),"AZ","NM"))]) # AZ and NM not in dataset
  
  colnames(capacity.covars) <- c("faval.1850","faval.1860","farmsize.1860", "access.1860")
  
  capacity.covars <-capacity.covars[match(rownames(outcomes.imputed[[d]]$M), rownames(capacity.covars)), ] # same order
  capacity.covars[is.na(capacity.covars)] <- 0
  
  pub.states <- c("AK","AL","AR","AZ","CA","CO","FL","IA","ID","IL","IN","KS","LA","MI","MN","MO","MS","MT","ND","NE","NM","NV","OH","OK","OR","SD","UT","WA","WI","WY") # 30 public land states
  treat_indices_order <- row.names(outcomes.imputed[[d]]$M)[row.names(outcomes.imputed[[d]]$M)%in% pub.states]
  
  CapacityEst(outcomes.missing=outcomes.missing,outcomes.imputed=outcomes.imputed,covars.x=capacity.covars,d=d,T0=t0,treated.indices=treat_indices_order,cores=parallel::detectCores(),estimator=estimator)
}
results
saveRDS(results, paste0(output_dir,"mc_capacity_results_","data_",d,"_T0_",T0,"_estimator_",estimator,".rds"))

if(doMPI){
  closeCluster(cl) # close down MPIcluster
  mpi.finalize()
}else{
  stopCluster(cl)
}
