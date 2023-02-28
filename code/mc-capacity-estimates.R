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
library(boot)

source('code/utils.R')
source('code/IFE.R')
source("code/MCEst.R")
source("code/PolitisWhite.R")

# Setup parallel processing 
library(parallel)
library(doParallel)
library(foreach)

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

  cores <- parallel::detectCores()
  print(paste0("number of cores used: ", cores))
  
  cl <- parallel::makeCluster(cores, outfile="")
  
  doParallel::registerDoParallel(cl) # register cluster
}

CapacityEst <- function(outcomes.imputed,covars.x,d,t0,treated.indices,cores,estimator=c("mc_plain","mc_weights","ADH","ENT","DID","IFE"), point.est=FALSE){

  # specify outcome and treatment matrix
  Y <- outcomes.imputed[[d]]$M # NxT  # imputed outcomes
  missing.mat <- outcomes.imputed[[d]]$M.missing
  missing.mat[is.na(missing.mat)] <- 0 # 0s are missing/imputed
  mask <- 1-outcomes.imputed[[d]]$mask#  # masked matrix, 1= control units and treated units before treatment and 0 = treated units after treatment
  
  N <- dim(mask)[1]
  T <- dim(mask)[2]
  
  Y_obs <- Y * mask
  
  fr_obs <- sum(mask)/(N*T) # store fraction observed entries
  print(paste0("fraction observed: ", fr_obs))
  
  # combine data back into list
  outcomes <- list("Y"=Y,
                   "Y_obs"=Y_obs,
                   "missing.mat"= missing.mat,
                   "mask"=mask)
  
  # get vector of initial treatment periods for N_t treated units
  
  A <- aggregate(col ~ row,
                 data = which(mask == 0, arr.ind = TRUE),
                 FUN = function(x) x[1])$col # gives the intial T0s for treated units
  
  ST <- aggregate(col ~ row,
                  data = which(mask == 0, arr.ind = TRUE),
                  FUN = function(x) x[1])$row  # switch treated indices
  NT <- setdiff(1:N, ST) # control indices
  
  if(point.est){ # return point estimates for plots
    mc_est <- MCEst(outcomes, covars.x, t0, ST, estimator, estimand=NULL, tseries=NULL)
    saveRDS(mc_est, paste0(output_dir,"mc_capacity_point_estimates_","data_",d,"_estimator_",estimator,".rds"))
  }
  
  # Get  bootstrap CIs 
  # block resampling with fixed block lengths of length bopt
  
  bopt <- max(b.star(t(Y),round=TRUE))   # get optimal stationary bootstrap lengths

  boot.tau <- tsboot(tseries=t(Y), MCEst, outcomes=outcomes, covars.x=covars.x, t0=t0, ST=ST, estimator=estimator, estimand="tau",
                 R= 999, parallel = "multicore", l =bopt, sim = "fixed") 
  
  boot.att <- tsboot(tseries=t(Y), MCEst, outcomes=outcomes, covars.x=covars.x, t0=t0, ST=ST, estimator=estimator, estimand="att",
                         R= 999, parallel = "multicore", l =bopt, sim = "fixed") 
  
  boot.att.bar <- tsboot(tseries=t(Y), MCEst, outcomes=outcomes, covars.x=covars.x, t0=t0, ST=ST, estimator=estimator, estimand="att.bar",
                     R= 999, parallel = "multicore", l =bopt, sim = "fixed")
  
  boot.att.bar.ci <- boot.ci(boot.att.bar)
  
  return(list("N"=N, "T"=T, "T0"=t0,"data"=d,"estimator"=estimator,"fr_obs"= fr_obs, "boot_tau"=boot.tau, "boot_att"=boot.att, "boot_att_bar"=boot.att.bar, "bopt"=bopt,
              "boot.att.bar.ci"=boot.att.bar.ci))
}

# define settings 
settings <- expand.grid("d"=c('rev.pc','exp.pc'),
                        "estimator"=c("mc_plain","mc_weights","ADH","ENT","DID"))

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

results <- foreach(i = c("mice-cart","mice-pmm","mtsdi"), .combine='cbind', .packages =c("MCPanel","matrixStats","Matrix","MASS","data.table","reshape","reshape2","emfactor","boot","glmnet"), .verbose = TRUE) %dopar% {
  
  # Load data
  outcomes.imputed <- readRDS(paste0("data/capacity-outcomes-", i,".rds")) 
  capacity.outcomes.linear <- readRDS("data/capacity-outcomes-linear.rds") # for covariates
  
  t0 <- which(colnames(outcomes.imputed[[d]]$M)=="1869")
  
  # Transform covars to unit and time-specific inputs
  capacity.covars <- cbind(capacity.outcomes.linear[[d]]$faval[,c("1850","1860")][sort(rownames(capacity.outcomes.linear[[d]]$faval[,c("1850","1860")])),], 
                           capacity.outcomes.linear[[d]]$farmsize[,c("1860")][sort(names(capacity.outcomes.linear[[d]]$farmsize[,c("1860")]))],
                           c("AZ"=0, "NM"=0, capacity.outcomes.linear[[d]]$access[,c("1860")])[sort(c(names(capacity.outcomes.linear[[d]]$access[,c("1860")]),"AZ","NM"))]) # AZ and NM not in dataset
  
  colnames(capacity.covars) <- c("faval.1850","faval.1860","farmsize.1860", "access.1860")
  
  capacity.covars <-capacity.covars[match(rownames(outcomes.imputed[[d]]$M), rownames(capacity.covars)), ] # same order
  capacity.covars[is.na(capacity.covars)] <- 0
  
  pub.states <- c("AK","AL","AR","AZ","CA","CO","FL","IA","ID","IL","IN","KS","LA","MI","MN","MO","MS","MT","ND","NE","NM","NV","OH","OK","OR","SD","UT","WA","WI","WY") # 30 public land states
  treat_indices_order <- row.names(outcomes.imputed[[d]]$M)[row.names(outcomes.imputed[[d]]$M)%in% pub.states]
  
  CapacityEst(outcomes.imputed=outcomes.imputed,covars.x=capacity.covars,d=d,t0=t0,treated.indices=treat_indices_order,cores=cores,estimator=estimator)
}
results
saveRDS(results, paste0(output_dir,"mc_capacity_results_","data_",d,"_estimator_",estimator,".rds"))

if(doMPI){
  closeCluster(cl) # close down MPIcluster
  mpi.finalize()
}else{
  stopCluster(cl)
}
