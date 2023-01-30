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

CapacitySim <- function(outcomes.imputed,covars.x,d,T0,sim,treated.indices,estimator=c("mc_plain","mc_weights","ADH","ENT","DID","IFE"),cores,n){
  
  # set the seed
  print(paste0("run number: ", n))
  set.seed(n, "L'Ecuyer-CMRG")
  
  # specify outcome and treatment matrix
  Y <- outcomes.imputed[[d]]$M # NxT  # imputed outcomes
  missing.mat <- outcomes.imputed[[d]]$M.missing
  missing.mat[is.na(missing.mat)] <- 0 # 0s are missing/imputed
  treat <- outcomes.imputed[[d]]$mask # NxT masked matrix 
  
  ## working with control units
  treat <- treat[!rownames(treat)%in%treated.indices,] 
  missing.mat <- missing.mat[!rownames(missing.mat)%in%treated.indices,]
  Y <- Y[!rownames(Y)%in%treated.indices,] 
  covars.x <- covars.x[!rownames(covars.x)%in%treated.indices,]
  
  ## Setting up the configuration
  N <- nrow(treat)
  T <- ncol(treat)
  number_T0 <- 4
  t0 <- ceiling(T*((1:number_T0)*2-1)/(2*number_T0))[T0]
  N_t <- ceiling(N*0.5) # no. treated units desired <=N
  
  att.true <- 0

  ## Simultaneuous (simul_adapt) or Staggered adoption (stag_adapt)
  
  e <-plogis(scale(cbind(Y[,1:(t0-1)],replicate((T-t0+1),Y[,(t0-1)])) + matrix(rowSums(covars.x),N,T))) # prob of being missing (treated/missing)
  e <- boundProbs(e) # winsorize extreme probabilities 
  
  if(sim == 1){ # masked matrix, 1= control units and treated units before treatment and 0 = treated units after treatment
    mask <- simul_adapt(Y, N_t, t0, treat_indices=0, weights = e[,t0])
  }else{
    mask <- stag_adapt(Y, N_t, t0, treat_indices=0, weights = e[,t0])
  }
  
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

  # bootstrap variance estimation
  bopt <- max(b.star(t(Y),round=TRUE))   # get optimal stationary bootstrap lengths
  boot.att.bar <- tsboot(tseries=t(Y), MCEst, outcomes=outcomes, covars.x=covars.x, t0=t0, ST=ST, estimator=estimator, estimand="att.bar",
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
  
  # cleanup
  cat(paste("Done with simulation run number",n, "\n"))
  return(list("N"=N, "T"=T, "T0"=t0, "N_t"=N_t,"estimator"=estimator, "data"= d, "fr_obs"= fr_obs,
              "bopt"=bopt, "boot.att.bar"=boot.att.bar, "boot_var"=boot_var,"cp"=cp, "abs_bias"=abs_bias, "CI_width"=CI_width))
}

# Load data
capacity.outcomes.mice <- readRDS("data/capacity-outcomes-mice-pmm.rds") 
capacity.outcomes.linear <- readRDS("data/capacity-outcomes-linear.rds") # for covariates

# define settings for simulation
settings <- expand.grid("d"=c('rev.pc','exp.pc'),
                        "T0"= seq(1:4),  
                        "estimator"=c("mc_plain","mc_weights","ADH","ENT","DID","IFE"))

args <- commandArgs(trailingOnly = TRUE) # command line arguments
thisrun <- settings[as.numeric(args[1]),] 

d <- as.character(thisrun[1]$d)
T0 <- as.numeric(thisrun[2]$T0)
estimator <- as.character(thisrun[3]$estimator)

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

results <- foreach(i = 1:n.runs, .combine='cbind', .packages =c("MCPanel","matrixStats","Matrix","MASS","data.table","reshape","reshape2","emfactor","boot"), .verbose = FALSE) %dopar% {
  
  # Transform covars to unit and time-specific inputs
  capacity.covars <- cbind(capacity.outcomes.linear[[d]]$faval[,c("1850","1860")][sort(rownames(capacity.outcomes.linear[[d]]$faval[,c("1850","1860")])),], 
                           capacity.outcomes.linear[[d]]$farmsize[,c("1860")][sort(names(capacity.outcomes.linear[[d]]$farmsize[,c("1860")]))],
                           c("AZ"=0, "NM"=0, capacity.outcomes.linear[[d]]$access[,c("1860")])[sort(c(names(capacity.outcomes.linear[[d]]$access[,c("1860")]),"AZ","NM"))]) # AZ and NM not in dataset
  
  colnames(capacity.covars) <- c("faval.1850","faval.1860","farmsize.1860", "access.1860")
  
  capacity.covars <-capacity.covars[match(rownames(capacity.outcomes.mice[[d]]$M), rownames(capacity.covars)), ] # same order
  capacity.covars[is.na(capacity.covars)] <- 0
  
  pub.states <- c("AK","AL","AR","AZ","CA","CO","FL","IA","ID","IL","IN","KS","LA","MI","MN","MO","MS","MT","ND","NE","NM","NV","OH","OK","OR","SD","UT","WA","WI","WY") # 30 public land states
  treat_indices_order <- row.names(capacity.outcomes.mice[[d]]$M)[row.names(capacity.outcomes.mice[[d]]$M)%in% pub.states]
  
  CapacitySim(outcomes.imputed=capacity.outcomes.mice,covars.x=capacity.covars,d,T0,sim=0,treated.indices=treat_indices_order,estimator,cores,n=i)
}
results
saveRDS(results, paste0(output_dir,"mc_capacity_sim_results_","data_",d,"_T0_",T0,"_estimator_",estimator,"_n_",n.runs,".rds"))

if(doMPI){
  closeCluster(cl) # close down MPIcluster
  mpi.finalize()
}else{
  stopCluster(cl)
}