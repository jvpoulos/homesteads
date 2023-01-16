##########################################
# DD Estimation (county-level measures)   #
##########################################
#N.B.: need to run dd-capacity.R first

library(data.table)
library(dplyr)
library(tidyverse) 
library(boot)

source('code/utils.R')

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

DDInequalityEst <- function(cores=parallel::detectCores()){
  
  ineq.did <- census.ts.state[c("year","state","aland.gini")]
  
  ineq.did <- merge(data.frame(ineq.did), data.frame(homesteads.state), by.x=c("state","year"), by.y=c("state_code","year"), all.x=TRUE)
  
  ## Create mask
  ineq.did$mask <- NA
  ineq.did$mask[ineq.did$homesteads.sum>0] <- 1
  
  ineq.did <- ineq.did %>% 
    group_by(state) %>% 
    fill(mask)  #default direction down
  
  ineq.did$mask[is.na(ineq.did$mask)] <- 0
  
  # homestead NAs are log(0)
  ineq.did$homesteads.pc[is.na(ineq.did$homesteads.pc) | ineq.did$homesteads.pc <=0] <- .Machine$double.eps
  ineq.did$homesteads.pc <- log(ineq.did$homesteads.pc)
  
  ## Farm value
  
  ineq.did <- merge(ineq.did, farmval.state[c("state.abb","year","faval")], by.x=c("state","year"),by.y=c("state.abb","year"),all.x=TRUE)
  
  # # Fill missing covariates
  
  ineq.did[ineq.did$year<=1870,] <- ineq.did[ineq.did$year<=1870,]%>%
    group_by(state) %>%
    fill(faval) %>% #default direction down
    fill(faval, .direction = "up")%>% 
    mutate_each(funs(log(.Machine$double.eps + .)), faval) # take logs
  
  ineq.did[ineq.did$year>1870,] <- ineq.did[ineq.did$year>1870,]%>%
    group_by(state) %>%
    fill(faval) %>% #default direction down
    fill(faval, .direction = "up") %>% 
    mutate_each(funs(log(.Machine$double.eps + .)), faval) # take logs
  
  ## Create interaction term
  
  ineq.did$did <- NA
  ineq.did$did <- ineq.did$mask*ineq.did$homesteads.pc
  
  ineq.f2 <- formula(aland.gini ~ factor(state) + mask + did + faval)
  
  # DD Estimates
  
  did.model <- glm(ineq.f2, data=ineq.did)
  
  did.delta <- coef(did.model)[['did']]
  
  did.ci <- confint(did.model,"did")
  
  did.nobs <- nobs(did.model)
  
  did.ar2 <- summary(lm(ineq.f2, data=ineq.did))$adj.r.squared
  
  # bootstrap variance estimation
  
  did.boot <- boot(data=ineq.did,
                   statistic=RunDiD,
                   f1=ineq.f2,
                   R=999,
                   strata=as.factor(ineq.did$state), # stratify by state
                   parallel="multicore", ncpus = cores)
  
  did.boot.delta <- did.boot$t0
  
  did.boot.CI <- boot.ci(did.boot, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
  
  return(list("model"=did.model,"delta"=did.delta,"CI"=did.ci,"nobs"=did.nobs, "ar2"=didar2,
              "boot"=did.boot, "boot.delta"=did.boot.delta,"did.boot.CI"=did.boot.CI))
}

## Load capacity data
load("results/capacity-state.RData")

results <- DDInequalityEst(cores=cores)
results
saveRDS(results, paste0(output_dir,"dd_inequality_results.rds"))

if(doMPI){
  closeCluster(cl) # close down MPIcluster
  mpi.finalize()
}else{
  stopCluster(cl)
}