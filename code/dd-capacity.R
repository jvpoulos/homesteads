###################################
# DD Estimation (state-level measures)   #
###################################
# Setup parallel processing 
library(parallel)
library(doParallel)
library(foreach)
library(boot)

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

DDCapacityEst <- function(imp_method, d, cores=parallel::detectCores()){
  
  library(data.table)
  library(dplyr)
  library(tidyverse) 
  library(boot)
  
  source('code/utils.R')
  
  capacity.outcomes <- readRDS(paste0("data/capacity-outcomes-", imp_method,".rds"))
  
  capacity.outcomes.M <- list("rev.pc"=capacity.outcomes[["rev.pc"]]$M,"exp.pc"=capacity.outcomes[["exp.pc"]]$M)
  capacity.outcomes.mask <- list("rev.pc"=capacity.outcomes[["rev.pc"]]$mask,"exp.pc"=capacity.outcomes[["exp.pc"]]$mask)
  
  capacity.outcomes.panel <- lapply(capacity.outcomes.M, melt, value.name="outcome")
  capacity.outcomes.mask <- lapply(capacity.outcomes.mask, melt, value.name="mask")
  
  capacity.outcomes.panel <- lapply(capacity.outcomes.panel, function(x){
    colnames(x) <- c("state","year","outcome")
    return(x)
  })
  capacity.outcomes.mask <- lapply(capacity.outcomes.mask, function(x){
    colnames(x) <- c("state","year","mask")
    return(x)
  })
  
  capacity.outcomes.panel$rev.pc <- merge(x=capacity.outcomes.panel$rev.pc, y=capacity.outcomes.mask$rev.pc, by=c("state","year"), all.x=TRUE)
  capacity.outcomes.panel$exp.pc <- merge(x=capacity.outcomes.panel$exp.pc, y=capacity.outcomes.mask$exp.pc, by=c("state","year"), all.x=TRUE)
  
  ## Per-capita total number of patents issued under the HSA 
  
  homesteads.state <- patents.sum %>%
    group_by(state_code,year) %>% #group counties by state and year
    dplyr::mutate(homesteads.sum=sum(homesteads)) %>%
    distinct(state_code,year,.keep_all = TRUE) %>%
    arrange(state_code,year) %>%
    select(state_code,year, homesteads.sum, ns.pop)
  
  homesteads.state <- homesteads.state  %>%
    group_by(state_code) %>%
    dplyr::mutate(homesteads.pc = homesteads.sum/ns.pop) %>%
    select(state_code,year, homesteads.pc, homesteads.sum) %>%
    arrange(state_code,year)
  
  capacity.outcomes.panel <- lapply(capacity.outcomes.panel, merge, y=homesteads.state[c("state_code","year","homesteads.pc")], by.x=c("state","year"),by.y=c("state_code","year"), all.x=TRUE)
  
  # NAs are 0
  capacity.outcomes.panel$rev.pc$homesteads.pc[is.na(capacity.outcomes.panel$rev.pc$homesteads.pc)] <- 0
  capacity.outcomes.panel$exp.pc$homesteads.pc[is.na(capacity.outcomes.panel$exp.pc$homesteads.pc)] <- 0
  
  ## RR access (incl. post-treatment access)
  
  rr.inter.m.state.dd <- rr.inter.m %>% 
    filter(!is.na(state)) %>% # rm DC & territories
    arrange(state,InOpBy)  %>% # sort by state/year
    group_by(state) %>% 
    mutate(cumulative.track = cumsum(track), # cumulative sum of track miles
           track2 = (cumulative.track/AREA_SQMI)) %>% # cumulative track miles per square mile
    dplyr::select(InOpBy,track2,state)
  
  rr.inter.m.state.dd <- rr.inter.m.state.dd[!duplicated(rr.inter.m.state.dd[c("InOpBy","state")]),] # keep one state-year obs
  
  rr.inter.m.state.dd <- as.data.frame(rr.inter.m.state.dd)
  
  rr.inter.m.state.dd$year <- rr.inter.m.state.dd$InOpBy
  
  capacity.outcomes.panel <- lapply(capacity.outcomes.panel, merge, y=rr.inter.m.state.dd[c("state","year","track2")], by=c("state","year"),all.x=TRUE)
  
  ## Farm value
  
  capacity.outcomes.panel <- lapply(capacity.outcomes.panel, merge, y=farmval.state[c("state.abb","year","faval")], by.x=c("state","year"),by.y=c("state.abb","year"),all.x=TRUE)
  
  # Farmsize
  
  capacity.outcomes.panel <- lapply(capacity.outcomes.panel, merge, y=census.ts.state[c("state","year","farmsize")], by=c("state","year"),all.x=TRUE)
  
  # Fill missing covars
  
  capacity.outcomes.panel$rev.pc[capacity.outcomes.panel$rev.pc$year<=1868,] <- capacity.outcomes.panel$rev.pc[capacity.outcomes.panel$rev.pc$year<=1868,]%>% 
    group_by(state) %>% 
    fill(track2, faval, farmsize) %>% #default direction down
    fill(track2, faval, farmsize, .direction = "up") %>% 
    mutate_each(funs(log(.Machine$double.eps + .)), homesteads.pc,faval,farmsize) # take logs
  
  capacity.outcomes.panel$rev.pc[capacity.outcomes.panel$rev.pc$year>1868,] <- capacity.outcomes.panel$rev.pc[capacity.outcomes.panel$rev.pc$year>1868,]%>% 
    group_by(state) %>% 
    fill(track2, faval, farmsize) %>% #default direction down
    fill(track2, faval, farmsize, .direction = "up") %>% 
    mutate_each(funs(log(.Machine$double.eps + .)), homesteads.pc,faval,farmsize) # take logs
  
  capacity.outcomes.panel$exp.pc[capacity.outcomes.panel$exp.pc$year<=1868,] <- capacity.outcomes.panel$exp.pc[capacity.outcomes.panel$exp.pc$year<=1868,]%>% 
    group_by(state) %>% 
    fill(track2, faval, farmsize) %>% #default direction down
    fill(track2, faval, farmsize, .direction = "up") %>% 
    mutate_each(funs(log(.Machine$double.eps + .)), homesteads.pc,faval,farmsize) # take logs
  
  capacity.outcomes.panel$exp.pc[capacity.outcomes.panel$exp.pc$year>1868,] <- capacity.outcomes.panel$exp.pc[capacity.outcomes.panel$exp.pc$year>1868,]%>% 
    group_by(state) %>% 
    fill(track2, faval, farmsize) %>% #default direction down
    fill(track2, faval, farmsize, .direction = "up") %>% 
    mutate_each(funs(log(.Machine$double.eps + .)), homesteads.pc,faval,farmsize) # take logs
  
  ## Create interaction term
  
  capacity.outcomes.panel <- lapply(capacity.outcomes.panel, function(x){
    x$did <- NA
    x$did <- x$mask*x$homesteads.pc
    return(x)
  })
  
  f2 <- formula(outcome ~ factor(state) + mask + did + track2 + faval + farmsize)
  
  # DD Estimates

  did.model <- glm(f2, data=capacity.outcomes.panel[[d]])
  
  did.delta <- coef(did.model)[['did']]
  
  did.ci <- confint(did.model,"did")
  
  did.nobs <- nobs(did.model)
  
  did.ar2 <- summary(lm(f2, data=capacity.outcomes.panel[[d]]))$adj.r.squared
  
  # bootstrap variance estimation
  
  did.boot <- boot(data=capacity.outcomes.panel[[d]],
                                statistic=RunDiD,
                                f1=f2,
                                R=999,
                                strata=as.factor(capacity.outcomes.panel[[d]]$state), # stratify by state
                                parallel="multicore", ncpus = cores)

  did.boot.delta <- did.boot$t0

  did.boot.CI <- boot.ci(did.boot, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs

  return(list("model"=did.model,"delta"=did.delta,"CI"=did.ci,"nobs"=did.nobs, "ar2"=didar2,
              "boot"=did.boot, "boot.delta"=did.boot.delta,"did.boot.CI"=did.boot.CI))
}

# define settings 
settings <- expand.grid("imp_method"=c("mice-cart","mice-pmm","mtsdi"))

args <- commandArgs(trailingOnly = TRUE) # command line arguments
thisrun <- settings[as.numeric(args[1]),] 

imp_method <- as.character(thisrun[1])

## Load capacity data
load("results/capacity-state.RData")

results <- foreach(d = c('rev.pc','exp.pc'), .combine='cbind', .verbose = FALSE) %dopar% {
  DDCapacityEst(imp_method=imp_method, d=d, cores=cores)
}
results
saveRDS(results, paste0(output_dir,"dd_capacity_results_","imp_method_",imp_method,".rds"))

if(doMPI){
  closeCluster(cl) # close down MPIcluster
  mpi.finalize()
}else{
  stopCluster(cl)
}