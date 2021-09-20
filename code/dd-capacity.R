###################################
# DD Estimation (state-level measures)   #
###################################
library(data.table)
library(dplyr)
library(boot)
library(tidyverse) 

## Setup parallel processing 
library(parallel)
library(doParallel)

cores <- detectCores()

cl <- parallel::makeForkCluster(cores)

doParallel::registerDoParallel(cores) # register cores (<p)

RNGkind("L'Ecuyer-CMRG") # ensure random number generation

source("code/RunDiD.R")

## Load capacity data
load("results/capacity-state.RData")
capacity.outcomes <- readRDS(paste0(data.directory,"capacity-outcomes-mice.rds")) # MICE imputed data

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

# Covars
rev.pc.all.robust.did <- boot(data=capacity.outcomes.panel$rev.pc,
                              statistic=RunDiD,
                              f1=f2,
                              R=1000,
                              strata=as.factor(capacity.outcomes.panel$rev.pc$state), # stratify by state
                              parallel="multicore", ncpus = cores)

rev.pc.all.robust.did.delta <- rev.pc.all.robust.did$t0

rev.pc.all.robust.did.CI <- boot.ci(rev.pc.all.robust.did, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs

rev.pc.all.robust.did.delta
rev.pc.all.robust.did.CI
summary(lm(f2, data=capacity.outcomes.panel$rev.pc))
nobs(lm(f2, data=capacity.outcomes.panel$rev.pc))

# Covars
exp.pc.all.robust.did <- boot(data=capacity.outcomes.panel$exp.pc,
                              statistic=RunDiD,
                              f1=f2,
                              R=1000,
                              strata=as.factor(capacity.outcomes.panel$exp.pc$state), # stratify by state
                              parallel="multicore", ncpus = cores)

exp.pc.all.robust.did.delta <- exp.pc.all.robust.did$t0

exp.pc.all.robust.did.CI <- boot.ci(exp.pc.all.robust.did, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs

exp.pc.all.robust.did.delta
exp.pc.all.robust.did.CI
summary(lm(f2, data=capacity.outcomes.panel$exp.pc))
nobs(lm(f2, data=capacity.outcomes.panel$exp.pc))