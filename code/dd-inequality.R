##########################################
# DD Estimation (county-level measures)   #
##########################################
#N.B.: need to run dd-capacity.R first

library(dplyr)
library(boot)

## Setup parallel processing 
library(parallel)
library(doParallel)

cores <- detectCores()

cl <- parallel::makeForkCluster(cores)

doParallel::registerDoParallel(cores) # register cores (<p)

RNGkind("L'Ecuyer-CMRG") # ensure random number generation

source(paste0(code.directory,"RunDiD.R"))

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

ineq.f2 <- formula(aland.gini ~ factor(state) + 
                factor(year) + mask + did + faval)
# DD Estimates

aland.gini.robust.did <- boot(data=ineq.did,
                        statistic=RunDiD,
                        f1=ineq.f2,
                        R=1000,
                        strata=as.factor(ineq.did$state), # stratify by state
                        parallel="multicore", ncpus = cores)

aland.gini.robust.did.delta <- aland.gini.robust.did$t0

aland.gini.robust.did.CI <- boot.ci(aland.gini.robust.did, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs

formatC(aland.gini.robust.did.delta,format="e")
aland.gini.robust.did.CI
summary(lm(ineq.f2, data=ineq.did))
nobs(lm(ineq.f2, data=ineq.did))