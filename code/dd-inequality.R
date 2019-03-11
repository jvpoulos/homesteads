###################################
# DD Estimation (county-level measures)   #
###################################
library(dplyr)
library(boot)

## Setup parallel processing 
library(parallel)
library(doParallel)

cores <- 4#detectCores()

cl <- parallel::makeForkCluster(cores)

doParallel::registerDoParallel(cores) # register cores (<p)

RNGkind("L'Ecuyer-CMRG") # ensure random number generation

source(paste0(code.directory,"RunDiD.R"))

ineq.did <- census.ts.wide[c("state.abb","county","year","state","fips","aland.gini","homesteads.sum","homesteads.pc")]

## Create mask
ineq.did$mask <- NA
ineq.did$mask[ineq.did$homesteads.sum>0] <- 1

ineq.did <- ineq.did %>% 
  group_by(state,county) %>% 
  fill(mask)  #default direction down

ineq.did$mask[is.na(ineq.did$mask)] <- 0
  
# homestead NAs are log(0)
ineq.did$homesteads.pc[is.na(ineq.did$homesteads.pc)] <- log(.Machine$double.eps)

# Fill missing ineq

ineq.did$aland.gini[is.nan(ineq.did$aland.gini)] <- NA

ineq.did[ineq.did$year<=1870,] <- ineq.did[ineq.did$year<=1870,]%>% 
  group_by(state,county) %>% 
  fill(aland.gini) %>% #default direction down
  fill(aland.gini, .direction = "up") 

ineq.did[ineq.did$year>1870,] <- ineq.did[ineq.did$year>1870,]%>% 
  group_by(state,county) %>% 
  fill(aland.gini) %>% #default direction down
  fill(aland.gini, .direction = "up") 

## Create interaction term

ineq.did$did <- NA
ineq.did$did <- ineq.did$mask*ineq.did$homesteads.pc

ineq.f2 <- formula(aland.gini ~ factor(state) + 
                factor(year) + mask + did)
# DD Estimates

aland.gini.robust.did <- boot(data=ineq.did,
                        statistic=RunDiD,
                        f1=ineq.f2,
                        R=1000,
                        strata=as.factor(ineq.did$state), # stratify by state
                        parallel="multicore", ncpus = cores)

aland.gini.robust.did.delta <- aland.gini.robust.did$t0
aland.gini.robust.did.delta

aland.gini.robust.did.CI <- boot.ci(aland.gini.robust.did, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
aland.gini.robust.did.CI