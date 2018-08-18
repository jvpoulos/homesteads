###################################
# DD Estimation on RR access (county-level)  #
###################################
library(dplyr)
library(boot)

source(paste0(code.directory,"RunDid.R"))

## Western public land states

rr.did <- homestead.rr.long[homestead.rr.long$state.abb %in% western.pub,]

# Create var for when treatment started

rr.did$time <- NA
rr.did$time <- 0
rr.did$time[(rr.did$year >= 1862)] <- 1

rr.did$treat <- 0
rr.did$treat <- rr.did$homesteads.pc 

rr.did$did <- NA
rr.did$did <- rr.did$treat* rr.did$time 

# DD Estimates

rr.f1 <- formula(access.mean ~ factor(state.abb) + 
                time + did)

rr.f2 <- formula(access.mean ~ factor(state.abb) + 
                farmval + 
                time + did)

# All years
access.did <- boot(data=rr.did,
                        statistic=RunDiD,
                        f1=rr.f1,
                        R=1000,
                        strata=as.factor(rr.did$state.abb), # stratify by state
                        parallel="multicore", ncpus = cores)

access.did.delta <- access.did$t0
access.did.delta

access.did.CI <- boot.ci(access.did, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
access.did.CI

# All years
access.robust.did <- boot(data=rr.did,
                        statistic=RunDiD,
                        f1=rr.f2,
                        R=1000,
                        strata=as.factor(rr.did$state.abb), # stratify by state
                        parallel="multicore", ncpus = cores)

access.robust.did.delta <- access.robust.did$t0
access.robust.did.delta

access.robust.did.CI <- boot.ci(access.robust.did, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
access.robust.did.CI

## Southern public land states

rr.did.south<- homestead.rr.long[homestead.rr.long$state.abb %in% southern.pub,]

# Create var for when treatment started

rr.did.south$time <- NA
rr.did.south$time <- 0
rr.did.south$time[(rr.did.south$year >= 1866)] <- 1

rr.did.south$treat <- 0
rr.did.south$treat <- rr.did.south$homesteads.pc 

rr.did.south$did <- NA
rr.did.south$did <- rr.did.south$treat* rr.did.south$time 

# DD Estimates

access.south.did <- boot(data=rr.did.south,
                        statistic=RunDiD,
                        f1=rr.f1,
                        R=1000,
                        strata=as.factor(rr.did.south$state.abb), # stratify by state
                        parallel="multicore", ncpus = cores)

access.south.did.delta <- access.south.did$t0
access.south.did.delta

access.south.did.CI <- boot.ci(access.south.did, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
access.south.did.CI

# All years
access.south.robust.did <- boot(data=rr.did.south,
                               statistic=RunDiD,
                               f1=rr.f2,
                               R=1000,
                               strata=as.factor(rr.did.south$state.abb), # stratify by state
                               parallel="multicore", ncpus = cores)

access.south.robust.did.delta <- access.south.robust.did$t0
access.south.robust.did.delta

access.south.robust.did.CI <- boot.ci(access.south.robust.did, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
access.south.robust.did.CI