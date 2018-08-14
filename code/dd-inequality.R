###################################
# DD Estimation (county-level measures)   #
###################################
library(dplyr)
library(boot)

source(paste0(code.directory,"RunDid.R"))

## Western public land states

ineq.did <- homestead.rr.long[homestead.rr.long$state.abb %in% western.pub,]

# Create var for when treatment started

ineq.did$time <- NA
ineq.did$time <- 0
ineq.did$time[(ineq.did$year >= 1862)] <- 1

ineq.did$treat <- 0
ineq.did$treat <- ineq.did$homesteads.pc # treat: homesteads.pc (ln)

ineq.did$did <- NA
ineq.did$did <- ineq.did$treat* ineq.did$time 

# DD Estimates

ineq.f1 <- formula(aland.gini ~ factor(state.abb) + 
                time + did)

ineq.f2 <- formula(aland.gini ~ factor(state.abb) + 
                farmval + 
                time + did)

# All years
aland.gini.did <- boot(data=ineq.did,
                        statistic=RunDiD,
                        f1=ineq.f1,
                        R=1000,
                        strata=as.factor(ineq.did$state.abb), # stratify by state
                        parallel="multicore", ncpus = cores)

aland.gini.did.delta <- aland.gini.did$t0

aland.gini.did.CI <- boot.ci(aland.gini.did, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs

# All years
aland.gini.robust.did <- boot(data=ineq.did,
                        statistic=RunDiD,
                        f1=ineq.f2,
                        R=1000,
                        strata=as.factor(ineq.did$state.abb), # stratify by state
                        parallel="multicore", ncpus = cores)

aland.gini.robust.did.delta <- aland.gini.robust.did$t0
aland.gini.robust.did.delta

aland.gini.robust.did.CI <- boot.ci(aland.gini.robust.did, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
aland.gini.robust.did.CI

## Southern public land states

ineq.did.south <- homestead.rr.long[homestead.rr.long$state.abb %in% southern.pub,]

# Create var for when treatment started

ineq.did.south$time <- NA
ineq.did.south$time <- 0
ineq.did.south$time[(ineq.did.south$year >= 1862)] <- 1

ineq.did.south$treat <- 0
ineq.did.south$treat <- ineq.did.south$homesteads.pc # treat: homesteads.pc (ln)

ineq.did.south$did <- NA
ineq.did.south$did <- ineq.did.south$treat* ineq.did.south$time 

# DD Estimates

aland.gini.south.did <- boot(data=ineq.did.south,
                        statistic=RunDiD,
                        f1=ineq.f1,
                        R=1000,
                        strata=as.factor(ineq.did.south$state.abb), # stratify by state
                        parallel="multicore", ncpus = cores)

aland.gini.south.did.delta <- aland.gini.south.did$t0

aland.gini.south.did.CI <- boot.ci(aland.gini.south.did, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs

# All years
aland.gini.south.robust.did <- boot(data=ineq.did.south,
                               statistic=RunDiD,
                               f1=ineq.f2,
                               R=1000,
                               strata=as.factor(ineq.did.south$state.abb), # stratify by state
                               parallel="multicore", ncpus = cores)

aland.gini.south.robust.did.delta <- aland.gini.south.robust.did$t0
aland.gini.south.robust.did.delta

aland.gini.south.robust.did.CI <- boot.ci(aland.gini.south.robust.did, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
aland.gini.south.robust.did.CI