###################################
# DD Estimation (county-level measures)   #
###################################
library(dplyr)
library(boot)

source(paste0(code.directory,"RunDiD.R"))

ineq.did <- census.ts.wide

# Create var for when treatment started

ineq.did$time <- NA
ineq.did$time <- 0
ineq.did$time[(ineq.did$year >= 1862)] <- 1

ineq.did$treat <- 0
ineq.did$treat <- ineq.did$homesteads.pc # treat: homesteads.pc (ln)

ineq.did$did <- NA
ineq.did$did <- ineq.did$treat* ineq.did$time 

# DD Estimates

ineq.f2 <- formula(aland.gini ~ factor(state.abb) + 
                farmval + 
                time + did)

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