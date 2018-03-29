###################################
#  Fixed effects model            #
###################################

library(boot)
library(tidyr)
library(zoo)
library(readxl)
library(parallel)
library(doParallel)
library(foreach)

source(paste0(homestead.code.directory,"RunFE.R"))

# educ 

f1 <- formula(educ.pc ~ homesteads.pc.lag + farmval.lag + state_code) 

# Southern public land state counties 

fe.south <- homestead.funds.long[homestead.funds.long$state_code %in% c(southern.pub),]

# All years
educ.south.state <- boot(data=fe.south,
                          statistic=RunFE,
                          f1=f1,
                          R=1000,
                          strata=as.factor(fe.south$state_code), # stratify by state
                          parallel="multicore", ncpus = cores)

educ.south.state.delta <- educ.south.state$t0
educ.south.state.delta

educ.south.state.CI <- boot.ci(educ.south.state, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
educ.south.state.CI

# Western public land state counties

fe.west <- homestead.funds.long[homestead.funds.long$state_code %in% c(western.pub),]

# All years

educ.west.state <- boot(data=fe.west,
                       statistic=RunFE,
                       f1=f1,
                       R=1000,
                       strata=as.factor(fe.west$state_code), # stratify by state
                       parallel="multicore", ncpus = cores)

educ.west.state.delta <- educ.west.state$t0
educ.west.state.delta

educ.west.state.CI <- boot.ci(educ.west.state, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
educ.west.state.CI

# exp 

f1 <- formula(exp.pc ~ homesteads.pc.lag + farmval.lag + state_code) 

# Southern public land state counties 

# All years
exp.south.state <- boot(data=fe.south,
                         statistic=RunFE,
                         f1=f1,
                         R=1000,
                         strata=as.factor(fe.south$state_code), # stratify by state
                         parallel="multicore", ncpus = cores)

exp.south.state.delta <- exp.south.state$t0
exp.south.state.delta

exp.south.state.CI <- boot.ci(exp.south.state, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
exp.south.state.CI

# Western public land state 

# All years

exp.west.state <- boot(data=fe.west,
                        statistic=RunFE,
                        f1=f1,
                        R=1000,
                        strata=as.factor(fe.west$state_code), # stratify by state
                        parallel="multicore", ncpus = cores)

exp.west.state.delta <- exp.west.state$t0
exp.west.state.delta

exp.west.state.CI <- boot.ci(exp.west.state, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
exp.west.state.CI

# rev 

f1 <- formula(rev.pc ~ homesteads.pc.lag + farmval.lag + state_code) 

# Southern public land state 

# All years
rev.south.state <- boot(data=fe.south,
                        statistic=RunFE,
                        f1=f1,
                        R=1000,
                        strata=as.factor(fe.south$state_code), # stratify by state
                        parallel="multicore", ncpus = cores)

rev.south.state.delta <- rev.south.state$t0
rev.south.state.delta

rev.south.state.CI <- boot.ci(rev.south.state, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
rev.south.state.CI

# Western public land state counties

# All years

rev.west.state <- boot(data=fe.west,
                       statistic=RunFE,
                       f1=f1,
                       R=1000,
                       strata=as.factor(fe.west$state_code), # stratify by state
                       parallel="multicore", ncpus = cores)

rev.west.state.delta <- rev.west.state$t0
rev.west.state.delta

rev.west.state.CI <- boot.ci(rev.west.state, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
rev.west.state.CI