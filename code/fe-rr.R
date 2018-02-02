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

# Railroad access

f1 <- formula(access.mean ~ homesteads.pc.lag + id) 

# Southern public land state counties 

fe.south <- homestead.rr.long[homestead.rr.long$state.abb %in% c(southern.pub),]

# All years
rr.south.fe <- boot(data=fe.south,
                          statistic=RunFE,
                          f1=f1,
                          R=1000,
                          parallel="multicore", ncpus = cores)

rr.south.fe.delta <- rr.south.fe$t0
rr.south.fe.delta

rr.south.fe.CI <- boot.ci(rr.south.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
rr.south.fe.CI

# Western public land state counties

fe.west <- homestead.rr.long[homestead.rr.long$state.abb %in% c(western.pub),]

# All years

rr.west.fe <- boot(data=fe.west,
                       statistic=RunFE,
                       f1=f1,
                       R=1000,
                       parallel="multicore", ncpus = cores)

rr.west.fe.delta <- rr.west.fe$t0
rr.west.fe.delta

rr.west.fe.CI <- boot.ci(rr.west.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
rr.west.fe.CI

# All public land states

fe.all <- homestead.rr.long[homestead.rr.long$state.abb %in% c(pub.states),]

# All years

rr.all.fe <- boot(data=fe.all,
                      statistic=RunFE,
                      f1=f1,
                      R=1000,
                      parallel="multicore", ncpus = cores)

rr.all.fe.delta <- rr.all.fe$t0
rr.all.fe.delta

rr.all.fe.CI <- boot.ci(rr.all.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
rr.all.fe.CI