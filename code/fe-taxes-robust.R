###################################
#  Fixed effects model  (robust)#
###################################

library(boot)
library(tidyr)
library(zoo)
library(readxl)
library(parallel)
library(doParallel)
library(foreach)

source(paste0(homestead.code.directory,"RunFE-robust.R"))

# Tax1 + RR access

f1 <- formula(taxpc1 ~ homesteads.pc.lag + access.mean.lag + id) 

# Southern public land state counties 

fe.south <- homestead.tax.long[homestead.tax.long$state.abb %in% c(southern.pub),]

# All years
tax1.south.fe <- boot(data=fe.south,
                          statistic=RunFERobust,
                          f1=f1,
                          R=1000,
                          parallel="multicore", ncpus = cores)

tax1.south.fe.delta <- tax1.south.fe$t0
tax1.south.fe.delta

tax1.south.fe.CI <- boot.ci(tax1.south.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
tax1.south.fe.CI

boot.ci(tax1.south.fe, conf=0.95, index=2, type="norm")$normal[2:3] # railroad access

# Western public land state counties

fe.west <- homestead.tax.long[homestead.tax.long$state.abb %in% c(western.pub),]

# All years

tax1.west.fe <- boot(data=fe.west,
                       statistic=RunFERobust,
                       f1=f1,
                       R=1000,
                       parallel="multicore", ncpus = cores)

tax1.west.fe.delta <- tax1.west.fe$t0
tax1.west.fe.delta

tax1.west.fe.CI <- boot.ci(tax1.west.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
tax1.west.fe.CI

boot.ci(tax1.west.fe, conf=0.95, index=2, type="norm")$normal[2:3] # RR

# All public land states

fe.all <- homestead.tax.long[homestead.tax.long$state.abb %in% c(pub.states),]

# All years

tax1.all.fe <- boot(data=fe.all,
                      statistic=RunFERobust,
                      f1=f1,
                      R=1000,
                      parallel="multicore", ncpus = cores)

tax1.all.fe.delta <- tax1.all.fe$t0
tax1.all.fe.delta

tax1.all.fe.CI <- boot.ci(tax1.all.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
tax1.all.fe.CI

boot.ci(tax1.all.fe, conf=0.95, index=2, type="norm")$normal[2:3] # RR

# tax2 + RR access

f1 <- formula(taxpc2 ~ homesteads.pc.lag + access.mean.lag + id) 

# Southern public land state counties 

fe.south <- homestead.tax.long[homestead.tax.long$state.abb %in% c(southern.pub),]

# All years
tax2.south.fe <- boot(data=fe.south,
                      statistic=RunFERobust,
                      f1=f1,
                      R=1000,
                      parallel="multicore", ncpus = cores)

tax2.south.fe.delta <- tax2.south.fe$t0
tax2.south.fe.delta

tax2.south.fe.CI <- boot.ci(tax2.south.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
tax2.south.fe.CI

boot.ci(tax2.south.fe, conf=0.95, index=2, type="norm")$normal[2:3] # RR

# Western public land state counties

fe.west <- homestead.tax.long[homestead.tax.long$state.abb %in% c(western.pub),]

# All years

tax2.west.fe <- boot(data=fe.west,
                     statistic=RunFERobust,
                     f1=f1,
                     R=1000,
                     parallel="multicore", ncpus = cores)

tax2.west.fe.delta <- tax2.west.fe$t0
tax2.west.fe.delta

tax2.west.fe.CI <- boot.ci(tax2.west.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
tax2.west.fe.CI

boot.ci(tax2.west.fe, conf=0.95, index=2, type="norm")$normal[2:3] ## RR

# All public land states

fe.all <- homestead.tax.long[homestead.tax.long$state.abb %in% c(pub.states),]

# All years

tax2.all.fe <- boot(data=fe.all,
                    statistic=RunFERobust,
                    f1=f1,
                    R=1000,
                    parallel="multicore", ncpus = cores)

tax2.all.fe.delta <- tax2.all.fe$t0
tax2.all.fe.delta

tax2.all.fe.CI <- boot.ci(tax2.all.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
tax2.all.fe.CI

boot.ci(tax2.all.fe, conf=0.95, index=2, type="norm")$normal[2:3] # rr

# land inequality + RR access

f1 <- formula(aland.gini ~ homesteads.pc.lag + access.mean.lag + id) 

# Southern public land state counties 

fe.south <- homestead.tax.long[homestead.tax.long$state.abb %in% c(southern.pub),]

# All years
aland.gini.south.fe <- boot(data=fe.south,
                      statistic=RunFERobust,
                      f1=f1,
                      R=1000,
                      parallel="multicore", ncpus = cores)

aland.gini.south.fe.delta <- aland.gini.south.fe$t0
aland.gini.south.fe.delta

aland.gini.south.fe.CI <- boot.ci(aland.gini.south.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
aland.gini.south.fe.CI

boot.ci(aland.gini.south.fe, conf=0.95, index=2, type="norm")$normal[2:3] # RR

# Western public land state counties

fe.west <- homestead.tax.long[homestead.tax.long$state.abb %in% c(western.pub),]

# All years

aland.gini.west.fe <- boot(data=fe.west,
                     statistic=RunFERobust,
                     f1=f1,
                     R=1000,
                     parallel="multicore", ncpus = cores)

aland.gini.west.fe.delta <- aland.gini.west.fe$t0
aland.gini.west.fe.delta

aland.gini.west.fe.CI <- boot.ci(aland.gini.west.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
aland.gini.west.fe.CI

boot.ci(aland.gini.west.fe, conf=0.95, index=2, type="norm")$normal[2:3] ## RR

# All public land states

fe.all <- homestead.tax.long[homestead.tax.long$state.abb %in% c(pub.states),]

# All years

aland.gini.all.fe <- boot(data=fe.all,
                    statistic=RunFERobust,
                    f1=f1,
                    R=1000,
                    parallel="multicore", ncpus = cores)

aland.gini.all.fe.delta <- aland.gini.all.fe$t0
aland.gini.all.fe.delta

aland.gini.all.fe.CI <- boot.ci(aland.gini.all.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
aland.gini.all.fe.CI

boot.ci(aland.gini.all.fe, conf=0.95, index=2, type="norm")$normal[2:3] # rr