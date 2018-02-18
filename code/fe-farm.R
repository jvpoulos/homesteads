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

## Farm value

f1 <- formula(farmval~ homesteads.pc.lag + id) 

# Southern public land state counties 

fe.south <- homestead.tax.long[homestead.tax.long$state.abb %in% c(southern.pub),]

# All years
farmval.south.fe <- boot(data=fe.south,
                        statistic=RunFE,
                        f1=f1,
                        R=1000,
                        parallel="multicore", ncpus = cores)

farmval.south.fe.delta <- farmval.south.fe$t0
farmval.south.fe.delta

farmval.south.fe.CI <- boot.ci(farmval.south.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
farmval.south.fe.CI

# Western public land state counties

fe.west <- homestead.tax.long[homestead.tax.long$state.abb %in% c(western.pub),]

# All years

farmval.west.fe <- boot(data=fe.west,
                       statistic=RunFE,
                       f1=f1,
                       R=1000,
                       parallel="multicore", ncpus = cores)

farmval.west.fe.delta <- farmval.west.fe$t0
farmval.west.fe.delta

farmval.west.fe.CI <- boot.ci(farmval.west.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
farmval.west.fe.CI

# All public land states

fe.all <- homestead.tax.long[homestead.tax.long$state.abb %in% c(pub.states),]

# All years

farmval.all.fe <- boot(data=fe.all,
                      statistic=RunFE,
                      f1=f1,
                      R=1000,
                      parallel="multicore", ncpus = cores)

farmval.all.fe.delta <- farmval.all.fe$t0
farmval.all.fe.delta

farmval.all.fe.CI <- boot.ci(farmval.all.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
farmval.all.fe.CI

## Farm output

f1 <- formula(output~ homesteads.pc.lag + id) 

# Southern public land state counties 

fe.south <- census.ts.wide[census.ts.wide$state.abb %in% c(southern.pub),]

# All years
output.south.fe <- boot(data=fe.south,
                          statistic=RunFE,
                          f1=f1,
                          R=1000,
                          parallel="multicore", ncpus = cores)

output.south.fe.delta <- output.south.fe$t0
output.south.fe.delta

output.south.fe.CI <- boot.ci(output.south.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
output.south.fe.CI

# Western public land state counties

fe.west <- census.ts.wide[census.ts.wide$state.abb %in% c(western.pub),]

# All years

output.west.fe <- boot(data=fe.west,
                       statistic=RunFE,
                       f1=f1,
                       R=1000,
                       parallel="multicore", ncpus = cores)

output.west.fe.delta <- output.west.fe$t0
output.west.fe.delta

output.west.fe.CI <- boot.ci(output.west.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
output.west.fe.CI

# All public land states

fe.all <- census.ts.wide[census.ts.wide$state.abb %in% c(pub.states),]

# All years

output.all.fe <- boot(data=fe.all,
                      statistic=RunFE,
                      f1=f1,
                      R=1000,
                      parallel="multicore", ncpus = cores)

output.all.fe.delta <- output.all.fe$t0
output.all.fe.delta

output.all.fe.CI <- boot.ci(output.all.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
output.all.fe.CI

## Farm tenancy

f1 <- formula(tenancy~ homesteads.pc.lag + id) 

# Southern public land state counties 

fe.south <- census.ts.wide[census.ts.wide$state.abb %in% c(southern.pub),]

# All years
tenancy.south.fe <- boot(data=fe.south,
                        statistic=RunFE,
                        f1=f1,
                        R=1000,
                        parallel="multicore", ncpus = cores)

tenancy.south.fe.delta <- tenancy.south.fe$t0
tenancy.south.fe.delta

tenancy.south.fe.CI <- boot.ci(tenancy.south.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
tenancy.south.fe.CI

# Western public land state counties

fe.west <- census.ts.wide[census.ts.wide$state.abb %in% c(western.pub),]

# All years

tenancy.west.fe <- boot(data=fe.west,
                       statistic=RunFE,
                       f1=f1,
                       R=1000,
                       parallel="multicore", ncpus = cores)

tenancy.west.fe.delta <- tenancy.west.fe$t0
tenancy.west.fe.delta

tenancy.west.fe.CI <- boot.ci(tenancy.west.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
tenancy.west.fe.CI

# All public land states

fe.all <- census.ts.wide[census.ts.wide$state.abb %in% c(pub.states),]

# All years

tenancy.all.fe <- boot(data=fe.all,
                      statistic=RunFE,
                      f1=f1,
                      R=1000,
                      parallel="multicore", ncpus = cores)

tenancy.all.fe.delta <- tenancy.all.fe$t0
tenancy.all.fe.delta

tenancy.all.fe.CI <- boot.ci(tenancy.all.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
tenancy.all.fe.CI

## Farm wages

f1 <- formula(wages~ homesteads.pc.lag + id) 

# Southern public land state counties 

fe.south <- census.ts.wide[census.ts.wide$state.abb %in% c(southern.pub),]

# All years
wages.south.fe <- boot(data=fe.south,
                         statistic=RunFE,
                         f1=f1,
                         R=1000,
                         parallel="multicore", ncpus = cores)

wages.south.fe.delta <- wages.south.fe$t0
wages.south.fe.delta

wages.south.fe.CI <- boot.ci(wages.south.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
wages.south.fe.CI

# Western public land state counties

fe.west <- census.ts.wide[census.ts.wide$state.abb %in% c(western.pub),]

# All years

wages.west.fe <- boot(data=fe.west,
                        statistic=RunFE,
                        f1=f1,
                        R=1000,
                        parallel="multicore", ncpus = cores)

wages.west.fe.delta <- wages.west.fe$t0
wages.west.fe.delta

wages.west.fe.CI <- boot.ci(wages.west.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
wages.west.fe.CI

# All public land states

fe.all <- census.ts.wide[census.ts.wide$state.abb %in% c(pub.states),]

# All years

wages.all.fe <- boot(data=fe.all,
                       statistic=RunFE,
                       f1=f1,
                       R=1000,
                       parallel="multicore", ncpus = cores)

wages.all.fe.delta <- wages.all.fe$t0
wages.all.fe.delta

wages.all.fe.CI <- boot.ci(wages.all.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
wages.all.fe.CI

## Farmsize

f1 <- formula(farmsize~ homesteads.pc.lag + id) 

# Southern public land state counties 

fe.south <- census.ts.wide[census.ts.wide$state.abb %in% c(southern.pub),]

# All years
farmsize.south.fe <- boot(data=fe.south,
                       statistic=RunFE,
                       f1=f1,
                       R=1000,
                       parallel="multicore", ncpus = cores)

farmsize.south.fe.delta <- farmsize.south.fe$t0
farmsize.south.fe.delta

farmsize.south.fe.CI <- boot.ci(farmsize.south.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
farmsize.south.fe.CI

# Western public land state counties

fe.west <- census.ts.wide[census.ts.wide$state.abb %in% c(western.pub),]

# All years

farmsize.west.fe <- boot(data=fe.west,
                      statistic=RunFE,
                      f1=f1,
                      R=1000,
                      parallel="multicore", ncpus = cores)

farmsize.west.fe.delta <- farmsize.west.fe$t0
farmsize.west.fe.delta

farmsize.west.fe.CI <- boot.ci(farmsize.west.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
farmsize.west.fe.CI

# All public land states

fe.all <- census.ts.wide[census.ts.wide$state.abb %in% c(pub.states),]

# All years

farmsize.all.fe <- boot(data=fe.all,
                     statistic=RunFE,
                     f1=f1,
                     R=1000,
                     parallel="multicore", ncpus = cores)

farmsize.all.fe.delta <- farmsize.all.fe$t0
farmsize.all.fe.delta

farmsize.all.fe.CI <- boot.ci(farmsize.all.fe, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
farmsize.all.fe.CI