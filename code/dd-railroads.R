###################################
# DD Estimation for comparison    #
###################################
library(dplyr)
library(boot)

source(paste0(code.directory,"RunDid.R"))

## Analysis 0: Effect of HSA on treated (western public land states), intervention: May 1862
# controls are non-southern state land states

rr.inter.did<- rr.0

rr.inter.did <- rr.inter.did[!is.na(rr.inter.did$cat),] # rm southern states

rr.inter.did$year <- rr.inter.did$InOpBy

# Create var for when treatment started

rr.inter.did$time <- NA
rr.inter.did$time <- 0
rr.inter.did$time[(rr.inter.did$year >= 1862)] <- 1

rr.inter.did$treat <- NA
rr.inter.did$treat <- ifelse(rr.inter.did$cat=="Treated",1,0)

rr.inter.did$did <- NA
rr.inter.did$did <- rr.inter.did$treat* rr.inter.did$time

# DD Estimates

# access 

access.data <- data.frame(subset(rr.inter.did, !is.na(access), select=c('time','treat','did','access')))
colnames(access.data)<- c('time','treat','did','y')

access.est <- boot(access.data,
                  RunDiD, R=1000, 
                  strata=access.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

access.est[1]

boot.ci(access.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs


## Analysis 1: Effect of SHA on treated (southern public land states), intervention: June 1866-June 1876-March 1889
# controls are southern state land states

rr.inter.did<- rr.1

rr.inter.did <- rr.inter.did[!is.na(rr.inter.did$cat),] # rm non-southern states

rr.inter.did$year <- rr.inter.did$InOpBy

# Create var for when treatment started

rr.inter.did$time <- NA
rr.inter.did$time <- 0
rr.inter.did$time[(rr.inter.did$year >= 1866)] <- 1

rr.inter.did$treat <- NA
rr.inter.did$treat <- ifelse(rr.inter.did$cat=="Treated",1,0)

rr.inter.did$did <- NA
rr.inter.did$did <- rr.inter.did$treat* rr.inter.did$time

# DD Estimates (intervention period)

# access 

access.data <- data.frame(subset(rr.inter.did, !is.na(access), select=c('time','treat','did','access')))
colnames(access.data)<- c('time','treat','did','y')

access.est <- boot(access.data,
                  RunDiD, R=1000, 
                  strata=access.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

access.est[1]

boot.ci(access.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

## Analysis 3: Effect of HSA restriction on treated, intervention: Mar 1889
# Treated is southern public land states (not MO)
# Controls are MO, state land states

rr.inter.did<- rr.3

rr.inter.did <- rr.inter.did[!is.na(rr.inter.did$cat),] 

rr.inter.did$year <- rr.inter.did$InOpBy

# Create var for when treatment started

rr.inter.did$time <- 0
rr.inter.did$time[rr.inter.did$year >= 1889] <- 1

rr.inter.did$treat <- NA
rr.inter.did$treat <- ifelse(rr.inter.did$cat=="Treated",1,0)

rr.inter.did$did <- NA
rr.inter.did$did <- rr.inter.did$treat* rr.inter.did$time

# DD estimates

# access 

access.data <- data.frame(subset(rr.inter.did, !is.na(access), select=c('time','treat','did','access')))
colnames(access.data)<- c('time','treat','did','y')

access.est <- boot(access.data,
                  RunDiD, R=1000, 
                  strata=access.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

access.est[1]

boot.ci(access.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs


## Analysis 4: Effect of HSA restriction on treated, intervention: Mar 1889
# Treated is non-southern public land states (not MO)
# Controls are MO, state land states

rr.inter.did<- rr.4

rr.inter.did <- rr.inter.did[!is.na(rr.inter.did$cat),] 

rr.inter.did$year <- rr.inter.did$InOpBy

# Create var for when treatment started

rr.inter.did$time <- 0
rr.inter.did$time[rr.inter.did$year >= 1889] <- 1

rr.inter.did$treat <- NA
rr.inter.did$treat <- ifelse(rr.inter.did$cat=="Treated",1,0)

rr.inter.did$did <- NA
rr.inter.did$did <- rr.inter.did$treat* rr.inter.did$time

# DD estimates

# access 

access.data <- data.frame(subset(rr.inter.did, !is.na(access), select=c('time','treat','did','access')))
colnames(access.data)<- c('time','treat','did','y')

access.est <- boot(access.data,
                  RunDiD, R=1000, 
                  strata=access.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

access.est[1]

boot.ci(access.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs