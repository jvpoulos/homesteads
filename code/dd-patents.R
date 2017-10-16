###################################
# DD Estimation for comparison    #
###################################
library(dplyr)
library(boot)

source(paste0(code.directory,"RunDid.R"))

## Analysis 0: Effect of HSA on treated (western public land states), intervention: May 1862
# controls are *southern public land states*

patents.did<- patents.sum[c("state_code","year","sales.pc", "homesteads.pc", "cat")]

patents.did <- patents.did[!is.na(patents.did$cat),] # rm southern states

# Create var for when treatment started

patents.did$time <- NA
patents.did$time <- 0
patents.did$time[(patents.did$year >= 1862)] <- 1

patents.did$treat <- NA
patents.did$treat <- ifelse(patents.did$cat=="Treated",1,0)

patents.did$did <- NA
patents.did$did <- patents.did$treat* patents.did$time

# DD Estimates

# sales.pc 

sales.data <- data.frame(subset(patents.did, !is.na(sales.pc) & year>=1820 & year<=1976, select=c('time','treat','did','sales.pc')))
colnames(sales.data)<- c('time','treat','did','y')

sales.est <- boot(sales.data,
                  RunDiD, R=1000, 
                  strata=sales.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

sales.est[1]

boot.ci(sales.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

did.sales.pc <- lm(sales.pc ~ treat*time, data = patents.did[patents.did$year>=1820 & patents.did$year<=1976, ]) # LM sanity check

summary(did.sales.pc)

confint(did.sales.pc)[4,]

# homesteads.pc 

homesteads.data <- data.frame(subset(patents.did, !is.na(homesteads.pc) & year>=1820 & year<=1976, select=c('time','treat','did','homesteads.pc')))
colnames(homesteads.data)<- c('time','treat','did','y')

homesteads.est <- boot(homesteads.data,
                  RunDiD, R=1000, 
                  strata=homesteads.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

homesteads.est[1]
boot.ci(homesteads.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

did.homesteads.pc <- lm(homesteads.pc ~ treat*time, data = patents.did[patents.did$year>=1820 & patents.did$year<=1976, ]) # LM sanity check

summary(did.homesteads.pc)

confint(did.homesteads.pc)[4,]

# DD Estimates (pre-GD)

# sales.pc 

sales.data <- data.frame(subset(patents.did, !is.na(sales.pc) & year>=1820 & year <1929, select=c('time','treat','did','sales.pc')))
colnames(sales.data)<- c('time','treat','did','y')

sales.est <- boot(sales.data,
                  RunDiD, R=1000, 
                  strata=sales.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

sales.est[1]

boot.ci(sales.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

# homesteads.pc 

homesteads.data <- data.frame(subset(patents.did, !is.na(homesteads.pc) & year>=1820 & year <1929, select=c('time','treat','did','homesteads.pc')))
colnames(homesteads.data)<- c('time','treat','did','y')

homesteads.est <- boot(homesteads.data,
                  RunDiD, R=1000, 
                  strata=homesteads.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

homesteads.est[1]
boot.ci(homesteads.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs


## Analysis 1: Effect of SHA on treated (southern public land states), intervention: June 1866-June 1876-March 1889
# controls are *western public land states*

patents.did<- patents.sum[c("state_code","year","sales.pc", "homesteads.pc", "cat")]

patents.did <- patents.did[!is.na(patents.did$cat),] 

# Create var for when treatment started

patents.did$time <- NA
patents.did$time <- 0
patents.did$time[(patents.did$year >= 1866)] <- 1

patents.did$treat <- NA
patents.did$treat <- ifelse(patents.did$cat=="Treated",1,0)

patents.did$did <- NA
patents.did$did <- patents.did$treat* patents.did$time

# DD Estimates (intervention period)

# sales.pc 

sales.data <- data.frame(subset(patents.did, !is.na(sales.pc) & year>=1820 & year<=1976, select=c('time','treat','did','sales.pc')))
colnames(sales.data)<- c('time','treat','did','y')

sales.est <- boot(sales.data,
                  RunDiD, R=1000, 
                  strata=sales.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

sales.est[1]

boot.ci(sales.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

# homesteads.pc 

homesteads.data <- data.frame(subset(patents.did, !is.na(homesteads.pc) & year>=1820 & year<=1976, select=c('time','treat','did','homesteads.pc')))
colnames(homesteads.data)<- c('time','treat','did','y')

homesteads.est <- boot(homesteads.data,
                  RunDiD, R=1000, 
                  strata=homesteads.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

homesteads.est[1]
boot.ci(homesteads.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

# DD Estimates (pre-GD)

# sales.pc 

sales.data <- data.frame(subset(patents.did, !is.na(sales.pc) & year <1929, select=c('time','treat','did','sales.pc')))
colnames(sales.data)<- c('time','treat','did','y')

sales.est <- boot(sales.data,
                  RunDiD, R=1000, 
                  strata=sales.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

sales.est[1]

boot.ci(sales.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

# homesteads.pc 

homesteads.data <- data.frame(subset(patents.did, !is.na(homesteads.pc) & year <1929, select=c('time','treat','did','homesteads.pc')))
colnames(homesteads.data)<- c('time','treat','did','y')

homesteads.est <- boot(homesteads.data,
                  RunDiD, R=1000, 
                  strata=homesteads.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

homesteads.est[1]
boot.ci(homesteads.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

## Analysis 3: Effect of HSA restriction on treated, intervention: Mar 1889
# Treated is southern public land states (not MO)
# Controls are MO, state land states

patents.did<- patents.sum[c("state_code","year","sales.pc", "homesteads.pc", "cat")]

patents.did <- patents.did[!is.na(patents.did$cat),] 

# Create var for when treatment started

patents.did$time <- 0
patents.did$time[patents.did$year >= 1889] <- 1

patents.did$treat <- NA
patents.did$treat <- ifelse(patents.did$cat=="Treated",1,0)

patents.did$did <- NA
patents.did$did <- patents.did$treat* patents.did$time

# DD estimates

# sales.pc 

sales.data <- data.frame(subset(patents.did, !is.na(sales.pc), select=c('time','treat','did','sales.pc')))
colnames(sales.data)<- c('time','treat','did','y')

sales.est <- boot(sales.data,
                  RunDiD, R=1000, 
                  strata=sales.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

sales.est[1]

boot.ci(sales.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

# homesteads.pc 
homesteads.data <- data.frame(subset(patents.did, !is.na(homesteads.pc), select=c('time','treat','did','homesteads.pc')))
colnames(homesteads.data)<- c('time','treat','did','y')

homesteads.est <- boot(homesteads.data,
                  RunDiD, R=1000, 
                  strata=homesteads.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

homesteads.est[1]
boot.ci(homesteads.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs


# DD estimates (pre-GD)

# sales.pc 

sales.data <- data.frame(subset(patents.did, !is.na(sales.pc) & year <1929, select=c('time','treat','did','sales.pc')))
colnames(sales.data)<- c('time','treat','did','y')

sales.est <- boot(sales.data,
                  RunDiD, R=1000, 
                  strata=sales.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

sales.est[1]

boot.ci(sales.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

# homesteads.pc 
homesteads.data <- data.frame(subset(patents.did, !is.na(homesteads.pc) & year <1929, select=c('time','treat','did','homesteads.pc')))
colnames(homesteads.data)<- c('time','treat','did','y')

homesteads.est <- boot(homesteads.data,
                  RunDiD, R=1000, 
                  strata=homesteads.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

homesteads.est[1]
boot.ci(homesteads.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs


## Analysis 4: Effect of HSA restriction on treated, intervention: Mar 1889
# Treated is non-southern public land states (not MO)
# Controls are MO, state land states

patents.did<- patents.sum[c("state_code","year","sales.pc", "homesteads.pc", "cat")]

patents.did <- patents.did[!is.na(patents.did$cat),] 

# Create var for when treatment started

patents.did$time <- 0
patents.did$time[patents.did$year >= 1889] <- 1

patents.did$treat <- NA
patents.did$treat <- ifelse(patents.did$cat=="Treated",1,0)

patents.did$did <- NA
patents.did$did <- patents.did$treat* patents.did$time

# DD estimates

# sales.pc 

sales.data <- data.frame(subset(patents.did, !is.na(sales.pc), select=c('time','treat','did','sales.pc')))
colnames(sales.data)<- c('time','treat','did','y')

sales.est <- boot(sales.data,
                  RunDiD, R=1000, 
                  strata=sales.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

sales.est[1]

boot.ci(sales.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

# homesteads.pc 
homesteads.data <- data.frame(subset(patents.did, !is.na(homesteads.pc), select=c('time','treat','did','homesteads.pc')))
colnames(homesteads.data)<- c('time','treat','did','y')

homesteads.est <- boot(homesteads.data,
                  RunDiD, R=1000, 
                  strata=homesteads.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

homesteads.est[1]
boot.ci(homesteads.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

# DD estimates (pre-GD)

# sales.pc 

sales.data <- data.frame(subset(patents.did, !is.na(sales.pc) & year <1929, select=c('time','treat','did','sales.pc')))
colnames(sales.data)<- c('time','treat','did','y')

sales.est <- boot(sales.data,
                  RunDiD, R=1000, 
                  strata=sales.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

sales.est[1]

boot.ci(sales.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

# homesteads.pc 
homesteads.data <- data.frame(subset(patents.did, !is.na(homesteads.pc) & year <1929, select=c('time','treat','did','homesteads.pc')))
colnames(homesteads.data)<- c('time','treat','did','y')

homesteads.est <- boot(homesteads.data,
                  RunDiD, R=1000, 
                  strata=homesteads.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

homesteads.est[1]
boot.ci(homesteads.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs