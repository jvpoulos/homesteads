###################################
# DD Estimation for comparison    #
###################################
library(dplyr)

source(paste0(code.directory,"RunDid.R"))

## Analysis 1: Effect of SHA on treated (southern public land states), intervention: June 1866-June 1876-March 1889
# controls are southern state land states

funds.did<- funds[c("state","year","rev.pc", "exp.pc", "cat")]

funds.did <- funds.did[!is.na(funds.did$cat),] # rm non-southern state land states

# Create var for when treatment started

funds.did$time <- NA
funds.did$time <- 0
funds.did$time[(funds.did$year >= 1866)] <- 1

funds.did$treat <- NA
funds.did$treat <- ifelse(funds.did$cat=="Treated",1,0)

funds.did$did <- NA
funds.did$did <- funds.did$treat* funds.did$time

# DD Estimates (pre-GD)

# rev.pc 

revpc.data <- data.frame(subset(funds.did, !is.na(rev.pc) & year <=1915, select=c('time','treat','did','rev.pc')))
colnames(revpc.data)<- c('time','treat','did','y')

revpc.est <- boot(revpc.data,
                 RunDiD, R=1000, 
                 strata=revpc.data$did, # stratify at time*treat
                 parallel="multicore", ncpus = cores)

revpc.est[1]

boot.ci(revpc.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

# LM sanity check
did.rev.pc <- lm(rev.pc ~ treat*time, data = funds.did[funds.did$year <=1915,]) 

summary(did.rev.pc)

confint(did.rev.pc)[4,]

# exp.pc 

exppc.data <- data.frame(subset(funds.did, !is.na(exp.pc) & year <=1915, select=c('time','treat','did','exp.pc')))
colnames(exppc.data)<- c('time','treat','did','y')

exppc.est <- boot(exppc.data,
                  RunDiD, R=1000, 
                  strata=exppc.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

exppc.est[1]
boot.ci(exppc.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

# LM sanity check
did.exp.pc <- lm(exp.pc ~ treat*time, data = funds.did[funds.did$year <=1915,]) 

summary(did.exp.pc)

confint(did.exp.pc)[4,]

## Analysis 3: Effect of HSA restriction on treated, intervention: Mar 1889
# Treated is non-southern public land states (not MO)
# Controls are MO, state land states

funds.did<- funds[c("state","year","rev.pc", "exp.pc", "treat")]

funds.did <- funds.did[!is.na(funds.did$treat),] # rm non-southern state land states

# Create var for when treatment started

funds.did$time <- 0
funds.did$time[funds.did$year >= 1889] <- 1

funds.did$treat <- NA
funds.did$treat <- ifelse(funds.did$treat=="Treated",1,0)

funds.did$did <- NA
funds.did$did <- funds.did$treat* funds.did$time

# DD estimates (pre-GD)

# rev.pc 

revpc.data <- data.frame(subset(funds.did, !is.na(rev.pc) & year <=1915, select=c('time','treat','did','rev.pc')))
colnames(revpc.data)<- c('time','treat','did','y')

revpc.est <- boot(revpc.data,
                  RunDiD, R=1000, 
                  strata=revpc.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

revpc.est[1]

boot.ci(revpc.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

# LM sanity check
did.rev.pc <- lm(rev.pc ~ treat*time, data = funds.did[funds.did$year <=1915,]) 

summary(did.rev.pc)

confint(did.rev.pc)[4,]

# exp.pc 
exppc.data <- data.frame(subset(funds.did, !is.na(exp.pc) & year <=1915, select=c('time','treat','did','exp.pc')))
colnames(exppc.data)<- c('time','treat','did','y')

exppc.est <- boot(exppc.data,
                  RunDiD, R=1000, 
                  strata=exppc.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

exppc.est[1]
boot.ci(exppc.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

# LM sanity check
did.exp.pc <- lm(exp.pc ~ treat*time, data = funds.did[funds.did$year <=1915,]) 

summary(did.exp.pc)

confint(did.exp.pc)[4,]