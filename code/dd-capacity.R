###################################
# DD Estimation for comparison    #
###################################
library(dplyr)
library(boot)

source(paste0(code.directory,"RunDid.R"))

## Analysis 0: Effect of HSA on treated (western public land states), intervention: May 1862
# controls are non-southern state land states

funds.did<- funds[c("state","year","rev.pc", "exp.pc","educ.pc", "cat")]

funds.did <- funds.did[!is.na(funds.did$cat),] # rm southern states

# Create var for when treatment started

funds.did$time <- NA
funds.did$time <- 0
funds.did$time[(funds.did$year >= 1862)] <- 1

funds.did$treat <- NA
funds.did$treat <- ifelse(funds.did$cat=="Treated",1,0)

funds.did$did <- NA
funds.did$did <- funds.did$treat* funds.did$time

# DD Estimates

# rev.pc 

revpc.data <- data.frame(subset(funds.did, !is.na(rev.pc), select=c('time','treat','did','rev.pc')))
colnames(revpc.data)<- c('time','treat','did','y')

revpc.est <- boot(revpc.data,
                  RunDiD, R=1000, 
                  strata=revpc.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

revpc.est[1]

boot.ci(revpc.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

# exp.pc 

exppc.data <- data.frame(subset(funds.did, !is.na(exp.pc), select=c('time','treat','did','exp.pc')))
colnames(exppc.data)<- c('time','treat','did','y')

exppc.est <- boot(exppc.data,
                  RunDiD, R=1000, 
                  strata=exppc.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

exppc.est[1]
boot.ci(exppc.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

# educ.pc 

educpc.data <- data.frame(subset(funds.did, !is.na(educ.pc), select=c('time','treat','did','educ.pc')))
colnames(educpc.data)<- c('time','treat','did','y')

educpc.est <- boot(educpc.data,
                  RunDiD, R=1000, 
                  strata=educpc.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

educpc.est[1]
boot.ci(educpc.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

# LM sanity check
did.educ.pc <- lm(educ.pc ~ treat*time, data = funds.did) 

summary(did.educ.pc)

confint(did.educ.pc)[4,]


# DD Estimates (pre-GD)

# rev.pc 

revpc.data <- data.frame(subset(funds.did, !is.na(rev.pc) & year <1929, select=c('time','treat','did','rev.pc')))
colnames(revpc.data)<- c('time','treat','did','y')

revpc.est <- boot(revpc.data,
                  RunDiD, R=1000, 
                  strata=revpc.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

revpc.est[1]

boot.ci(revpc.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

# exp.pc 

exppc.data <- data.frame(subset(funds.did, !is.na(exp.pc) & year <1929, select=c('time','treat','did','exp.pc')))
colnames(exppc.data)<- c('time','treat','did','y')

exppc.est <- boot(exppc.data,
                  RunDiD, R=1000, 
                  strata=exppc.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

exppc.est[1]
boot.ci(exppc.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs


# educ.pc 

educpc.data <- data.frame(subset(funds.did, !is.na(educ.pc) & year <1929, select=c('time','treat','did','educ.pc')))
colnames(educpc.data)<- c('time','treat','did','y')

educpc.est <- boot(educpc.data,
                  RunDiD, R=1000, 
                  strata=educpc.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

educpc.est[1]
boot.ci(educpc.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

# LM sanity check
did.educ.pc <- lm(educ.pc ~ treat*time, data = funds.did[funds.did$year <1929,]) 

summary(did.educ.pc)

confint(did.educ.pc)[4,]

## Analysis 1: Effect of SHA on treated (southern public land states), intervention: June 1866-June 1876-March 1889
# controls are southern state land states

funds.did<- funds[c("state","year","rev.pc", "exp.pc", "educ.pc","cat")]

funds.did <- funds.did[!is.na(funds.did$cat),] # rm non-southern states

# Create var for when treatment started

funds.did$time <- NA
funds.did$time <- 0
funds.did$time[(funds.did$year >= 1866)] <- 1

funds.did$treat <- NA
funds.did$treat <- ifelse(funds.did$cat=="Treated",1,0)

funds.did$did <- NA
funds.did$did <- funds.did$treat* funds.did$time

# DD Estimates (intervention period)

# rev.pc 

revpc.data <- data.frame(subset(funds.did, !is.na(rev.pc), select=c('time','treat','did','rev.pc')))
colnames(revpc.data)<- c('time','treat','did','y')

revpc.est <- boot(revpc.data,
                  RunDiD, R=1000, 
                  strata=revpc.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

revpc.est[1]

boot.ci(revpc.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

# exp.pc 

exppc.data <- data.frame(subset(funds.did, !is.na(exp.pc), select=c('time','treat','did','exp.pc')))
colnames(exppc.data)<- c('time','treat','did','y')

exppc.est <- boot(exppc.data,
                  RunDiD, R=1000, 
                  strata=exppc.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

exppc.est[1]
boot.ci(exppc.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

# educ.pc 

educpc.data <- data.frame(subset(funds.did, !is.na(educ.pc), select=c('time','treat','did','educ.pc')))
colnames(educpc.data)<- c('time','treat','did','y')

educpc.est <- boot(educpc.data,
                  RunDiD, R=1000, 
                  strata=educpc.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

educpc.est[1]
boot.ci(educpc.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

# DD Estimates (pre-GD)

# rev.pc 

revpc.data <- data.frame(subset(funds.did, !is.na(rev.pc) & year <1929, select=c('time','treat','did','rev.pc')))
colnames(revpc.data)<- c('time','treat','did','y')

revpc.est <- boot(revpc.data,
                 RunDiD, R=1000, 
                 strata=revpc.data$did, # stratify at time*treat
                 parallel="multicore", ncpus = cores)

revpc.est[1]

boot.ci(revpc.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

# exp.pc 

exppc.data <- data.frame(subset(funds.did, !is.na(exp.pc) & year <1929, select=c('time','treat','did','exp.pc')))
colnames(exppc.data)<- c('time','treat','did','y')

exppc.est <- boot(exppc.data,
                  RunDiD, R=1000, 
                  strata=exppc.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

exppc.est[1]
boot.ci(exppc.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

# educ.pc 

educpc.data <- data.frame(subset(funds.did, !is.na(educ.pc) & year <1929, select=c('time','treat','did','educ.pc')))
colnames(educpc.data)<- c('time','treat','did','y')

educpc.est <- boot(educpc.data,
                  RunDiD, R=1000, 
                  strata=educpc.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

educpc.est[1]
boot.ci(educpc.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

## Analysis 3: Effect of HSA restriction on treated, intervention: Mar 1889
# Treated is southern public land states (not MO)
# Controls are MO, state land states

funds.did<- funds[c("state","year","rev.pc", "exp.pc", "educ.pc", "cat")]

funds.did <- funds.did[!is.na(funds.did$cat),] 

# Create var for when treatment started

funds.did$time <- 0
funds.did$time[funds.did$year >= 1889] <- 1

funds.did$treat <- NA
funds.did$treat <- ifelse(funds.did$cat=="Treated",1,0)

funds.did$did <- NA
funds.did$did <- funds.did$treat* funds.did$time

# DD estimates

# rev.pc 

revpc.data <- data.frame(subset(funds.did, !is.na(rev.pc), select=c('time','treat','did','rev.pc')))
colnames(revpc.data)<- c('time','treat','did','y')

revpc.est <- boot(revpc.data,
                  RunDiD, R=1000, 
                  strata=revpc.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

revpc.est[1]

boot.ci(revpc.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

# exp.pc 
exppc.data <- data.frame(subset(funds.did, !is.na(exp.pc), select=c('time','treat','did','exp.pc')))
colnames(exppc.data)<- c('time','treat','did','y')

exppc.est <- boot(exppc.data,
                  RunDiD, R=1000, 
                  strata=exppc.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

exppc.est[1]
boot.ci(exppc.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

# educ.pc 
educpc.data <- data.frame(subset(funds.did, !is.na(educ.pc), select=c('time','treat','did','educ.pc')))
colnames(educpc.data)<- c('time','treat','did','y')

educpc.est <- boot(educpc.data,
                  RunDiD, R=1000, 
                  strata=educpc.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

educpc.est[1]
boot.ci(educpc.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs


# DD estimates (pre-GD)

# rev.pc 

revpc.data <- data.frame(subset(funds.did, !is.na(rev.pc) & year <1929, select=c('time','treat','did','rev.pc')))
colnames(revpc.data)<- c('time','treat','did','y')

revpc.est <- boot(revpc.data,
                  RunDiD, R=1000, 
                  strata=revpc.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

revpc.est[1]

boot.ci(revpc.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

# exp.pc 
exppc.data <- data.frame(subset(funds.did, !is.na(exp.pc) & year <1929, select=c('time','treat','did','exp.pc')))
colnames(exppc.data)<- c('time','treat','did','y')

exppc.est <- boot(exppc.data,
                  RunDiD, R=1000, 
                  strata=exppc.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

exppc.est[1]
boot.ci(exppc.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

# educ.pc 
educpc.data <- data.frame(subset(funds.did, !is.na(educ.pc) & year <1929, select=c('time','treat','did','educ.pc')))
colnames(educpc.data)<- c('time','treat','did','y')

educpc.est <- boot(educpc.data,
                  RunDiD, R=1000, 
                  strata=educpc.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

educpc.est[1]
boot.ci(educpc.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

## Analysis 4: Effect of HSA restriction on treated, intervention: Mar 1889
# Treated is non-southern public land states (not MO)
# Controls are MO, state land states

funds.did<- funds[c("state","year","rev.pc", "exp.pc", "educ.pc", "cat")]

funds.did <- funds.did[!is.na(funds.did$cat),] 

# Create var for when treatment started

funds.did$time <- 0
funds.did$time[funds.did$year >= 1889] <- 1

funds.did$treat <- NA
funds.did$treat <- ifelse(funds.did$cat=="Treated",1,0)

funds.did$did <- NA
funds.did$did <- funds.did$treat* funds.did$time

# DD estimates

# rev.pc 

revpc.data <- data.frame(subset(funds.did, !is.na(rev.pc), select=c('time','treat','did','rev.pc')))
colnames(revpc.data)<- c('time','treat','did','y')

revpc.est <- boot(revpc.data,
                  RunDiD, R=1000, 
                  strata=revpc.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

revpc.est[1]

boot.ci(revpc.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

# exp.pc 
exppc.data <- data.frame(subset(funds.did, !is.na(exp.pc), select=c('time','treat','did','exp.pc')))
colnames(exppc.data)<- c('time','treat','did','y')

exppc.est <- boot(exppc.data,
                  RunDiD, R=1000, 
                  strata=exppc.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

exppc.est[1]
boot.ci(exppc.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

# educ.pc 
educpc.data <- data.frame(subset(funds.did, !is.na(educ.pc), select=c('time','treat','did','educ.pc')))
colnames(educpc.data)<- c('time','treat','did','y')

educpc.est <- boot(educpc.data,
                   RunDiD, R=1000, 
                   strata=educpc.data$did, # stratify at time*treat
                   parallel="multicore", ncpus = cores)

educpc.est[1]
boot.ci(educpc.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs


# DD estimates (pre-GD)

# rev.pc 

revpc.data <- data.frame(subset(funds.did, !is.na(rev.pc) & year <1929, select=c('time','treat','did','rev.pc')))
colnames(revpc.data)<- c('time','treat','did','y')

revpc.est <- boot(revpc.data,
                  RunDiD, R=1000, 
                  strata=revpc.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

revpc.est[1]

boot.ci(revpc.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

# exp.pc 
exppc.data <- data.frame(subset(funds.did, !is.na(exp.pc) & year <1929, select=c('time','treat','did','exp.pc')))
colnames(exppc.data)<- c('time','treat','did','y')

exppc.est <- boot(exppc.data,
                  RunDiD, R=1000, 
                  strata=exppc.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

exppc.est[1]
boot.ci(exppc.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

# educ.pc 
educpc.data <- data.frame(subset(funds.did, !is.na(educ.pc) & year <1929, select=c('time','treat','did','educ.pc')))
colnames(educpc.data)<- c('time','treat','did','y')

educpc.est <- boot(educpc.data,
                   RunDiD, R=1000, 
                   strata=educpc.data$did, # stratify at time*treat
                   parallel="multicore", ncpus = cores)

educpc.est[1]
boot.ci(educpc.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs