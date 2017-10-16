###################################
# DD estimates on census outcomes #
###################################
library(dplyr)
library(boot)
library(readr)

source(paste0(code.directory,"RunDid.R"))

## Analysis 0: Effect of HSA on treated (western public land states), intervention: May 1862
# controls are non-southern state land states

census.ts.did <- census.ts # use counties

icpsr<- read_csv(paste0(data.directory,"icpsr-state-code.csv"), col_names = FALSE)# get icpsr state code crosswalk
colnames(icpsr) <- c("state.code","state.name")

census.ts.did <- merge(census.ts.did, icpsr, all.x=TRUE, by.x="state", by.y="state.code")
census.ts.did <- subset(census.ts.did, !is.na(state.name)) # rm D.C. & territories

census.ts.did$treat <- NA
census.ts.did$treat[census.ts.did$state.name %in% setdiff(pub.states,southern.pub)] <- 1
census.ts.did$treat[census.ts.did$state.name %in% setdiff(state.land.states,southern.state)] <- 0

census.ts.did <- subset(census.ts.did, !is.na(treat)) # rm non-southern state land states

# Create var for when treatment started

census.ts.did$time <- NA
census.ts.did$time <- 0
census.ts.did$time[(census.ts.did$year >= 1862)] <- 1

census.ts.did$did <- NA
census.ts.did$did <- census.ts.did$treat* census.ts.did$time

# inequality

ineq.data <- data.frame(subset(census.ts.did, !is.na(aland.gini), select=c('time','treat','did','aland.gini')))
colnames(ineq.data)<- c('time','treat','did','y')

ineq.est <- boot(ineq.data,
                 RunDiD, R=1000, 
                 strata=ineq.data$did, # stratify at time*treat
                 parallel="multicore", ncpus = cores)

ineq.est[1]

boot.ci(ineq.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

nrow(ineq.data) #report N

# LM sanity check
did.aland.gini <- lm(aland.gini ~ treat*time, data = census.ts.did) 

summary(did.aland.gini)

confint(did.aland.gini)[4,]

## Analysis 1: Effect of SHA on treated (southern public land states), intervention: June 1866-June 1876-March 1889
# controls are southern state land states

census.ts.did <- census.ts # use counties

icpsr<- read_csv(paste0(data.directory,"icpsr-state-code.csv"), col_names = FALSE)# get icpsr state code crosswalk
colnames(icpsr) <- c("state.code","state.name")

census.ts.did <- merge(census.ts.did, icpsr, all.x=TRUE, by.x="state", by.y="state.code")
census.ts.did <- subset(census.ts.did, !is.na(state.name)) # rm D.C. & territories

census.ts.did$treat <- NA
census.ts.did$treat[census.ts.did$state.name %in% southern.pub] <- 1
census.ts.did$treat[census.ts.did$state.name %in% southern.state] <- 0

census.ts.did <- subset(census.ts.did, !is.na(treat)) # rm non-southern state land states

# Create var for when treatment started

census.ts.did$time <- NA
census.ts.did$time <- 0
census.ts.did$time[(census.ts.did$year >= 1866)] <- 1

census.ts.did$did <- NA
census.ts.did$did <- census.ts.did$treat* census.ts.did$time

# inequality

ineq.data <- data.frame(subset(census.ts.did, !is.na(aland.gini), select=c('time','treat','did','aland.gini')))
colnames(ineq.data)<- c('time','treat','did','y')

ineq.est <- boot(ineq.data,
                  RunDiD, R=1000, 
                  strata=ineq.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

ineq.est[1]

boot.ci(ineq.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

nrow(ineq.data) #report N

# LM sanity check
did.aland.gini <- lm(aland.gini ~ treat*time, data = census.ts.did) 

summary(did.aland.gini)

confint(did.aland.gini)[4,]

## Analysis 3: Effect of HSA restriction on treated, intervention: Mar 1889
# Treated is southern public land states (not MO)
# Controls are MO, state land states

census.ts.did <- census.ts # use counties

icpsr<- read_csv(paste0(data.directory,"icpsr-state-code.csv"), col_names = FALSE)# get icpsr state code crosswalk
colnames(icpsr) <- c("state.code","state.name")

census.ts.did <- merge(census.ts.did, icpsr, all.x=TRUE, by.x="state", by.y="state.code")
census.ts.did <- subset(census.ts.did, !is.na(state.name)) # rm D.C. & territories

census.ts.did$treat <- NA
census.ts.did$treat[census.ts.did$state.name %in% southern.pub] <- 1
census.ts.did$treat[census.ts.did$state.name %in% c("MO",state.land.states)] <- 0

census.ts.did <- subset(census.ts.did, !is.na(treat)) # rm non-southern state land states

# Create var for when treatment started

census.ts.didtime <- NA
census.ts.did$time <- 0
census.ts.did$time[census.ts.did$year >= 1889] <- 1

census.ts.did$did <- NA
census.ts.did$did <- census.ts.did$treat* census.ts.did$time

# inequality 

ineq.data <- data.frame(subset(census.ts.did, !is.na(aland.gini), select=c('time','treat','did','aland.gini')))
colnames(ineq.data)<- c('time','treat','did','y')

ineq.est <- boot(ineq.data,
                 RunDiD, R=1000, 
                 strata=ineq.data$did, # stratify at time*treat
                 parallel="multicore", ncpus = cores)

ineq.est[1]

boot.ci(ineq.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

nrow(ineq.data) #report N

# tenancy 

tenancy.data <- data.frame(subset(census.ts.did, !is.na(tenancy) & census.ts.did$year<=1950, select=c('time','treat','did','tenancy')))
colnames(tenancy.data)<- c('time','treat','did','y')

tenancy.est <- boot(tenancy.data,
                    RunDiD, R=1000, 
                    strata=tenancy.data$did, # stratify at time*treat
                    parallel="multicore", ncpus = cores)

tenancy.est[1]

boot.ci(tenancy.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

nrow(tenancy.data) #report N

# wages 

wages.data <- data.frame(subset(census.ts.did, !is.na(wages), select=c('time','treat','did','wages')))
colnames(wages.data)<- c('time','treat','did','y')

wages.est <- boot(wages.data,
                  RunDiD, R=1000, 
                  strata=wages.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

wages.est[1]

boot.ci(wages.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

nrow(wages.data) #report N

# output 

output.data <- data.frame(subset(census.ts.did, !is.na(output), select=c('time','treat','did','output')))
colnames(output.data)<- c('time','treat','did','y')

output.est <- boot(output.data,
                   RunDiD, R=1000, 
                   strata=output.data$did, # stratify at time*treat
                   parallel="multicore", ncpus = cores)

output.est[1]

boot.ci(output.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

nrow(output.data) #report N


## Analysis 4: Effect of HSA restriction on treated, intervention: Mar 1889
# Treated is western public land states (not MO)
# Controls are MO, state land states

census.ts.did <- census.ts # use counties

icpsr<- read_csv(paste0(data.directory,"icpsr-state-code.csv"), col_names = FALSE)# get icpsr state code crosswalk
colnames(icpsr) <- c("state.code","state.name")

census.ts.did <- merge(census.ts.did, icpsr, all.x=TRUE, by.x="state", by.y="state.code")
census.ts.did <- subset(census.ts.did, !is.na(state.name)) # rm D.C. & territories

census.ts.did$treat <- NA
census.ts.did$treat[census.ts.did$state.name %in% setdiff(setdiff(pub.states,southern.pub), "MO")] <- 1
census.ts.did$treat[census.ts.did$state.name %in% c("MO",state.land.states)] <- 0

census.ts.did <- subset(census.ts.did, !is.na(treat)) # rm non-southern state land states

# Create var for when treatment started

census.ts.didtime <- NA
census.ts.did$time <- 0
census.ts.did$time[census.ts.did$year >= 1889] <- 1

census.ts.did$did <- NA
census.ts.did$did <- census.ts.did$treat* census.ts.did$time

# inequality 

ineq.data <- data.frame(subset(census.ts.did, !is.na(aland.gini), select=c('time','treat','did','aland.gini')))
colnames(ineq.data)<- c('time','treat','did','y')

ineq.est <- boot(ineq.data,
                 RunDiD, R=1000, 
                 strata=ineq.data$did, # stratify at time*treat
                 parallel="multicore", ncpus = cores)

ineq.est[1]

boot.ci(ineq.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

nrow(ineq.data) #report N

# tenancy 

tenancy.data <- data.frame(subset(census.ts.did, !is.na(tenancy) & census.ts.did$year<=1950, select=c('time','treat','did','tenancy')))
colnames(tenancy.data)<- c('time','treat','did','y')

tenancy.est <- boot(tenancy.data,
                 RunDiD, R=1000, 
                 strata=tenancy.data$did, # stratify at time*treat
                 parallel="multicore", ncpus = cores)

tenancy.est[1]

boot.ci(tenancy.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

nrow(tenancy.data) #report N

# wages 

wages.data <- data.frame(subset(census.ts.did, !is.na(wages), select=c('time','treat','did','wages')))
colnames(wages.data)<- c('time','treat','did','y')

wages.est <- boot(wages.data,
                    RunDiD, R=1000, 
                    strata=wages.data$did, # stratify at time*treat
                    parallel="multicore", ncpus = cores)

wages.est[1]

boot.ci(wages.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

nrow(wages.data) #report N

# output 

output.data <- data.frame(subset(census.ts.did, !is.na(output), select=c('time','treat','did','output')))
colnames(output.data)<- c('time','treat','did','y')

output.est <- boot(output.data,
                  RunDiD, R=1000, 
                  strata=output.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

output.est[1]

boot.ci(output.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

nrow(output.data) #report N