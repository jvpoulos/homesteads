###################################
# DD Estimation for comparison    #
###################################
library(dplyr)

source(paste0(code.directory,"RunDid.R"))

## Analysis 3: Effect of HSA restriction on treated, intervention: Mar 1889
# Treated is non-southern public land states (not MO)
# Controls are MO, state land states

taxpc.did<- taxpc

taxpc.did$treat <- NA
taxpc.did$treat[taxpc.did$state.abb %in% setdiff(setdiff(pub.states,southern.pub), "MO")] <- 1
taxpc.did$treat[taxpc.did$state.abb %in% c("MO",state.land.states)] <- 0

taxpc.did <- taxpc.did[!is.na(taxpc.did$treat),] # rm non-southern state land states

# Create var for when treatment started

taxpc.did$time <- 0
taxpc.did$time[taxpc.did$year >= 1889] <- 1

taxpc.did$did <- NA
taxpc.did$did <- taxpc.did$treat* taxpc.did$time

# DD estimates

# taxpc1 

taxpc1.data <- data.frame(subset(taxpc.did, !is.na(taxpc1), select=c('time','treat','did','taxpc1')))
colnames(taxpc1.data)<- c('time','treat','did','y')

taxpc1.est <- boot(taxpc1.data,
                  RunDiD, R=1000, 
                  strata=taxpc1.data$did, # stratify at time*treat
                  parallel="multicore", ncpus = cores)

taxpc1.est[1]

boot.ci(taxpc1.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

nrow(taxpc1.data) # report N

# LM sanity check
did.taxpc1 <- lm(taxpc1 ~ treat*time, data = taxpc.did) 

summary(did.taxpc1)

confint(did.taxpc1)[4,]

# taxpc2 

taxpc2.data <- data.frame(subset(taxpc.did, !is.na(taxpc2), select=c('time','treat','did','taxpc2')))
colnames(taxpc2.data)<- c('time','treat','did','y')

taxpc2.est <- boot(taxpc2.data,
                   RunDiD, R=1000, 
                   strata=taxpc2.data$did, # stratify at time*treat
                   parallel="multicore", ncpus = cores)

taxpc2.est[1]

boot.ci(taxpc2.est, conf=0.95, type=c("basic")) # nonparametric bootstrap CIs

nrow(taxpc2.data) # report N

# LM sanity check
did.taxpc2 <- lm(taxpc2 ~ treat*time, data = taxpc.did) 

summary(did.taxpc2)

confint(did.taxpc2)[4,]