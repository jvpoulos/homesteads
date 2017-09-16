###################################
# DD estimates on railroad track #
###################################
library(dplyr)

## Analysis 1: Effect of SHA on treated (southern public land states), intervention: June 1866-June 1876-March 1889
# controls are southern state land states

rr.inter.did <- rr.inter.m 

rr.inter.did$treat <- NA
rr.inter.did$treat[rr.inter.did$state %in% southern.pub] <- 1
rr.inter.did$treat[rr.inter.did$state %in% southern.state] <- 0

rr.inter.did <- subset(rr.inter.did, !is.na(treat)) # rm non-southern state land states

rr.inter.did$year <- rr.inter.did$InOpBy

# Create var for when treatment started

rr.inter.did$time <- NA
rr.inter.did$time <- 0
rr.inter.did$time[rr.inter.did$year >= 1866] <- 1

# track2 
did.track2 <- lm(track2 ~ treat*time, data = rr.inter.did)

summary(did.track2)

confint(did.track2)[4,]

## Analysis 3: Effect of HSA restriction on treated, intervention: Mar 1889
# Treated is non-southern public land states (not MO)
# Controls are MO, state land states

rr.inter.did <- rr.inter.m 

# Summarize by treategory

rr.inter.did$treat <- NA
rr.inter.did$treat[rr.inter.did$state %in% setdiff(setdiff(pub.states,southern.pub), "MO")] <- 1
rr.inter.did$treat[rr.inter.did$state %in% c("MO",state.land.states)] <- 0

rr.inter.did <- subset(rr.inter.did, !is.na(treat)) # rm non-southern state land states

rr.inter.did$year <- rr.inter.did$InOpBy

# Create var for when treatment started

rr.inter.did$time <- 0
rr.inter.did$time[rr.inter.did$year >= 1889] <- 1

# track2 
did.track2 <- lm(track2 ~ treat*time, data = rr.inter.did) # two-period DD

summary(did.track2)

confint(did.track2)[4,]
