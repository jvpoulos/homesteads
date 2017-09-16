###################################
# DD Estimation for comparison    #
###################################
library(dplyr)

## Analysis 1: Effect of SHA on treated (southern public land states), intervention: June 1866-June 1876-March 1889
# controls are southern state land states

funds.did<- funds[c("state","year","rev.pc", "exp.pc", "cat")]

funds.did <- funds.did[!is.na(funds.did$cat),] # rm non-southern state land states

# Create var for when treatment started

funds.did$time <- NA
funds.did$time <- 0
funds.did$time[(funds.did$year >= 1866)] <- 1

# DD Estimates (pre-GD)

# rev.pc 
did.rev.pc <- lm(rev.pc ~ cat*time, data = funds.did[funds.did$year <=1915,]) 

summary(did.rev.pc)

confint(did.rev.pc)[4,]

# exp.pc 
did.exp.pc <- lm(exp.pc ~ cat*time, data = funds.did[funds.did$year <=1915,]) 

summary(did.exp.pc)

confint(did.exp.pc)[4,]

## Analysis 3: Effect of HSA restriction on treated, intervention: Mar 1889
# Treated is non-southern public land states (not MO)
# Controls are MO, state land states

funds.did<- funds[c("state","year","rev.pc", "exp.pc", "cat")]

funds.did <- funds.did[!is.na(funds.did$cat),] # rm non-southern state land states

# Create var for when treatment started

funds.did$time <- 0
funds.did$time[funds.did$year >= 1889] <- 1

# DD estimates (pre-GD)

# rev.pc 
did.rev.pc <- lm(rev.pc ~ cat*time, data = funds.did[funds.did$year <=1915,]) 

summary(did.rev.pc)

confint(did.rev.pc)[4,]

# exp.pc 
did.exp.pc <- lm(exp.pc ~ cat*time, data = funds.did[funds.did$year <=1915,]) 

summary(did.exp.pc)

confint(did.exp.pc)[4,]