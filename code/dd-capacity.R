###################################
# DD Estimation for comparison    #
###################################
library(dplyr)

## Analysis 1: Effect of SHA on treated (southern public land states), intervention: June 1866-June 1876-March 1889
# controls are southern state land states

cats.funds.did <- cats.funds

# Create var for when treatment started

cats.funds.did$time <- NA
cats.funds.did$time <- 0
cats.funds.did$time[(cats.funds.did$year >= 1866 & cats.funds.did$year <= 1876) | (cats.funds.did$year >= 1889 & cats.funds.did$year <= 1976)] <- 1

# rev.pc 
did.rev.pc <- lm(rev.pc ~ cat*time, data = cats.funds.did[cats.funds.did$year <=1976,]) 

summary(did.rev.pc)

confint(did.rev.pc)[4,]

# exp.pc 
did.exp.pc <- lm(exp.pc ~ cat*time, data = cats.funds.did[cats.funds.did$year <=1976,]) 

summary(did.exp.pc)

confint(did.exp.pc)[4,]

# ed.pc 
did.ed.pc <- lm(ed.pc ~ cat*time, data = cats.funds.did[cats.funds.did$year <=1976,]) 

summary(did.ed.pc)

confint(did.ed.pc)[4,]

## Analysis 3: Effect of HSA restriction on treated, intervention: Mar 1889
# Treated is non-southern public land states (not MO)
# Controls are MO, state land states

cats.funds.did <- cats.funds

# Create var for when treatment started

cats.funds.did$time <- 0
cats.funds.did$time[cats.funds.did$year >= 1889 & cats.funds.did$year <= 1976] <- 1

# rev.pc 
did.rev.pc <- lm(rev.pc ~ cat*time, data = cats.funds.did[cats.funds.did$year <=1976,]) 

summary(did.rev.pc)

confint(did.rev.pc)[4,]

# exp.pc 
did.exp.pc <- lm(exp.pc ~ cat*time, data = cats.funds.did[cats.funds.did$year <=1976,]) 

summary(did.exp.pc)

confint(did.exp.pc)[4,]

# ed.pc 
did.ed.pc <- lm(ed.pc ~ cat*time, data = cats.funds.did[cats.funds.did$year <=1976,]) 

summary(did.ed.pc)

confint(did.ed.pc)[4,]