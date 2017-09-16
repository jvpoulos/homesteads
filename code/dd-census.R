###################################
# DD estimates on census outcomes #
###################################
library(dplyr)
library(boot)
library(readr)

## Analysis 1: Effect of SHA on treated (southern public land states), intervention: June 1866-June 1876-March 1889
# controls are southern state land states

census.ts.did <- census.ts # use counties

icpsr<- read_csv(paste0(data.directory,"icpsr-state-code.csv"), col_names = FALSE)# get icpsr state code crosswalk
colnames(icpsr) <- c("state.code","state.name")

census.ts.did <- merge(census.ts.did, icpsr, all.x=TRUE, by.x="state", by.y="state.code")
census.ts.did <- subset(census.ts.did, !is.na(state.name)) # rm D.C. & territories

census.ts.did$cat <- NA
census.ts.did$cat[census.ts.did$state.name %in% southern.pub] <- 1
census.ts.did$cat[census.ts.did$state.name %in% southern.state] <- 0

census.ts.did <- subset(census.ts.did, !is.na(cat)) # rm non-southern state land states

# Create var for when treatment started

census.ts.did$time <- NA
census.ts.did$time <- 0
census.ts.did$time[(census.ts.did$year >= 1866)] <- 1

# land.gini 
did.land.gini <- lm(land.gini ~ cat*time, data = census.ts.did) 

summary(did.land.gini)

confint(did.land.gini)[4,]

# aland.gini 
did.aland.gini <- lm(aland.gini ~ cat*time, data = census.ts.did) 

summary(did.aland.gini)

confint(did.aland.gini)[4,]

## Analysis 3: Effect of HSA restriction on treated, intervention: Mar 1889
# Treated is non-southern public land states (not MO)
# Controls are MO, state land states

census.ts.did <- census.ts # use counties

icpsr<- read_csv(paste0(data.directory,"icpsr-state-code.csv"), col_names = FALSE)# get icpsr state code crosswalk
colnames(icpsr) <- c("state.code","state.name")

census.ts.did <- merge(census.ts.did, icpsr, all.x=TRUE, by.x="state", by.y="state.code")
census.ts.did <- subset(census.ts.did, !is.na(state.name)) # rm D.C. & territories

# Summarize by category

census.ts.did$cat <- NA
census.ts.did$cat[census.ts.did$state.name %in% setdiff(setdiff(pub.states,southern.pub), "MO")] <- 1
census.ts.did$cat[census.ts.did$state.name %in% c("MO",state.land.states)] <- 0

census.ts.did <- subset(census.ts.did, !is.na(cat)) # rm non-southern state land states

# Create var for when treatment started

census.ts.didtime <- NA
census.ts.did$time <- 0
census.ts.did$time[census.ts.did$year >= 1889] <- 1

# land.gini 
did.land.gini <- lm(land.gini ~ cat*time, data = census.ts.did) 

summary(did.land.gini)

confint(did.land.gini)[4,]

# aland.gini 
did.aland.gini <- lm(aland.gini ~ cat*time, data = census.ts.did) 

summary(did.aland.gini)

confint(did.aland.gini)[4,]

# tenancy 
did.tenancy <- lm(tenancy ~ cat*time, data = census.ts.did) 

summary(did.tenancy)

confint(did.tenancy)[4,]

# wages 

did.wages <- lm(wages ~ cat*time, data = census.ts.did) 

summary(did.wages)

confint(did.wages)[4,]

# output 

did.output <- lm(output ~ cat*time, data = census.ts.did) 

summary(did.output)

confint(did.output)[4,]