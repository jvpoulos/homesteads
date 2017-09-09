###################################
# DD estimates on railroad access #
###################################
library(dplyr)

## Analysis 1: Effect of SHA on treated (southern public land states), intervention: June 1866-June 1876-March 1889
# controls are southern state land states

rr.inter.did <- rr.inter 

rr.inter.did$cat <- NA
rr.inter.did$cat[rr.inter.did$state %in% southern.pub] <- "Treated"
rr.inter.did$cat[rr.inter.did$state %in% southern.state] <- "Control"

rr.inter.did$year <- rr.inter.did$InOpBy

# Create control and treated sums
cats.rr.did <- rr.inter.did %>% 
  filter(!is.na(cat)) %>% # rm non-southern state land states
  group_by(year,cat) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)),access) 

# Create var for when treatment started

cats.rr.did$time <- NA
cats.rr.did$time <- 0
cats.rr.did$time[(cats.rr.did$year >= 1866 & cats.rr.did$year <= 1876) | (cats.rr.did$year >= 1889 & cats.rr.did$year <= 1976)] <- 1

# access 
did.access <- lm(access ~ cat*time, data = cats.rr.did[cats.rr.did$year <=1976,]) 

summary(did.access)

confint(did.access)[4,]

## Analysis 3: Effect of HSA restriction on treated, intervention: Mar 1889
# Treated is non-southern public land states (not MO)
# Controls are MO, state land states

# Summarize by category

rr.inter.did$cat <- NA
rr.inter.did$cat[rr.inter.did$state %in% setdiff(setdiff(pub.states,southern.pub), "MO")] <- "Treated"
rr.inter.did$cat[rr.inter.did$state %in% c("MO",state.land.states)] <- "Control"

# Create control and treated sums
cats.rr.did <- rr.inter.did %>% 
  filter(!is.na(cat)) %>% # rm non-southern state land states
  group_by(year,cat) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)),access) 

# Create var for when treatment started

cats.rr.did$time <- 0
cats.rr.did$time[cats.rr.did$year >= 1889 & cats.rr.did$year <= 1976] <- 1

# access 
did.access <- lm(access ~ cat*time, data = cats.rr.did[cats.rr.did$year <=1976,]) 

summary(did.access)

confint(did.access)[4,]