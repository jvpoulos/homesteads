###################################
# DD estimates on inequality /tenancy #
###################################
library(dplyr)

## Analysis 1: Effect of SHA on treated (southern public land states), intervention: June 1866-June 1876-March 1889
# controls are southern state land states

census.ts.did <- census.ts # use counties

icpsr<- read_csv(paste0(data.directory,"icpsr-state-code.csv"), col_names = FALSE)# get icpsr state code crosswalk
colnames(icpsr) <- c("state.code","state.name")

census.ts.did <- merge(census.ts.did, icpsr, all.x=TRUE, by.x="state", by.y="state.code")
census.ts.did <- subset(census.ts.did, !is.na(state.name)) # rm D.C. & territories

census.ts.did$cat <- NA
census.ts.did$cat[census.ts.did$state.name %in% southern.pub] <- "Treated"
census.ts.did$cat[census.ts.did$state.name %in% southern.state] <- "Control"

# Create control and treated sums
cats.census.did <- census.ts.did %>% 
  filter(!is.na(cat)) %>% # rm non-southern state land states
  group_by(year,cat) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)),land.gini,aland.gini,tenancy) 

# Create var for when treatment started

cats.census.did$time <- NA
cats.census.did$time <- 0
cats.census.did$time[(cats.census.did$year >= 1866 & cats.census.did$year <= 1876) | (cats.census.did$year >= 1889 & cats.census.did$year <= 1976)] <- 1

# land.gini 
did.land.gini <- lm(land.gini ~ cat*time, data = cats.census.did[cats.census.did$year <=1976,]) 

summary(did.land.gini)

confint(did.land.gini)[4,]

# aland.gini 
did.aland.gini <- lm(aland.gini ~ cat*time, data = cats.census.did[cats.census.did$year <=1976,]) 

summary(did.aland.gini)

confint(did.aland.gini)[4,]

# tenancy 
did.tenancy <- lm(tenancy ~ cat*time, data = cats.census.did[cats.census.did$year <=1976,]) 

summary(did.tenancy)

confint(did.tenancy)[4,]

## Analysis 3: Effect of HSA restriction on treated, intervention: Mar 1889
# Treated is non-southern public land states (not MO)
# Controls are MO, state land states

# Summarize by category

census.ts.did$cat <- NA
census.ts.did$cat[census.ts.did$state.name %in% setdiff(setdiff(pub.states,southern.pub), "MO")] <- "Treated"
census.ts.did$cat[census.ts.did$state.name %in% c("MO",state.land.states)] <- "Control"

# Create control and treated sums
cats.census.did <- census.ts.did %>% 
  filter(!is.na(cat)) %>% # rm non-southern state land states
  group_by(year,cat) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)),land.gini,aland.gini,tenancy) 

# Create var for when treatment started

cats.census.did$time <- 0
cats.census.did$time[cats.census.did$year >= 1889 & cats.census.did$year <= 1976] <- 1

# land.gini 
did.land.gini <- lm(land.gini ~ cat*time, data = cats.census.did[cats.census.did$year <=1976,]) 

summary(did.land.gini)

confint(did.land.gini)[4,]

# aland.gini 
did.aland.gini <- lm(aland.gini ~ cat*time, data = cats.census.did[cats.census.did$year <=1976,]) 

summary(did.aland.gini)

confint(did.aland.gini)[4,]

# tenancy 
did.tenancy <- lm(tenancy ~ cat*time, data = cats.census.did[cats.census.did$year <=1976,]) 

summary(did.tenancy)

confint(did.tenancy)[4,]