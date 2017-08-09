###################################
# DD Estimation for comparison    #
###################################
library(dplr)

## Analysis 1: Effect of SHA on treated (south) sales, intervention: June 1866-June 1876; period: - Feb 1889 (restrictions imposted March 1889)

# controls are non-south

# Summarize by date/county/state

patents.sum.did <- patents %>%
  group_by(date,county_code,state_code) %>%
  summarise_each(funs(sum),sales,homesteads)

# Category
patents.sum.did$cat <- ifelse(patents.sum.did$state_code %in% southern.pub, "Treated", "Control") 

cats.sums.did <- patents.sum.did %>% 
  group_by(date,cat) %>% 
  summarise_each(funs(sum),sales) 

# Create var for when treatment started

cats.sums.did$time <- 0
cats.sums.did$time[cats.sums.did$date >= "Jun 1866" & cats.sums.did$date <= "Feb 1889"] <- 1

# Sales 
did.sales <- lm(sales ~ cat*time, data = cats.sums.did[cats.sums.did$date <= "Feb 1889",]) 

summary(did.sales)

confint(did.sales)[4,]

## Analysis 3: Effect on treated (south), intervention: Mar 1889; period: Jul 1876 - Oct 1976 (HSA repeal)

# Controls are MO counties

# Category
patents.sum.did$cat <- ifelse(patents.sum.did$state_code=="MO", "Control", "Treated") 

cats.sums.did <- patents.sum.did %>% 
  filter(state_code %in% c(southern.pub,"MO")) %>% # treated is south
  group_by(date,cat) %>% 
  summarise_each(funs(sum),sales,homesteads) 

# Create var for when treatment started

cats.sums.did$time <- 0
cats.sums.did$time[cats.sums.did$date >= "Mar 1889" & cats.sums.did$date <= "Oct 1976"] <- 1

# Sales 
did.sales <- lm(sales ~ cat*time, data = cats.sums.did[cats.sums.did$date <= "Oct 1976",]) 

summary(did.sales)

confint(did.sales)[4,]

# homesteads 
did.homesteads <- lm(homesteads ~ cat*time, data = cats.sums.did[cats.sums.did$date <= "Oct 1976",]) 

summary(did.homesteads)

confint(did.homesteads)[4,]