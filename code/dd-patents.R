###################################
# DD Estimation for comparison    #
###################################
library(dplr)

## Analysis 1: Effect of SHA on treated (south) sales, intervention: June 1866-June 1876; period: - Feb 1889 (restrictions imposted March 1889)

# controls are non-south

# Summarize by date/county/state

patents.sum.did <- patents %>%
  group_by(date,county_code,state_code) %>%
  summarise_each(funs(sum),sales)

# Category
patents.sum.did$cat <- ifelse(patents.sum.did$state_code %in% southern.pub, "Treated", "Control") 

cats.sums.did <- patents.sum.did %>% 
  group_by(date,cat) %>% 
  summarise_each(funs(sum),sales) 

# Create var for when treatment started

cats.sums.did$time <- 0
cats.sums.did$time[cats.sums.did$date >= "Jun 1866"] <- 1

# Sales 
did.sales <- lm(sales ~ cat*time, data = cats.sums.did[cats.sums.did$date >= "Nov 1803",]) # subset to when treated counties exist

summary(did.sales)

confint(did.sales)[4,]