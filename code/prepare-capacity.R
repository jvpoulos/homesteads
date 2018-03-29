###################################
#  Merge homesteads & funds       #
###################################

library(dplyr)

# State/year sum 

patents.sum.state <- patents %>% # get a state-year sum of patents
  filter(authority_code==251101) %>% # only homesteads
  group_by(year,state_code) %>%
  summarise_each(funs(sum),homesteads) %>%
  arrange(state_code, year) # sort

# # *cumulative* homesteads by state
# homesteads.cumsum.state <- patents.sum.state %>% 
#   group_by(state_code) %>%
#   mutate(cumulative.homesteads = cumsum(homesteads))  %>%
#   arrange(state_code, year) %>%
#   select(state_code, year, cumulative.homesteads)

homestead.funds.long <- merge(patents.sum.state, 
                              funds, by.x = c("state_code", "year"), 
                            by.y = c("state", "year"), all.y=TRUE) 

homestead.funds.long$homesteads.pc <- log(homestead.funds.long$homesteads/homestead.funds.long$ns.pop)

homestead.funds.long$homesteads.pc[is.na(homestead.funds.long$homesteads.pc)] <- 0

# Lagged homesteads pc

homestead.funds.long <- homestead.funds.long %>% 
  group_by(state_code) %>% 
  arrange(state_code, year) %>% 
  mutate(homesteads.pc.lag=lag(homesteads.pc),
         farmsize.lag=lag(farmsize),
         farms.lag=lag(farms))

