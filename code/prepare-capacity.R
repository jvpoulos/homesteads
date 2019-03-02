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

homestead.funds.long <- merge(patents.sum.state, 
                              funds, by.x = c("state_code", "year"), 
                            by.y = c("state", "year"), all.y=TRUE) 

homestead.funds.long$homesteads.pc <- log((homestead.funds.long$homesteads/homestead.funds.long$ns.pop) +.Machine
                                          $double.eps) # log homesteads pc

homestead.funds.long$homesteads.pc[is.na(homestead.funds.long$homesteads.pc)] <- 0