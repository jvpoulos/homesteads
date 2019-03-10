###################################
#  Merge homesteads & funds       #
###################################

library(dplyr)

# State/year sum 

# Create access measures - state level

# RR
rr.inter.m.state.dd <- rr.inter.m %>% 
  filter(!is.na(state)) %>% # rm DC & territories
  arrange(state,InOpBy)  %>% # sort by state/year
  group_by(state) %>% 
  mutate(cumulative.track = cumsum(track), # cumulative sum of track miles
         track2 = cumulative.track/AREA_SQMI) %>% # cumulative track miles per square mile
  dplyr::select(InOpBy,FIPS,cumulative.track,track2,state,AREA_SQMI)

rr.inter.m.state.dd <- rr.inter.m.state.dd[!duplicated(rr.inter.m.state.dd[c("InOpBy","state")]),] # keep one state-year obs

rr.inter.m.state.dd <- as.data.frame(rr.inter.m.state.dd)

rr.inter.m.state.dd$year <- rr.inter.m.state.dd$InOpBy

## patents
patents.sum.state <- patents %>% # get a state-year sum of patents
  filter(authority_code==251101) %>% # only homesteads
  group_by(year,state_code) %>%
  summarise_each(funs(sum),homesteads) %>%
  arrange(state_code, year) # sort

homestead.funds.long <- merge(patents.sum.state, 
                              funds, by.x = c("state_code", "year"), 
                            by.y = c("state", "year"), all.y=TRUE) 

homestead.funds.long <- merge(rr.inter.m.state.dd, 
                              homestead.funds.long, by.x = c("state", "year"), 
                              by.y = c("state_code", "year"), all.y=TRUE) 

homestead.funds.long$homesteads.pc <- log((homestead.funds.long$homesteads/homestead.funds.long$ns.pop) +.Machine
                                          $double.eps) # log homesteads pc

homestead.funds.long$homesteads.pc[is.na(homestead.funds.long$homesteads.pc)] <- 0