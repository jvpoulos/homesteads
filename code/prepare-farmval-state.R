##########################################
### Prepare average farm values        ###
##########################################

farmval <- read.csv(paste0(census.data.directory,"census-county/farmval.csv"), stringsAsFactors=FALSE)

farmval$year <- farmval$year +1000 # fix year

# Make logs
farmval$farmval <- log(farmval$faval+.Machine
                        $double.eps)

# Create lags
farmval <- farmval %>% 
  group_by(fips) %>% 
  mutate(farmval.lag = TLag(farmval, 10, time = year))

# keep states
farmval <- farmval[farmval$county==0,]

# state abbr
farmval$state.abb <- setNames(state.abb, state.name)[farmval$state]

# merge 
homestead.funds.long <- merge(homestead.funds.long, farmval[c("state.abb","year","farmval","farmval.lag")], 
                            by.x=c("state_code","year2"),
                            by.y=c("state.abb","year"), all.x=TRUE)
