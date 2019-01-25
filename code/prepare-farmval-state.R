##########################################
### Prepare average farm values        ###
##########################################

farmval <- read.csv(paste0(census.data.directory,"census-county/farmval.csv"), stringsAsFactors=FALSE)

farmval$year <- farmval$year +1000 # fix year

# Make logs
farmval$farmval <- log(farmval$faval+.Machine
                       $double.eps)

# Create lags

TLag <- function(x, n = 1L, time) { 
  index <- match(time - n, time, incomparables = NA)
  x[index]
}

farmval <- farmval %>% 
  group_by(fips) %>% 
  mutate(faval.lag = TLag(faval, 10, time = year))

# keep states
farmval <- farmval[farmval$county==0,]

# state abbr
farmval$state.abb <- setNames(state.abb, state.name)[tools::toTitleCase(tolower(farmval$name))]

# merge 
#homestead.funds.long <- merge(homestead.funds.long, farmval[c("state.abb","year","faval","faval.lag")], 
#                            by.x=c("state_code","year2"),
#                            by.y=c("state.abb","year"), all.x=TRUE)