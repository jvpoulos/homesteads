###################################
#  Prepare homesteads                 #
###################################

library(tidyr)
library(readxl)
library(dplyr)

# Prepare data

census.ts.means <- census.ts %>% # Take county-year means
  filter(year %in% c(1860:1950)) %>% # compares 1860 to each post-period census year (1870-1950)
  group_by(state,county,year) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) 

census.ts.means <- census.ts.means[c("fips","state", "county", "land.gini","aland.gini","farmsize", "tenancy","wages","output","slave.share","aa.share","native.share","asian.share","white.share","water.access","rail.access","year")]

# Get state abbreviations
fips.codes <- data.frame(read_excel(paste0(data.directory,'US_FIPS_Codes.xls'), skip = 1))
fips.codes$FIPS.State <- as.numeric(fips.codes$FIPS.State)
fips.codes$FIPS.County <- as.numeric(fips.codes$FIPS.County)

fips.codes <- fips.codes[!duplicated(fips.codes$FIPS.State),][c("State","FIPS.State")]

census.ts.means <- merge(census.ts.means, fips.codes, by.x =c("state"), by.y=c("FIPS.State"))

census.ts.means$state.abb <- setNames(state.abb, state.name)[census.ts.means$State] # state abbreviations

# Reshape 

census.ts.aland <- spread(census.ts.means[c("state.abb", "state","county", "aland.gini", "year")], key = year, value = aland.gini, sep="y")
census.ts.land <- spread(census.ts.means[c("state.abb", "state","county", "land.gini","year")], key = year, value = land.gini, sep="y")

# Merge post-period merge homestead counts

census.ts.aland <- merge(census.ts.aland, patents.decennial.wide, by.x=c("state.abb","county")
                         , by.y=c("state_code","county_code"), all.x=TRUE)

census.ts.land <- merge(census.ts.land, patents.decennial.wide, by.x=c("state.abb","county")
                        , by.y=c("state_code","county_code"), all.x=TRUE)

# Wide dataset with lags

census.ts.wide <- merge(census.ts.means, patents.decennial, by.x=c("state.abb","county","year")
                                      , by.y=c("state_code","county_code","year2"), all.x=TRUE)

TLag <- function(x, n = 1L, time) { 
  index <- match(time - n, time, incomparables = NA)
  x[index]
}

census.ts.wide <- census.ts.wide %>% 
  group_by(state.abb,county) %>% 
  mutate(aland.gini.lag = TLag(aland.gini, 10, time = year),
         land.gini.lag = TLag(land.gini, 10, time = year),
         homesteads.pc.lag = TLag(homesteads.pc, 10, time = year))

census.ts.wide$id <- interaction(census.ts.wide$state, census.ts.wide$county) # make ID