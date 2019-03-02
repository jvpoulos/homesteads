###################################
#  Merge homesteads & railroads       #
###################################

library(tidyr)
library(readr)

rr.inter.m.county <- as.data.frame(rr.inter.m.county)

## standardize FIPS county
rr.inter.m.county$fips <- as.numeric(as.character(rr.inter.m.county$FIPS))

census.ts.aland$fips <- census.ts.aland$state*1000 + census.ts.aland$county/10
census.ts.wide$fips <- census.ts.wide$state*1000 + census.ts.wide$county/10

rr.inter.m.county$year <- rr.inter.m.county$InOpBy

## Calc railroad access for each decennial 

decennial.homestead <- read_csv(paste0(homestead.data.directory, "decennial-homestead.csv"), 
                                col_names = c("year","year2"))

rr.sum.county <- merge(rr.inter.m.county, decennial.homestead, by="year", all.x=TRUE)

rr.decennial.county <- rr.sum.county %>% # maximum access up to decennial year
  filter(!is.na(year2)) %>%
  group_by(year2,fips) %>%
  mutate(track2= max(track2)) %>%
  arrange(fips, year2) %>% # sort
  dplyr::select(-year)

rr.decennial.county <- rr.decennial.county[!duplicated(rr.decennial.county[c("year2","fips")]),] # keep one county-decennial obs

# Make wide

rr.decennial.wide <- spread(rr.decennial.county[c("year2","state","fips","track2")], key = year2, value = track2, sep="rr")

rr.decennial.wide <- as.data.frame(rr.decennial.wide)

# merge to census datasets with homesteads data

# wide datasets (for GBR test period sequence)
homestead.rr.wide <- merge(census.ts.aland, rr.decennial.wide, by = c("fips"), all.x=TRUE)

# long datasets (for FE and pooled analyses)
rr.decennial.county$year <- rr.decennial.county$year2

homestead.rr.long <- merge(census.ts.wide, rr.decennial.county[c("fips", "year", "track2")], 
                           by = c("fips", "year"), all.x=TRUE)