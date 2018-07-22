###################################
#  Merge homesteads & railroads       #
###################################

library(tidyr)
library(readr)

rr.inter.m <- as.data.frame(rr.inter.m)

## standardize FIPS county
rr.inter.m$fips <- as.numeric(as.character(rr.inter.m$FIPS))

census.ts.aland$fips <- census.ts.aland$state*1000 + census.ts.aland$county/10
census.ts.wide$fips <- census.ts.wide$state*1000 + census.ts.wide$county/10

rr.inter.m$year <- rr.inter.m$InOpBy

## Calc railroad access for each decennial 

decennial.homestead <- read_csv(paste0(homestead.data.directory, "decennial-homestead.csv"), 
                                col_names = c("year","year2"))

rr.sum <- merge(rr.inter.m, decennial.homestead, by="year", all.x=TRUE)

rr.decennial <- rr.sum %>% # mean access for next decennial year
  filter(!is.na(year2)) %>%
  group_by(year2,fips) %>%
  mutate(access.mean = mean(access)) %>%
  arrange(fips, year2) %>% # sort
  select(-year)%>%
  select(-access)

rr.decennial <- rr.decennial[!duplicated(rr.decennial[c("year2","fips")]),] # keep one county-decennial obs

# Make wide

rr.decennial.wide <- spread(rr.decennial[c("year2","state","fips","access.mean")], key = year2, value = access.mean, sep="rr")

rr.decennial.wide <- as.data.frame(rr.decennial.wide)

# merge to census datasets with homesteads data

# wide datasets (for GBR test period sequence)
homestead.rr.wide <- merge(census.ts.aland, rr.decennial.wide, by = c("fips"), all.x=TRUE)

# long datasets (for FE and pooled analyses)
rr.decennial$year <- rr.decennial$year2

homestead.rr.long <- merge(census.ts.wide, rr.decennial[c("fips", "year", "access.mean")], 
                           by = c("fips", "year"), all.x=TRUE)

# # Merge to long dataset with tax data (for FE robust)
# 
# homestead.tax.long <- merge(homestead.tax.long, homestead.rr.long[c("id", "year", "access.mean")],
#                            by = c("id", "year"), all.x=TRUE)
# 
# # Create lags
# TLag <- function(x, n = 1L, time) {
#   index <- match(time - n, time, incomparables = NA)
#   x[index]
# }
# 
# homestead.tax.long  <- homestead.tax.long %>%
#   group_by(state.abb,county) %>%
#   mutate(access.mean.lag = TLag(access.mean, 10, time = year),
#          output.lag = TLag(output, 10, time = year),
#          wages.lag = TLag(wages, 10, time = year))