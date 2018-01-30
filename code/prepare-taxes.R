###################################
#  Merge homesteads & taxes       #
###################################

library(tidyr)

# standardize FIPS county
tax1$fips <- as.numeric(tax1$FIPS.County)*10
tax2$fips <- as.numeric(tax2$FIPS.County)*10

# move off-census decennial years to match census
tax1$year[tax1$year==1922] <- 1920
tax1$year[tax1$year==1932] <- 1930
tax1$year[tax1$year==1942] <- 1940

tax2$year[tax2$year==1932] <- 1930

# make taxes wide

tax1.wide <- spread(tax1[c("state.abb", "fips", "taxpc1", "year")], key = year, value = taxpc1, sep="3x")

tax2.wide <- spread(tax2[c("state.abb", "fips", "taxpc2", "year")], key = year, value = taxpc2, sep="3x")

# merge to census datasets with homesteads data

# wide datasets (for GBR)
homestead.tax.wide <- merge(census.ts.aland, tax1.wide, by.x = c("state.abb", "county"), 
                            by.y = c("state.abb", "fips"), all.x=TRUE)

homestead.tax.wide <- merge(homestead.tax.wide, tax2.wide, by.x = c("state.abb", "county"), 
                            by.y = c("state.abb", "fips"), all.x=TRUE)

# long datasets (for FE)
homestead.tax.long <- merge(census.ts.wide, tax1[c("state.abb", "fips", "year", "taxpc1")], by.x = c("state.abb", "county", "year"), 
                            by.y = c("state.abb", "fips", "year"), all.x=TRUE)

homestead.tax.long <- merge(homestead.tax.long, tax2[c("state.abb", "fips", "year", "taxpc2")], by.x = c("state.abb", "county", "year"), 
                            by.y = c("state.abb", "fips", "year"), all.x=TRUE)