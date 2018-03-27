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

# merge 
homestead.tax.long <- merge(homestead.tax.long, farmval[c("fips","year","farmval","farmval.lag")], 
                            by=c("fips","year"), all.x=TRUE)

homestead.tax.long <- merge(homestead.tax.long, farmval[c("fips","year","farmval")], 
                            by=c("fips","year"), all.x=TRUE)