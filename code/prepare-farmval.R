##########################################
### Prepare average farm values        ###
##########################################

farmval <- read.csv(paste0(census.data.directory,"census-county/farmval.csv"), stringsAsFactors=FALSE)

farmval$year <- farmval$year +1000 # fix year

# Make logs

farmval$farmval <- log(farmval$faval+.Machine
                        $double.eps)

# merge 
homestead.rr.long <- merge(homestead.rr.long, farmval[c("fips","year","farmval")], 
                            by=c("fips","year"), all.x=TRUE)