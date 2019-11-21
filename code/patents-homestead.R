###################################
# Prepare patents data            #
###################################

require(data.table)
require(reshape2)
require(stringr)
library(dplyr)
library(weights)
library(caret)
library(zoo)
library(tidyr)
library(readr)

# # Set location of files
# setwd(paste0(data.directory, "patents"))
# 
# # Import glo files
# 
# patent.files <- list.files(pattern = "*_Patent.csv", recursive = TRUE)
# 
# county.files <- list.files(pattern = "*County.csv", recursive = TRUE)
# 
# patents <- do.call(rbind,lapply(patent.files,read.csv,
#                                 header=T,
#                                 sep = ",",
#                                 stringsAsFactors=FALSE))
# 
# counties <- do.call(rbind,lapply(county.files,read.csv,
#                                        header=T,
#                                        sep = ",",
#                                        stringsAsFactors=FALSE))
# 
# 
# # Merge patents with counties
# counties <- counties[!duplicated(counties$accession_nr),] # keep 1 county record/accession
# 
# patents <- merge(patents, counties[c("accession_nr","county_code")], by= c("accession_nr"),all.x=TRUE)
# 
# # Fix county code
# patents$county_code <- as.numeric(patents$county_code)
# 
# patents <- patents[!is.na(patents$county_code),] # rm obs w missing county codes
# 
# patents$county_code <- patents$county_code*10 # make county code consistent with ICPSR
# 
# # Codes, etc. to numeric
# 
# patents$sales <- 0
# patents$sales[patents$authority_code==272002] <- 1
# 
# patents$homesteads <- 0
# patents$homesteads[patents$authority_code==251101] <- 1
# 
# # Date to time
# 
# patents$date <-as.Date(patents$signature_date, format="%m/%d/%Y") # convert to date
# patents$year <- as.numeric(format(patents$date ,"%Y")) # extract year
# 
# patents <- patents[!is.na(patents$year) & !patents$year>2017,] # subset to non-missing and valid dates
# 
# # Save patents
# saveRDS(patents, paste0(data.directory, "patents/patents.rds"))

#############################################

patents <- readRDS(paste0(data.directory, "patents/patents.rds"))

# Summarize by date/county/state

patents.sum <- patents %>% # get a county-year sum of patents
  filter(authority_code==251101) %>% # only homesteads
  filter(year %in% c(1862:1950)) %>% 
  group_by(year,county_code,state_code) %>%
  summarise_each(funs(sum),homesteads) %>%
  arrange(state_code, county_code,year) # sort

## Calc per capita homesteads for each decennial 

decennial.homestead <- read_csv(paste0(data.directory,"decennial-homestead.csv"), 
                                col_names = c("year","year2"))

patents.sum <- merge(patents.sum, decennial.homestead, by="year", all.x=TRUE)

patents.sum$year2[patents.sum$year2==1860 & patents.sum$state_code=="WY"] <- 1870  # WY earliest 1870
patents.sum$year2[patents.sum$year2 %in% c(1870,1880) & patents.sum$state_code %in% c("ND","SD")] <- 1890  # SD/ND earlies 1890

patents.sum <- merge(patents.sum, census.ts.state[c('year','state','ns.pop')], by.x=c('year2','state_code'), by.y=c('year','state'),all.x=TRUE)

patents.decennial <- patents.sum %>% # sum homesteads for next decennial year
  group_by(county_code,state_code,year2) %>%
  mutate(homesteads.sum = sum(homesteads)) %>%
  arrange(state_code, county_code, year2) %>% # sort
  dplyr::select(-year)%>%
  dplyr::select(-homesteads)

patents.decennial <- patents.decennial[!duplicated(patents.decennial[c("year2","county_code","state_code")]),] # keep one county-decennial obs

patents.decennial$homesteads.pc <- (patents.decennial$homesteads.sum/patents.decennial$ns.pop)

# Make wide

patents.decennial.wide <- spread(patents.decennial[c("year2","state_code","county_code","homesteads.pc")], key = year2, value = homesteads.pc, sep="x")

patents.decennial.wide <- as.data.frame(patents.decennial.wide)