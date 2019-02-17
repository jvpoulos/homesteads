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

# Set location of files
setwd(paste0(data.directory, "patents"))

# Import glo files

patent.files <- list.files(pattern = "*_Patent.csv", recursive = TRUE)

county.files <- list.files(pattern = "*County.csv", recursive = TRUE)

patents <- do.call(rbind,lapply(patent.files,read.csv,
                                header=T,
                                sep = ",",
                                stringsAsFactors=FALSE))

counties <- do.call(rbind,lapply(county.files,read.csv,
                                       header=T,
                                       sep = ",",
                                       stringsAsFactors=FALSE))


# Merge patents with counties
counties <- counties[!duplicated(counties$accession_nr),] # keep 1 county record/accession

patents <- merge(patents, counties[c("accession_nr","county_code")], by= c("accession_nr"),all.x=TRUE)

# Fix county code
patents$county_code <- as.numeric(patents$county_code)

patents <- patents[!is.na(patents$county_code),] # rm obs w missing county codes

patents$county_code <- patents$county_code*10 # make county code consistent with ICPSR

# Codes, etc. to numeric

patents$sales <- 0
patents$sales[patents$authority_code==272002] <- 1

patents$homesteads <- 0
patents$homesteads[patents$authority_code==251101] <- 1

# Date to time

patents$date <-as.Date(patents$signature_date, format="%m/%d/%Y") # convert to date
patents$year <- as.numeric(format(patents$date ,"%Y")) # extract year

patents <- patents[!is.na(patents$year) & !patents$year>2017,] # subset to non-missing and valid dates

# Summarize by date/county/state

patents.sum <- patents %>%
  filter(state_code %in% pub.states) %>% # only public land states
  group_by(year,county_code,state_code) %>%
  summarise_each(funs(sum),sales,homesteads)

## Make per-capita measures

patents.sum$year2 <- NA
patents.sum$year2 <- signif(patents.sum$year,3) # merge by nearest decennial
patents.sum$year2[patents.sum$year<=1785] <- 1790
patents.sum$year2[patents.sum$year2<1880 & patents.sum$state_code=="AK"] <- 1880
patents.sum$year2[patents.sum$year2>=1975] <- 1983 # non-slave population (1790-1970,1983)

patents.sum <- merge(patents.sum, census.ts.state[c('year','state','ns.pop')], by.x=c('year2','state_code'), by.y=c('year','state'),all.x=TRUE)

patents.sum  <- patents.sum  %>% group_by(county_code, state_code) %>% fill(ns.pop, .direction="up") # fill missing

patents.sum$sales.pc <- NA
patents.sum$sales.pc <- patents.sum$sales/patents.sum$ns.pop

patents.sum$homesteads.pc <- NA
patents.sum$homesteads.pc <- patents.sum$homesteads/patents.sum$ns.pop