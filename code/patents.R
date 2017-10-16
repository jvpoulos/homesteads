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

analysis <- 0

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
# # Save workspace
# save.image(paste0(data.directory, "patents/patents.RData"))

#############################################

data.directory <-"~/Dropbox/github/land-reform/data/"
load(paste0(data.directory, "patents/patents.RData"))

# Summarize by date/county/state

patents.sum <- patents %>%
  filter(state_code %in% pub.states) %>% # only public land states
  group_by(year,county_code,state_code) %>%
  summarise_each(funs(sum),sales,homesteads)

## Make log per-capita measures

patents.sum$year2 <- NA
patents.sum$year2 <- signif(patents.sum$year,3) # merge by nearest decennial
patents.sum$year2[patents.sum$year<=1785] <- 1790 
patents.sum$year2[patents.sum$year2<1880 & patents.sum$state_code=="AK"] <- 1880
patents.sum$year2[patents.sum$year2>=1975] <- 1983 # non-slave population (1790-1970,1983)

patents.sum <- merge(patents.sum, census.ts.state[c('year','state','ns.pop')], by.x=c('year2','state_code'), by.y=c('year','state'),all.x=TRUE)

patents.sum  <- patents.sum  %>% group_by(county_code, state_code) %>% fill(ns.pop, .direction="up") # fill missing

patents.sum$sales.pc <- NA
patents.sum$sales.pc <- log(patents.sum$sales/patents.sum$ns.pop+ .Machine$double.eps)

patents.sum$homesteads.pc <- NA
patents.sum$homesteads.pc <- log(patents.sum$homesteads/patents.sum$ns.pop+ .Machine$double.eps)


if(analysis==0){
  
  ## Analysis 0: Effect of HSA on treated (western public land states), intervention: May 1862
  # controls are *southern public land states*
  
  # Summarize by category
  
  patents.sum$cat <- NA
  patents.sum$cat[patents.sum$state_code %in% setdiff(pub.states,southern.pub)] <- "Treated"
  patents.sum$cat[patents.sum$state_code %in% southern.pub] <- "Control"
  
  # Create control and treated sums
  cats.patents.sum <- patents.sum %>% 
    filter(!is.na(cat)) %>% 
    group_by(year,cat) %>% 
    summarise_each(funs(mean(., na.rm = TRUE))) %>%
    select(-state_code) %>%
    select(-county_code)
  
  cats.patents.sum.r <- reshape(data.frame(cats.patents.sum), idvar = "year", timevar = "cat", direction = "wide")
  
  patents.sum.control <- patents.sum[!is.na(patents.sum$cat) & patents.sum$cat=="Control",][c("county_code", "state_code","year","homesteads.pc","sales.pc")] # discard treated since we have treated time-series
  patents.sum.control$id <- interaction(patents.sum.control$county_code, patents.sum.control$state_code)
  
  homesteads.pc <- reshape(data.frame(patents.sum.control[c("year","homesteads.pc","homesteads.pc","id")]), idvar = "year", timevar = "id", direction = "wide")
  sales.pc <- reshape(data.frame(patents.sum.control[c("year","homesteads.pc","sales.pc","id")]), idvar = "year", timevar = "id", direction = "wide")

  #Labels
  
  homesteads.pc.y <- cats.patents.sum.r[c("year", "homesteads.pc.Treated")]
  homesteads.pc.y <- homesteads.pc.y[!is.na(homesteads.pc.y$homesteads.pc.Treated),]
  
  sales.pc.y <- cats.patents.sum.r[c("year", "sales.pc.Treated")]
  sales.pc.y <- sales.pc.y[!is.na(sales.pc.y$sales.pc.Treated),]
  
  # Splits
  
  homesteads.pc.years <- intersect(homesteads.pc$year,homesteads.pc.y$year) # common homesteads.pc years in treated and control
  
  homesteads.pc.x.train <- homesteads.pc[homesteads.pc$year %in% homesteads.pc.years & homesteads.pc$year < 1859,]
  homesteads.pc.x.val <- homesteads.pc[homesteads.pc$year %in% homesteads.pc.years & (homesteads.pc$year >= 1859 & homesteads.pc$year < 1862),] # 1859, 1860, 1861 for validation
  homesteads.pc.x.test <- homesteads.pc[homesteads.pc$year %in% homesteads.pc.years & homesteads.pc$year >= 1862,]
  
  homesteads.pc.y.train <- homesteads.pc.y[homesteads.pc.y$year %in% homesteads.pc.years & homesteads.pc.y$year < 1859,]
  homesteads.pc.y.val <- homesteads.pc.y[homesteads.pc.y$year %in% homesteads.pc.years & (homesteads.pc.y$year >= 1859 & homesteads.pc.y$year < 1862),]
  homesteads.pc.y.test <- homesteads.pc.y[homesteads.pc.y$year %in% homesteads.pc.years &homesteads.pc.y$year >= 1862,]
  
  sales.pc.years <- intersect(sales.pc$year,sales.pc.y$year) # common sales.pc years in treated and control
  
  sales.pc.x.train <- sales.pc[sales.pc$year %in% sales.pc.years & sales.pc$year < 1859,]
  sales.pc.x.val <- sales.pc[sales.pc$year %in% sales.pc.years & (sales.pc$year >= 1859  & sales.pc$year < 1862),]
  sales.pc.x.test <- sales.pc[sales.pc$year %in% sales.pc.years & sales.pc$year >= 1862,]
  
  sales.pc.y.train <- sales.pc.y[sales.pc.y$year %in% sales.pc.years & sales.pc.y$year < 1859,]
  sales.pc.y.val <- sales.pc.y[sales.pc.y$year %in% sales.pc.years & (sales.pc.y$year >= 1859 & sales.pc.y$year < 1862),]
  sales.pc.y.test <- sales.pc.y[sales.pc.y$year %in% sales.pc.years &sales.pc.y$year >= 1862,]
  
  # Preprocess
  homesteads.pc.x.train <- data.frame(sapply(homesteads.pc.x.train, as.numeric))
  homesteads.pc.x.train[is.na(homesteads.pc.x.train)] <- 0 # fill NA with 0 before scale
  homesteads.pc.pre.train <- preProcess(homesteads.pc.x.train[!colnames(homesteads.pc.x.train) %in% c("year")], method = c("center", "scale","medianImpute"))
  homesteads.pc.x.train[!colnames(homesteads.pc.x.train) %in% c("year")] <- predict(homesteads.pc.pre.train, homesteads.pc.x.train[!colnames(homesteads.pc.x.train) %in% c("year")] )
  
  homesteads.pc.x.val <- data.frame(sapply(homesteads.pc.x.val, as.numeric))
  homesteads.pc.x.val[!colnames(homesteads.pc.x.val) %in% c("year")] <- predict(homesteads.pc.pre.train, homesteads.pc.x.val[!colnames(homesteads.pc.x.val) %in% c("year")] ) # use training values for val set 
  
  homesteads.pc.x.test <- data.frame(sapply(homesteads.pc.x.test, as.numeric))
  homesteads.pc.x.test[!colnames(homesteads.pc.x.test) %in% c("year")] <- predict(homesteads.pc.pre.train, homesteads.pc.x.test[!colnames(homesteads.pc.x.test) %in% c("year")] ) # use training values for test set 
  
  sales.pc.x.train <- data.frame(sapply(sales.pc.x.train, as.numeric))
  sales.pc.x.train[is.na(sales.pc.x.train)] <- 0 # fill NA with 0 before scale
  sales.pc.pre.train <- preProcess(sales.pc.x.train[!colnames(sales.pc.x.train) %in% c("year")], method = c("center", "scale","medianImpute"))
  sales.pc.x.train[!colnames(sales.pc.x.train) %in% c("year")] <- predict(sales.pc.pre.train, sales.pc.x.train[!colnames(sales.pc.x.train) %in% c("year")] )
  
  sales.pc.x.val <- data.frame(sapply(sales.pc.x.val, as.numeric))
  sales.pc.x.val[!colnames(sales.pc.x.val) %in% c("year")] <- predict(sales.pc.pre.train, sales.pc.x.val[!colnames(sales.pc.x.val) %in% c("year")] ) # use training values for val set 
  
  sales.pc.x.test <- data.frame(sapply(sales.pc.x.test, as.numeric))
  sales.pc.x.test[!colnames(sales.pc.x.test) %in% c("year")] <- predict(sales.pc.pre.train, sales.pc.x.test[!colnames(sales.pc.x.test) %in% c("year")] ) # use training values for test set 
  
  # Export each as csv (labels, features)
  data.directory <- "~/Dropbox/github/drnns-prediction/data/patents/analysis-01/treated/"
  
  write.csv(homesteads.pc.x.train[!colnames(homesteads.pc.x.train) %in% c("year")], paste0(data.directory,"homesteads-x-train.csv"), row.names=FALSE) 
  write.csv(homesteads.pc.x.val[!colnames(homesteads.pc.x.val) %in% c("year")] , paste0(data.directory,"homesteads-x-val.csv"), row.names=FALSE) 
  write.csv(homesteads.pc.x.test[!colnames(homesteads.pc.x.test) %in% c("year")] , paste0(data.directory,"homesteads-x-test.csv"), row.names=FALSE) 
  write.csv(homesteads.pc.y.train[!colnames(homesteads.pc.y.train) %in% c("year")], paste0(data.directory,"homesteads-y-train.csv"), row.names=FALSE) 
  write.csv(homesteads.pc.y.val[!colnames(homesteads.pc.y.val) %in% c("year")], paste0(data.directory,"homesteads-y-val.csv"), row.names=FALSE) 
  write.csv(homesteads.pc.y.test[!colnames(homesteads.pc.y.test) %in% c("year")], paste0(data.directory,"homesteads-y-test.csv"), row.names=FALSE) 
  
  write.csv(sales.pc.x.train[!colnames(sales.pc.x.train) %in% c("year")], paste0(data.directory,"sales-x-train.csv"), row.names=FALSE) 
  write.csv(sales.pc.x.val[!colnames(sales.pc.x.val) %in% c("year")] , paste0(data.directory,"sales-x-val.csv"), row.names=FALSE) 
  write.csv(sales.pc.x.test[!colnames(sales.pc.x.test) %in% c("year")] , paste0(data.directory,"sales-x-test.csv"), row.names=FALSE) 
  write.csv(sales.pc.y.train[!colnames(sales.pc.y.train) %in% c("year")], paste0(data.directory,"sales-y-train.csv"), row.names=FALSE) 
  write.csv(sales.pc.y.val[!colnames(sales.pc.y.val) %in% c("year")], paste0(data.directory,"sales-y-val.csv"), row.names=FALSE) 
  write.csv(sales.pc.y.test[!colnames(sales.pc.y.test) %in% c("year")], paste0(data.directory,"sales-y-test.csv"), row.names=FALSE) 
  }

if(analysis==1){
  ## Analysis 1: Effect of SHA on treated (southern public land states), intervention: June 1866-June 1876-March 1889
  # controls are *western public land states*
  
  # Summarize by category
  
  patents.sum$cat <- NA
  patents.sum$cat[patents.sum$state_code %in% southern.pub] <- "Treated"
  patents.sum$cat[patents.sum$state_code %in% setdiff(pub.states, southern.pub)] <- "Control"
  
  # Create control and treated sums
  cats.patents.sum <- patents.sum %>% 
    filter(!is.na(cat)) %>% 
    group_by(year,cat) %>% 
    summarise_each(funs(mean(., na.rm = TRUE))) %>%
    select(-state_code) %>%
    select(-county_code)
  
  cats.patents.sum.r <- reshape(data.frame(cats.patents.sum), idvar = "year", timevar = "cat", direction = "wide")
  
  patents.sum.control <- patents.sum[!is.na(patents.sum$cat) & patents.sum$cat=="Control",][c("county_code", "state_code","year","homesteads.pc","sales.pc")] # discard treated since we have treated time-series
  patents.sum.control$id <- interaction(patents.sum.control$county_code, patents.sum.control$state_code)
  
  homesteads.pc <- reshape(data.frame(patents.sum.control[c("year","homesteads.pc","homesteads.pc","id")]), idvar = "year", timevar = "id", direction = "wide")
  sales.pc <- reshape(data.frame(patents.sum.control[c("year","homesteads.pc","sales.pc","id")]), idvar = "year", timevar = "id", direction = "wide")
  
  #Labels
  
  homesteads.pc.y <- cats.patents.sum.r[c("year", "homesteads.pc.Treated")]
  homesteads.pc.y <- homesteads.pc.y[!is.na(homesteads.pc.y$homesteads.pc.Treated),]
  
  sales.pc.y <- cats.patents.sum.r[c("year", "sales.pc.Treated")]
  sales.pc.y <- sales.pc.y[!is.na(sales.pc.y$sales.pc.Treated),]
  
  
  # Splits
  
  homesteads.pc.years <- intersect(homesteads.pc$year,homesteads.pc.y$year) # common homesteads.pc years in treated and control
  
  homesteads.pc.x.train <- homesteads.pc[homesteads.pc$year %in% homesteads.pc.years & homesteads.pc$year < 1859,]
  homesteads.pc.x.val <- homesteads.pc[homesteads.pc$year %in% homesteads.pc.years & (homesteads.pc$year >= 1859 & homesteads.pc$year < 1866),] # 1859, 1865 for validation
  homesteads.pc.x.test <- homesteads.pc[homesteads.pc$year %in% homesteads.pc.years & homesteads.pc$year >= 1866,]
  
  homesteads.pc.y.train <- homesteads.pc.y[homesteads.pc.y$year %in% homesteads.pc.years & homesteads.pc.y$year < 1859,]
  homesteads.pc.y.val <- homesteads.pc.y[homesteads.pc.y$year %in% homesteads.pc.years & (homesteads.pc.y$year >= 1859 & homesteads.pc.y$year < 1866),]
  homesteads.pc.y.test <- homesteads.pc.y[homesteads.pc.y$year %in% homesteads.pc.years &homesteads.pc.y$year >= 1866,]
  
  sales.pc.years <- intersect(sales.pc$year,sales.pc.y$year) # common sales.pc years in treated and control
  
  sales.pc.x.train <- sales.pc[sales.pc$year %in% sales.pc.years & sales.pc$year < 1859,]
  sales.pc.x.val <- sales.pc[sales.pc$year %in% sales.pc.years & (sales.pc$year >= 1859  & sales.pc$year < 1866),]
  sales.pc.x.test <- sales.pc[sales.pc$year %in% sales.pc.years & sales.pc$year >= 1866,]
  
  sales.pc.y.train <- sales.pc.y[sales.pc.y$year %in% sales.pc.years & sales.pc.y$year < 1859,]
  sales.pc.y.val <- sales.pc.y[sales.pc.y$year %in% sales.pc.years & (sales.pc.y$year >= 1859 & sales.pc.y$year < 1866),]
  sales.pc.y.test <- sales.pc.y[sales.pc.y$year %in% sales.pc.years &sales.pc.y$year >= 1866,]
  
  # Preprocess
  homesteads.pc.x.train <- data.frame(sapply(homesteads.pc.x.train, as.numeric))
  homesteads.pc.x.train[is.na(homesteads.pc.x.train)] <- 0 # fill NA with 0 before scale
  homesteads.pc.pre.train <- preProcess(homesteads.pc.x.train[!colnames(homesteads.pc.x.train) %in% c("year")], method = c("center", "scale","medianImpute"))
  homesteads.pc.x.train[!colnames(homesteads.pc.x.train) %in% c("year")] <- predict(homesteads.pc.pre.train, homesteads.pc.x.train[!colnames(homesteads.pc.x.train) %in% c("year")] )
  
  homesteads.pc.x.val <- data.frame(sapply(homesteads.pc.x.val, as.numeric))
  homesteads.pc.x.val[!colnames(homesteads.pc.x.val) %in% c("year")] <- predict(homesteads.pc.pre.train, homesteads.pc.x.val[!colnames(homesteads.pc.x.val) %in% c("year")] ) # use training values for val set 
  
  homesteads.pc.x.test <- data.frame(sapply(homesteads.pc.x.test, as.numeric))
  homesteads.pc.x.test[!colnames(homesteads.pc.x.test) %in% c("year")] <- predict(homesteads.pc.pre.train, homesteads.pc.x.test[!colnames(homesteads.pc.x.test) %in% c("year")] ) # use training values for test set 
  
  sales.pc.x.train <- data.frame(sapply(sales.pc.x.train, as.numeric))
  sales.pc.x.train[is.na(sales.pc.x.train)] <- 0 # fill NA with 0 before scale
  sales.pc.pre.train <- preProcess(sales.pc.x.train[!colnames(sales.pc.x.train) %in% c("year")], method = c("center", "scale","medianImpute"))
  sales.pc.x.train[!colnames(sales.pc.x.train) %in% c("year")] <- predict(sales.pc.pre.train, sales.pc.x.train[!colnames(sales.pc.x.train) %in% c("year")] )
  
  sales.pc.x.val <- data.frame(sapply(sales.pc.x.val, as.numeric))
  sales.pc.x.val[!colnames(sales.pc.x.val) %in% c("year")] <- predict(sales.pc.pre.train, sales.pc.x.val[!colnames(sales.pc.x.val) %in% c("year")] ) # use training values for val set 
  
  sales.pc.x.test <- data.frame(sapply(sales.pc.x.test, as.numeric))
  sales.pc.x.test[!colnames(sales.pc.x.test) %in% c("year")] <- predict(sales.pc.pre.train, sales.pc.x.test[!colnames(sales.pc.x.test) %in% c("year")] ) # use training values for test set 
  
  # Export each as csv (labels, features)
  data.directory <- "~/Dropbox/github/drnns-prediction/data/patents/analysis-12/treated/"
  
  write.csv(homesteads.pc.x.train[!colnames(homesteads.pc.x.train) %in% c("year")], paste0(data.directory,"homesteads-x-train.csv"), row.names=FALSE) 
  write.csv(homesteads.pc.x.val[!colnames(homesteads.pc.x.val) %in% c("year")] , paste0(data.directory,"homesteads-x-val.csv"), row.names=FALSE) 
  write.csv(homesteads.pc.x.test[!colnames(homesteads.pc.x.test) %in% c("year")] , paste0(data.directory,"homesteads-x-test.csv"), row.names=FALSE) 
  write.csv(homesteads.pc.y.train[!colnames(homesteads.pc.y.train) %in% c("year")], paste0(data.directory,"homesteads-y-train.csv"), row.names=FALSE) 
  write.csv(homesteads.pc.y.val[!colnames(homesteads.pc.y.val) %in% c("year")], paste0(data.directory,"homesteads-y-val.csv"), row.names=FALSE) 
  write.csv(homesteads.pc.y.test[!colnames(homesteads.pc.y.test) %in% c("year")], paste0(data.directory,"homesteads-y-test.csv"), row.names=FALSE) 
  
  write.csv(sales.pc.x.train[!colnames(sales.pc.x.train) %in% c("year")], paste0(data.directory,"sales-x-train.csv"), row.names=FALSE) 
  write.csv(sales.pc.x.val[!colnames(sales.pc.x.val) %in% c("year")] , paste0(data.directory,"sales-x-val.csv"), row.names=FALSE) 
  write.csv(sales.pc.x.test[!colnames(sales.pc.x.test) %in% c("year")] , paste0(data.directory,"sales-x-test.csv"), row.names=FALSE) 
  write.csv(sales.pc.y.train[!colnames(sales.pc.y.train) %in% c("year")], paste0(data.directory,"sales-y-train.csv"), row.names=FALSE) 
  write.csv(sales.pc.y.val[!colnames(sales.pc.y.val) %in% c("year")], paste0(data.directory,"sales-y-val.csv"), row.names=FALSE) 
  write.csv(sales.pc.y.test[!colnames(sales.pc.y.test) %in% c("year")], paste0(data.directory,"sales-y-test.csv"), row.names=FALSE) 
  
}

if(analysis==3){
  ## Analysis 3: Effect of HSA restriction on treated, intervention: Mar 1889
  # Treated is southern public land states (not MO)
  # Controls are MO, state land states
  
  # Summarize by category
  
  patents.sum$cat <- NA
  patents.sum$cat[patents.sum$state_code %in% southern.pub] <- "Treated"
  patents.sum$cat[patents.sum$state_code %in% c("MO",state.land.states)] <- "Control"
  
  # Create control and treated sums
  cats.patents.sum <- patents.sum %>% 
    filter(!is.na(cat)) %>% 
    group_by(year,cat) %>% 
    summarise_each(funs(mean(., na.rm = TRUE))) %>%
    select(-state_code) %>%
    select(-county_code)
  
  cats.patents.sum.r <- reshape(data.frame(cats.patents.sum), idvar = "year", timevar = "cat", direction = "wide")
  
  patents.sum.control <- patents.sum[!is.na(patents.sum$cat) & patents.sum$cat=="Control",][c("county_code", "state_code","year","homesteads.pc","sales.pc")] # discard treated since we have treated time-series
  patents.sum.control$id <- interaction(patents.sum.control$county_code, patents.sum.control$state_code)
  
  homesteads.pc <- reshape(data.frame(patents.sum.control[c("year","homesteads.pc","homesteads.pc","id")]), idvar = "year", timevar = "id", direction = "wide")
  sales.pc <- reshape(data.frame(patents.sum.control[c("year","homesteads.pc","sales.pc","id")]), idvar = "year", timevar = "id", direction = "wide")
  
  # Labels
  
  homesteads.pc.y <- cats.patents.sum.r[c("year", "homesteads.pc.Treated")]
  homesteads.pc.y <- homesteads.pc.y[!is.na(homesteads.pc.y$homesteads.pc.Treated),]
  
  sales.pc.y <- cats.patents.sum.r[c("year", "sales.pc.Treated")]
  sales.pc.y <- sales.pc.y[!is.na(sales.pc.y$sales.pc.Treated),]
  
  # Splits
  
  homesteads.pc.years <- intersect(homesteads.pc$year,homesteads.pc.y$year) # common homesteads.pc years in treated and control
  
  homesteads.pc.x.train <- homesteads.pc[homesteads.pc$year %in% homesteads.pc.years & homesteads.pc$year < 1886,]
  homesteads.pc.x.val <- homesteads.pc[homesteads.pc$year %in% homesteads.pc.years & (homesteads.pc$year >= 1886 & homesteads.pc$year < 1889),] # 1886, 1886, 1888 for validation
  homesteads.pc.x.test <- homesteads.pc[homesteads.pc$year %in% homesteads.pc.years & homesteads.pc$year >= 1889,]
  
  homesteads.pc.y.train <- homesteads.pc.y[homesteads.pc.y$year %in% homesteads.pc.years & homesteads.pc.y$year < 1886,]
  homesteads.pc.y.val <- homesteads.pc.y[homesteads.pc.y$year %in% homesteads.pc.years & (homesteads.pc.y$year >= 1886 & homesteads.pc.y$year < 1889),]
  homesteads.pc.y.test <- homesteads.pc.y[homesteads.pc.y$year %in% homesteads.pc.years & homesteads.pc.y$year >= 1889,]
  
  sales.pc.years <- intersect(sales.pc$year,sales.pc.y$year) # common sales.pc years in treated and control
  
  sales.pc.x.train <- sales.pc[sales.pc$year %in% sales.pc.years & sales.pc$year < 1886,]
  sales.pc.x.val <- sales.pc[sales.pc$year %in% sales.pc.years & (sales.pc$year >= 1886 & sales.pc$year < 1889),]
  sales.pc.x.test <- sales.pc[sales.pc$year %in% sales.pc.years & sales.pc$year >= 1889,]
  
  sales.pc.y.train <- sales.pc.y[sales.pc.y$year %in% sales.pc.years & sales.pc.y$year < 1886,]
  sales.pc.y.val <- sales.pc.y[sales.pc.y$year %in% sales.pc.years & (sales.pc.y$year >= 1886 & sales.pc.y$year < 1889),]
  sales.pc.y.test <- sales.pc.y[sales.pc.y$year %in% sales.pc.years & sales.pc.y$year >= 1889,]
  
  # Preprocess
  
  homesteads.pc.x.train <- data.frame(sapply(homesteads.pc.x.train, as.numeric))
  homesteads.pc.x.train[is.na(homesteads.pc.x.train)] <- 0 # fill NA with 0 before scale
  homesteads.pc.pre.train <- preProcess(homesteads.pc.x.train[!colnames(homesteads.pc.x.train) %in% c("year")], method = c("center", "scale","medianImpute"))
  homesteads.pc.x.train[!colnames(homesteads.pc.x.train) %in% c("year")] <- predict(homesteads.pc.pre.train, homesteads.pc.x.train[!colnames(homesteads.pc.x.train) %in% c("year")] )
  
  homesteads.pc.x.val <- data.frame(sapply(homesteads.pc.x.val, as.numeric))
  homesteads.pc.x.val[!colnames(homesteads.pc.x.val) %in% c("year")] <- predict(homesteads.pc.pre.train, homesteads.pc.x.val[!colnames(homesteads.pc.x.val) %in% c("year")] ) # use training values for val set 
  
  homesteads.pc.x.test <- data.frame(sapply(homesteads.pc.x.test, as.numeric))
  homesteads.pc.x.test[!colnames(homesteads.pc.x.test) %in% c("year")] <- predict(homesteads.pc.pre.train, homesteads.pc.x.test[!colnames(homesteads.pc.x.test) %in% c("year")] ) # use training values for test set 
  
  sales.pc.x.train <- data.frame(sapply(sales.pc.x.train, as.numeric))
  sales.pc.x.train[is.na(sales.pc.x.train)] <- 0 # fill NA with 0 before scale
  sales.pc.pre.train <- preProcess(sales.pc.x.train[!colnames(sales.pc.x.train) %in% c("year")], method = c("center", "scale","medianImpute"))
  sales.pc.x.train[!colnames(sales.pc.x.train) %in% c("year")] <- predict(sales.pc.pre.train, sales.pc.x.train[!colnames(sales.pc.x.train) %in% c("year")] )
  
  sales.pc.x.val <- data.frame(sapply(sales.pc.x.val, as.numeric))
  sales.pc.x.val[!colnames(sales.pc.x.val) %in% c("year")] <- predict(sales.pc.pre.train, sales.pc.x.val[!colnames(sales.pc.x.val) %in% c("year")] ) # use training values for val set 
  
  sales.pc.x.test <- data.frame(sapply(sales.pc.x.test, as.numeric))
  sales.pc.x.test[!colnames(sales.pc.x.test) %in% c("year")] <- predict(sales.pc.pre.train, sales.pc.x.test[!colnames(sales.pc.x.test) %in% c("year")] ) # use training values for test set 
  
  # Export each as csv (labels, features)
  data.directory <- "~/Dropbox/github/drnns-prediction/data/patents/analysis-34/treated/"
  
  write.csv(homesteads.pc.x.train[!colnames(homesteads.pc.x.train) %in% c("year")], paste0(data.directory,"homesteads-x-train.csv"), row.names=FALSE) 
  write.csv(homesteads.pc.x.val[!colnames(homesteads.pc.x.val) %in% c("year")] , paste0(data.directory,"homesteads-x-val.csv"), row.names=FALSE) 
  write.csv(homesteads.pc.x.test[!colnames(homesteads.pc.x.test) %in% c("year")] , paste0(data.directory,"homesteads-x-test.csv"), row.names=FALSE) 
  write.csv(homesteads.pc.y.train[!colnames(homesteads.pc.y.train) %in% c("year")], paste0(data.directory,"homesteads-y-train.csv"), row.names=FALSE) 
  write.csv(homesteads.pc.y.val[!colnames(homesteads.pc.y.val) %in% c("year")], paste0(data.directory,"homesteads-y-val.csv"), row.names=FALSE) 
  write.csv(homesteads.pc.y.test[!colnames(homesteads.pc.y.test) %in% c("year")], paste0(data.directory,"homesteads-y-test.csv"), row.names=FALSE) 
  
  write.csv(sales.pc.x.train[!colnames(sales.pc.x.train) %in% c("year")], paste0(data.directory,"sales-x-train.csv"), row.names=FALSE) 
  write.csv(sales.pc.x.val[!colnames(sales.pc.x.val) %in% c("year")] , paste0(data.directory,"sales-x-val.csv"), row.names=FALSE) 
  write.csv(sales.pc.x.test[!colnames(sales.pc.x.test) %in% c("year")] , paste0(data.directory,"sales-x-test.csv"), row.names=FALSE) 
  write.csv(sales.pc.y.train[!colnames(sales.pc.y.train) %in% c("year")], paste0(data.directory,"sales-y-train.csv"), row.names=FALSE) 
  write.csv(sales.pc.y.val[!colnames(sales.pc.y.val) %in% c("year")], paste0(data.directory,"sales-y-val.csv"), row.names=FALSE) 
  write.csv(sales.pc.y.test[!colnames(sales.pc.y.test) %in% c("year")], paste0(data.directory,"sales-y-test.csv"), row.names=FALSE) 
}

if(analysis==4){
  ## Analysis 4: Effect of HSA restriction on treated, intervention: Mar 1889
  # Treated is western public land states (not MO)
  # *Controls are MO*
  
  # Summarize by category
  
  patents.sum$cat <- NA
  patents.sum$cat[patents.sum$state_code %in% setdiff(setdiff(pub.states,southern.pub), "MO")] <- "Treated"
  patents.sum$cat[patents.sum$state_code %in% c("MO")] <- "Control"
  
  # Create control and treated sums
  cats.patents.sum <- patents.sum %>% 
    filter(!is.na(cat)) %>% 
    group_by(year,cat) %>% 
    summarise_each(funs(mean(., na.rm = TRUE))) %>%
    select(-state_code) %>%
    select(-county_code)
  
  cats.patents.sum.r <- reshape(data.frame(cats.patents.sum), idvar = "year", timevar = "cat", direction = "wide")
  
  patents.sum.control <- patents.sum[!is.na(patents.sum$cat) & patents.sum$cat=="Control",][c("county_code", "state_code","year","homesteads.pc","sales.pc")] # discard treated since we have treated time-series
  patents.sum.control$id <- interaction(patents.sum.control$county_code, patents.sum.control$state_code)
  
  homesteads.pc <- reshape(data.frame(patents.sum.control[c("year","homesteads.pc","homesteads.pc","id")]), idvar = "year", timevar = "id", direction = "wide")
  sales.pc <- reshape(data.frame(patents.sum.control[c("year","homesteads.pc","sales.pc","id")]), idvar = "year", timevar = "id", direction = "wide")

  # Labels
  
  homesteads.pc.y <- cats.patents.sum.r[c("year", "homesteads.pc.Treated")]
  homesteads.pc.y <- homesteads.pc.y[!is.na(homesteads.pc.y$homesteads.pc.Treated),]
  
  sales.pc.y <- cats.patents.sum.r[c("year", "sales.pc.Treated")]
  sales.pc.y <- sales.pc.y[!is.na(sales.pc.y$sales.pc.Treated),]
  
  # Splits
  
  homesteads.pc.years <- intersect(homesteads.pc$year,homesteads.pc.y$year) # common homesteads.pc years in treated and control
  
  homesteads.pc.x.train <- homesteads.pc[homesteads.pc$year %in% homesteads.pc.years & homesteads.pc$year < 1886,]
  homesteads.pc.x.val <- homesteads.pc[homesteads.pc$year %in% homesteads.pc.years & (homesteads.pc$year >= 1886 & homesteads.pc$year < 1889),] # 1886, 1886, 1888 for validation
  homesteads.pc.x.test <- homesteads.pc[homesteads.pc$year %in% homesteads.pc.years & homesteads.pc$year >= 1889,]
  
  homesteads.pc.y.train <- homesteads.pc.y[homesteads.pc.y$year %in% homesteads.pc.years & homesteads.pc.y$year < 1886,]
  homesteads.pc.y.val <- homesteads.pc.y[homesteads.pc.y$year %in% homesteads.pc.years & (homesteads.pc.y$year >= 1886 & homesteads.pc.y$year < 1889),]
  homesteads.pc.y.test <- homesteads.pc.y[homesteads.pc.y$year %in% homesteads.pc.years & homesteads.pc.y$year >= 1889,]
  
  sales.pc.years <- intersect(sales.pc$year,sales.pc.y$year) # common sales.pc years in treated and control
  
  sales.pc.x.train <- sales.pc[sales.pc$year %in% sales.pc.years & sales.pc$year < 1886,]
  sales.pc.x.val <- sales.pc[sales.pc$year %in% sales.pc.years & (sales.pc$year >= 1886 & sales.pc$year < 1889),]
  sales.pc.x.test <- sales.pc[sales.pc$year %in% sales.pc.years & sales.pc$year >= 1889,]
  
  sales.pc.y.train <- sales.pc.y[sales.pc.y$year %in% sales.pc.years & sales.pc.y$year < 1886,]
  sales.pc.y.val <- sales.pc.y[sales.pc.y$year %in% sales.pc.years & (sales.pc.y$year >= 1886 & sales.pc.y$year < 1889),]
  sales.pc.y.test <- sales.pc.y[sales.pc.y$year %in% sales.pc.years & sales.pc.y$year >= 1889,]
  
  # Preprocess
  
  homesteads.pc.x.train <- data.frame(sapply(homesteads.pc.x.train, as.numeric))
  homesteads.pc.x.train[is.na(homesteads.pc.x.train)] <- 0 # fill NA with 0 before scale
  homesteads.pc.pre.train <- preProcess(homesteads.pc.x.train[!colnames(homesteads.pc.x.train) %in% c("year")], method = c("center", "scale","medianImpute"))
  homesteads.pc.x.train[!colnames(homesteads.pc.x.train) %in% c("year")] <- predict(homesteads.pc.pre.train, homesteads.pc.x.train[!colnames(homesteads.pc.x.train) %in% c("year")] )
  
  homesteads.pc.x.val <- data.frame(sapply(homesteads.pc.x.val, as.numeric))
  homesteads.pc.x.val[!colnames(homesteads.pc.x.val) %in% c("year")] <- predict(homesteads.pc.pre.train, homesteads.pc.x.val[!colnames(homesteads.pc.x.val) %in% c("year")] ) # use training values for val set 
  
  homesteads.pc.x.test <- data.frame(sapply(homesteads.pc.x.test, as.numeric))
  homesteads.pc.x.test[!colnames(homesteads.pc.x.test) %in% c("year")] <- predict(homesteads.pc.pre.train, homesteads.pc.x.test[!colnames(homesteads.pc.x.test) %in% c("year")] ) # use training values for test set 
  
  sales.pc.x.train <- data.frame(sapply(sales.pc.x.train, as.numeric))
  sales.pc.x.train[is.na(sales.pc.x.train)] <- 0 # fill NA with 0 before scale
  sales.pc.pre.train <- preProcess(sales.pc.x.train[!colnames(sales.pc.x.train) %in% c("year")], method = c("center", "scale","medianImpute"))
  sales.pc.x.train[!colnames(sales.pc.x.train) %in% c("year")] <- predict(sales.pc.pre.train, sales.pc.x.train[!colnames(sales.pc.x.train) %in% c("year")] )
  
  sales.pc.x.val <- data.frame(sapply(sales.pc.x.val, as.numeric))
  sales.pc.x.val[!colnames(sales.pc.x.val) %in% c("year")] <- predict(sales.pc.pre.train, sales.pc.x.val[!colnames(sales.pc.x.val) %in% c("year")] ) # use training values for val set 
  
  sales.pc.x.test <- data.frame(sapply(sales.pc.x.test, as.numeric))
  sales.pc.x.test[!colnames(sales.pc.x.test) %in% c("year")] <- predict(sales.pc.pre.train, sales.pc.x.test[!colnames(sales.pc.x.test) %in% c("year")] ) # use training values for test set 
  
  # Export each as csv (labels, features)
  data.directory <- "~/Dropbox/github/drnns-prediction/data/patents/analysis-41/treated/"
  
  write.csv(homesteads.pc.x.train[!colnames(homesteads.pc.x.train) %in% c("year")], paste0(data.directory,"homesteads-x-train.csv"), row.names=FALSE) 
  write.csv(homesteads.pc.x.val[!colnames(homesteads.pc.x.val) %in% c("year")] , paste0(data.directory,"homesteads-x-val.csv"), row.names=FALSE) 
  write.csv(homesteads.pc.x.test[!colnames(homesteads.pc.x.test) %in% c("year")] , paste0(data.directory,"homesteads-x-test.csv"), row.names=FALSE) 
  write.csv(homesteads.pc.y.train[!colnames(homesteads.pc.y.train) %in% c("year")], paste0(data.directory,"homesteads-y-train.csv"), row.names=FALSE) 
  write.csv(homesteads.pc.y.val[!colnames(homesteads.pc.y.val) %in% c("year")], paste0(data.directory,"homesteads-y-val.csv"), row.names=FALSE) 
  write.csv(homesteads.pc.y.test[!colnames(homesteads.pc.y.test) %in% c("year")], paste0(data.directory,"homesteads-y-test.csv"), row.names=FALSE) 
  
  write.csv(sales.pc.x.train[!colnames(sales.pc.x.train) %in% c("year")], paste0(data.directory,"sales-x-train.csv"), row.names=FALSE) 
  write.csv(sales.pc.x.val[!colnames(sales.pc.x.val) %in% c("year")] , paste0(data.directory,"sales-x-val.csv"), row.names=FALSE) 
  write.csv(sales.pc.x.test[!colnames(sales.pc.x.test) %in% c("year")] , paste0(data.directory,"sales-x-test.csv"), row.names=FALSE) 
  write.csv(sales.pc.y.train[!colnames(sales.pc.y.train) %in% c("year")], paste0(data.directory,"sales-y-train.csv"), row.names=FALSE) 
  write.csv(sales.pc.y.val[!colnames(sales.pc.y.val) %in% c("year")], paste0(data.directory,"sales-y-val.csv"), row.names=FALSE) 
  write.csv(sales.pc.y.test[!colnames(sales.pc.y.test) %in% c("year")], paste0(data.directory,"sales-y-test.csv"), row.names=FALSE) 
  }