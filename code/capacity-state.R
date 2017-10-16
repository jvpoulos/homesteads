############################################################################
# Prepare capacity data (state-level or aggregated at county level)        #
############################################################################

require(data.table)
require(reshape2)
require(stringr)
require(dplyr)
require(weights)
require(caret)
require(zoo)
require(tidyr)

analysis <- 0

## STATE-LEVEL DATA

iso.funds <- c(1,3,31) # target iso codes
names.funds <- c("total.rev",'total.exp')

## Sources and Uses of Funds in State and Local Governments, 1790-1915 (ICPSR 9728)
## preliminary  financial  data on  state government revenues and expenditures for 48 states during the period 1790-1915, 

# Set location of files
setwd(paste0(data.directory, "capacity/","ICPSR_09728"))

# Import files 

data.files.9728 <- list.files(pattern = "*Data.txt", recursive = TRUE)

funds.9728 <- do.call(rbind,lapply(data.files.9728[1:94],read.csv, 
                                   header=FALSE,
                                   row.names=NULL,
                                   sep = ",",
                                   stringsAsFactors=FALSE,
                                   col.names = c("state","year","iso","total"))) # parts 1-94

# Clean parts 1-94
funds.9728 <- funds.9728[!is.na(funds.9728$year),] # drop empty rows

funds.9728 <- funds.9728[with(funds.9728, order(state, year)), ] # order by state and year

funds.9728 <- funds.9728[funds.9728$iso %in% iso.funds,] 

# Make data wide

funds.9728$row <- 1:nrow(funds.9728)
funds.9728 <- spread(funds.9728, key = iso, value = total)

# Collapse by state/year

funds.9728 <- funds.9728 %>% 
  group_by(state,year) %>% 
  summarise_each(funs(mean(., na.rm = TRUE))) 

# Clean state codes
funds.9728$state[funds.9728$state=="MIS"] <- "MS"
funds.9728$state[funds.9728$state=="IO"] <- "IA" 
funds.9728$state[funds.9728$state=="KA"] <- "KS" 

# Clean MS revenue in 1843

funds.9728$`1`[funds.9728$state=="MS" & funds.9728$year==1843] <- funds.9728$`1`[funds.9728$state=="MS" & funds.9728$year==1843]/10
 
## State and Local Government [United States]: Sources and Uses of Funds, State Financial Statistics, 1933-1937 (ICPSR 6306)

# Set location of files
setwd(paste0(data.directory, "capacity/","ICPSR_06306"))

# Import files 

data.files.6306 <- list.files(pattern = "*Data.txt", recursive = TRUE)

funds.6306 <- do.call(rbind,lapply(data.files.6306[c(1:2,5:6)],read.csv, 
                                   header=FALSE,
                                   row.names=NULL,
                                   sep = ",",
                                   stringsAsFactors=FALSE,
                                   col.names = c("state","year","iso","value"))) # 1-2 levels of aggregation

funds.6306 <- funds.6306[!is.na(funds.6306$year),] # drop empty rows

funds.6306 <- funds.6306[with(funds.6306, order(state, year)), ] # order by state and year

funds.6306 <- funds.6306[with(funds.6306, order(state, year)), ] # order by state and year

funds.6306 <- funds.6306[funds.6306$iso %in% iso.funds,] 

# Make data wide

funds.6306$row <- 1:nrow(funds.6306)
funds.6306 <- spread(funds.6306, key = iso, value = value)

# Collapse by state/year

funds.6306 <- funds.6306 %>% 
  group_by(state,year) %>% 
  summarise_each(funs(mean(., na.rm = TRUE))) 

# Clean state codes
funds.6306$state[funds.6306$state=="NB"] <- "NE"

## State and Local Government: Sources and Uses of Funds, 1902, 1913, 1932, 1942, 1962, 1972, and 1982 (ICPSR 6304)

# Set location of files
setwd(paste0(data.directory, "capacity/","ICPSR_06304"))

# Import files 

data.files.6304 <- list.files(pattern = "*Data.txt", recursive = TRUE)

funds.6304 <- do.call(rbind,lapply(data.files.6304[1:7],read.csv, 
                                       header=FALSE,
                                       row.names=NULL,
                                       sep = ",",
                                       skip = 12,
                                       stringsAsFactors=FALSE,
                                       col.names = c("state","year","type","iso","value"))) 

funds.6304 <- funds.6304[funds.6304$type=='SSS',] # keep state gov't

funds.6304 <- funds.6304[funds.6304$iso %in% c(iso.funds, 3100),] 

funds.6304$iso[funds.6304$iso ==3100] <- 31 # for consitency

# Make data wide

funds.6304$row <- 1:nrow(funds.6304)
funds.6304 <- spread(funds.6304, key = iso, value = value)

# Collapse by state/year

funds.6304 <- funds.6304 %>% 
  group_by(state,year) %>% 
  summarise_each(funs(mean(., na.rm = TRUE))) 

## Append Wallis datasets

funds <- rbind(rbind(funds.9728,funds.6306),funds.6304) %>% 
  group_by(state,year) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)))  # takes mean of duplicates from 1902,1913,1932

funds <- funds[with(funds, order(state, year)), ] # order by state and year

# Rm Outliers

funds <- subset(funds, !(funds$state=="WA" & funds$year>=1907 & funds$year<=1918))
funds <- subset(funds, !(funds$state=="RI" & funds$year==1822))
funds <- subset(funds, !(funds$state=="RI" & funds$year==1822))

## Make log per-capita measures

funds$year2 <- signif(funds$year,3) # merge by nearest decennial
funds$year2[funds$year<=1785] <- 1790 # VA
funds$year2[funds$year2<1880 & funds$state=="AK"] <- 1880
funds$year2[funds$year2==1980] <- 1983

funds <- merge(funds, census.ts.state[c('year','state','ns.pop')], by.x=c('year2','state'), by.y=c('year','state'),all.x=TRUE)

funds["rev.pc"] <- NA
funds["rev.pc"] <- log(funds["1"]/funds$ns.pop+ .Machine$double.eps)

funds["exp.pc"] <- NA
funds["exp.pc"] <- log(funds["3"]/funds$ns.pop+ .Machine$double.eps)

funds["educ.pc"] <- NA
funds["educ.pc"] <- log(funds["31"]/funds$ns.pop+ .Machine$double.eps)

# clean feature set
funds <- funds[colnames(funds) %in% c("state","year",
                                      "rev.pc","exp.pc","educ.pc")]

if(analysis==0){
  
## Analysis 0: Effect of HSA on treated (western public land states), intervention: May 1862
# controls are non-southern state land states

# Summarize by category

funds$cat <- NA
funds$cat[funds$state %in% setdiff(pub.states,southern.pub)] <- "Treated"
funds$cat[funds$state %in% setdiff(state.land.states,southern.state)] <- "Control"

# Create control and treated sums
cats.funds <- funds %>% 
  filter(!is.na(cat)) %>% # rm southern states
  group_by(year,cat) %>% 
  summarise_each(funs(mean(., na.rm = TRUE))) %>%
  select(-state)

cats.funds.r <- reshape(data.frame(cats.funds), idvar = "year", timevar = "cat", direction = "wide")

funds.control <- funds[!is.na(funds$cat) & funds$cat=="Control",][c("state","year","rev.pc","exp.pc","educ.pc")] # discard treated since we have treated time-series

rev.pc <- reshape(data.frame(funds.control), idvar = "year", timevar = "state", direction = "wide")
exp.pc <- reshape(data.frame(funds.control), idvar = "year", timevar = "state", direction = "wide")
educ.pc <- reshape(data.frame(funds.control), idvar = "year", timevar = "state", direction = "wide")

#Labels

rev.pc.y <- cats.funds.r[c("year", "rev.pc.Treated")]
rev.pc.y <- rev.pc.y[!is.na(rev.pc.y$rev.pc.Treated),]

exp.pc.y <- cats.funds.r[c("year", "exp.pc.Treated")]
exp.pc.y <- exp.pc.y[!is.na(exp.pc.y$exp.pc.Treated),]

educ.pc.y <- cats.funds.r[c("year", "educ.pc.Treated")]
educ.pc.y <- educ.pc.y[!is.na(educ.pc.y$educ.pc.Treated),]

# Splits

rev.pc.years <- intersect(rev.pc$year,rev.pc.y$year) # common rev.pc years in treated and control

rev.pc.x.train <- rev.pc[rev.pc$year %in% rev.pc.years & rev.pc$year < 1859,]
rev.pc.x.val <- rev.pc[rev.pc$year %in% rev.pc.years & (rev.pc$year >= 1859 & rev.pc$year < 1862),] # 1859, 1860, 1861 for validation
rev.pc.x.test <- rev.pc[rev.pc$year %in% rev.pc.years & rev.pc$year >= 1862,]

rev.pc.y.train <- rev.pc.y[rev.pc.y$year %in% rev.pc.years & rev.pc.y$year < 1859,]
rev.pc.y.val <- rev.pc.y[rev.pc.y$year %in% rev.pc.years & (rev.pc.y$year >= 1859 & rev.pc.y$year < 1862),]
rev.pc.y.test <- rev.pc.y[rev.pc.y$year %in% rev.pc.years &rev.pc.y$year >= 1862,]

exp.pc.years <- intersect(exp.pc$year,exp.pc.y$year) # common exp.pc years in treated and control

exp.pc.x.train <- exp.pc[exp.pc$year %in% exp.pc.years & exp.pc$year < 1859,]
exp.pc.x.val <- exp.pc[exp.pc$year %in% exp.pc.years & (exp.pc$year >= 1859  & exp.pc$year < 1862),]
exp.pc.x.test <- exp.pc[exp.pc$year %in% exp.pc.years & exp.pc$year >= 1862,]

exp.pc.y.train <- exp.pc.y[exp.pc.y$year %in% exp.pc.years & exp.pc.y$year < 1859,]
exp.pc.y.val <- exp.pc.y[exp.pc.y$year %in% exp.pc.years & (exp.pc.y$year >= 1859 & exp.pc.y$year < 1862),]
exp.pc.y.test <- exp.pc.y[exp.pc.y$year %in% exp.pc.years &exp.pc.y$year >= 1862,]

educ.pc.years <- intersect(educ.pc$year,educ.pc.y$year) # common educ.pc years in treated and control

educ.pc.x.train <- educ.pc[educ.pc$year %in% educ.pc.years & educ.pc$year < 1859,]
educ.pc.x.val <- educ.pc[educ.pc$year %in% educ.pc.years & (educ.pc$year >= 1859  & educ.pc$year < 1862),]
educ.pc.x.test <- educ.pc[educ.pc$year %in% educ.pc.years & educ.pc$year >= 1862,]

educ.pc.y.train <- educ.pc.y[educ.pc.y$year %in% educ.pc.years & educ.pc.y$year < 1859,]
educ.pc.y.val <- educ.pc.y[educ.pc.y$year %in% educ.pc.years & (educ.pc.y$year >= 1859 & educ.pc.y$year < 1862),]
educ.pc.y.test <- educ.pc.y[educ.pc.y$year %in% educ.pc.years &educ.pc.y$year >= 1862,]

# Preprocess
rev.pc.x.train <- data.frame(sapply(rev.pc.x.train, as.numeric))
rev.pc.x.train[is.na(rev.pc.x.train)] <- 0 # fill NA with 0 before scale
rev.pc.pre.train <- preProcess(rev.pc.x.train[!colnames(rev.pc.x.train) %in% c("year")], method = c("center", "scale","medianImpute"))
rev.pc.x.train[!colnames(rev.pc.x.train) %in% c("year")] <- predict(rev.pc.pre.train, rev.pc.x.train[!colnames(rev.pc.x.train) %in% c("year")] )

rev.pc.x.val <- data.frame(sapply(rev.pc.x.val, as.numeric))
rev.pc.x.val[!colnames(rev.pc.x.val) %in% c("year")] <- predict(rev.pc.pre.train, rev.pc.x.val[!colnames(rev.pc.x.val) %in% c("year")] ) # use training values for val set 

rev.pc.x.test <- data.frame(sapply(rev.pc.x.test, as.numeric))
rev.pc.x.test[!colnames(rev.pc.x.test) %in% c("year")] <- predict(rev.pc.pre.train, rev.pc.x.test[!colnames(rev.pc.x.test) %in% c("year")] ) # use training values for test set 

exp.pc.x.train <- data.frame(sapply(exp.pc.x.train, as.numeric))
exp.pc.x.train[is.na(exp.pc.x.train)] <- 0 # fill NA with 0 before scale
exp.pc.pre.train <- preProcess(exp.pc.x.train[!colnames(exp.pc.x.train) %in% c("year")], method = c("center", "scale","medianImpute"))
exp.pc.x.train[!colnames(exp.pc.x.train) %in% c("year")] <- predict(exp.pc.pre.train, exp.pc.x.train[!colnames(exp.pc.x.train) %in% c("year")] )

exp.pc.x.val <- data.frame(sapply(exp.pc.x.val, as.numeric))
exp.pc.x.val[!colnames(exp.pc.x.val) %in% c("year")] <- predict(exp.pc.pre.train, exp.pc.x.val[!colnames(exp.pc.x.val) %in% c("year")] ) # use training values for val set 

exp.pc.x.test <- data.frame(sapply(exp.pc.x.test, as.numeric))
exp.pc.x.test[!colnames(exp.pc.x.test) %in% c("year")] <- predict(exp.pc.pre.train, exp.pc.x.test[!colnames(exp.pc.x.test) %in% c("year")] ) # use training values for test set 

educ.pc.x.train <- data.frame(sapply(educ.pc.x.train, as.numeric))
educ.pc.x.train[is.na(educ.pc.x.train)] <- 0 # fill NA with 0 before scale
educ.pc.pre.train <- preProcess(educ.pc.x.train[!colnames(educ.pc.x.train) %in% c("year")], method = c("center", "scale","medianImpute"))
educ.pc.x.train[!colnames(educ.pc.x.train) %in% c("year")] <- predict(educ.pc.pre.train, educ.pc.x.train[!colnames(educ.pc.x.train) %in% c("year")] )

educ.pc.x.val <- data.frame(sapply(educ.pc.x.val, as.numeric))
educ.pc.x.val[!colnames(educ.pc.x.val) %in% c("year")] <- predict(educ.pc.pre.train, educ.pc.x.val[!colnames(educ.pc.x.val) %in% c("year")] ) # use training values for val set 

educ.pc.x.test <- data.frame(sapply(educ.pc.x.test, as.numeric))
educ.pc.x.test[!colnames(educ.pc.x.test) %in% c("year")] <- predict(educ.pc.pre.train, educ.pc.x.test[!colnames(educ.pc.x.test) %in% c("year")] ) # use training values for test set

# Export each as csv (labels, features)
data.directory <- "~/Dropbox/github/drnns-prediction/data/capacity/analysis-01/treated/"

write.csv(rev.pc.x.train[!colnames(rev.pc.x.train) %in% c("year")], paste0(data.directory,"revpc-x-train.csv"), row.names=FALSE) 
write.csv(rev.pc.x.val[!colnames(rev.pc.x.val) %in% c("year")] , paste0(data.directory,"revpc-x-val.csv"), row.names=FALSE) 
write.csv(rev.pc.x.test[!colnames(rev.pc.x.test) %in% c("year")] , paste0(data.directory,"revpc-x-test.csv"), row.names=FALSE) 
write.csv(rev.pc.y.train[!colnames(rev.pc.y.train) %in% c("year")], paste0(data.directory,"revpc-y-train.csv"), row.names=FALSE) 
write.csv(rev.pc.y.val[!colnames(rev.pc.y.val) %in% c("year")], paste0(data.directory,"revpc-y-val.csv"), row.names=FALSE) 
write.csv(rev.pc.y.test[!colnames(rev.pc.y.test) %in% c("year")], paste0(data.directory,"revpc-y-test.csv"), row.names=FALSE) 

write.csv(exp.pc.x.train[!colnames(exp.pc.x.train) %in% c("year")], paste0(data.directory,"exppc-x-train.csv"), row.names=FALSE) 
write.csv(exp.pc.x.val[!colnames(exp.pc.x.val) %in% c("year")] , paste0(data.directory,"exppc-x-val.csv"), row.names=FALSE) 
write.csv(exp.pc.x.test[!colnames(exp.pc.x.test) %in% c("year")] , paste0(data.directory,"exppc-x-test.csv"), row.names=FALSE) 
write.csv(exp.pc.y.train[!colnames(exp.pc.y.train) %in% c("year")], paste0(data.directory,"exppc-y-train.csv"), row.names=FALSE) 
write.csv(exp.pc.y.val[!colnames(exp.pc.y.val) %in% c("year")], paste0(data.directory,"exppc-y-val.csv"), row.names=FALSE) 
write.csv(exp.pc.y.test[!colnames(exp.pc.y.test) %in% c("year")], paste0(data.directory,"exppc-y-test.csv"), row.names=FALSE) 

write.csv(educ.pc.x.train[!colnames(educ.pc.x.train) %in% c("year")], paste0(data.directory,"educpc-x-train.csv"), row.names=FALSE) 
write.csv(educ.pc.x.val[!colnames(educ.pc.x.val) %in% c("year")] , paste0(data.directory,"educpc-x-val.csv"), row.names=FALSE) 
write.csv(educ.pc.x.test[!colnames(educ.pc.x.test) %in% c("year")] , paste0(data.directory,"educpc-x-test.csv"), row.names=FALSE) 
write.csv(educ.pc.y.train[!colnames(educ.pc.y.train) %in% c("year")], paste0(data.directory,"educpc-y-train.csv"), row.names=FALSE) 
write.csv(educ.pc.y.val[!colnames(educ.pc.y.val) %in% c("year")], paste0(data.directory,"educpc-y-val.csv"), row.names=FALSE) 
write.csv(educ.pc.y.test[!colnames(educ.pc.y.test) %in% c("year")], paste0(data.directory,"educpc-y-test.csv"), row.names=FALSE) 
}

if(analysis==1){
## Analysis 1: Effect of SHA on treated (southern public land states), intervention: June 1866-June 1876-March 1889
# controls are southern state land states

# Summarize by category

funds$cat <- NA
funds$cat[funds$state %in% southern.pub] <- "Treated"
funds$cat[funds$state %in% southern.state] <- "Control"

# Create control and treated sums
cats.funds <- funds %>% 
  filter(!is.na(cat)) %>% # rm non-southern state land states
  group_by(year,cat) %>% 
  summarise_each(funs(mean(., na.rm = TRUE))) %>%
  select(-state)

cats.funds.r <- reshape(data.frame(cats.funds), idvar = "year", timevar = "cat", direction = "wide")

funds.control <- funds[!is.na(funds$cat) & funds$cat=="Control",][c("state","year","rev.pc","exp.pc","educ.pc")] # discard treated since we have treated time-series

rev.pc <- reshape(data.frame(funds.control), idvar = "year", timevar = "state", direction = "wide")
exp.pc <- reshape(data.frame(funds.control), idvar = "year", timevar = "state", direction = "wide")
educ.pc <- reshape(data.frame(funds.control), idvar = "year", timevar = "state", direction = "wide")

#Labels

rev.pc.y <- cats.funds.r[c("year", "rev.pc.Treated")]
rev.pc.y <- rev.pc.y[!is.na(rev.pc.y$rev.pc.Treated),]

exp.pc.y <- cats.funds.r[c("year", "exp.pc.Treated")]
exp.pc.y <- exp.pc.y[!is.na(exp.pc.y$exp.pc.Treated),]

educ.pc.y <- cats.funds.r[c("year", "educ.pc.Treated")]
educ.pc.y <- educ.pc.y[!is.na(educ.pc.y$educ.pc.Treated),]

# Splits

rev.pc.years <- intersect(rev.pc$year,rev.pc.y$year) # common rev.pc years in treated and control

rev.pc.x.train <- rev.pc[rev.pc$year %in% rev.pc.years & rev.pc$year < 1859,]
rev.pc.x.val <- rev.pc[rev.pc$year %in% rev.pc.years & (rev.pc$year >= 1859 & rev.pc$year < 1866),] # 1859, 1865 for validation
rev.pc.x.test <- rev.pc[rev.pc$year %in% rev.pc.years & rev.pc$year >= 1866,]

rev.pc.y.train <- rev.pc.y[rev.pc.y$year %in% rev.pc.years & rev.pc.y$year < 1859,]
rev.pc.y.val <- rev.pc.y[rev.pc.y$year %in% rev.pc.years & (rev.pc.y$year >= 1859 & rev.pc.y$year < 1866),]
rev.pc.y.test <- rev.pc.y[rev.pc.y$year %in% rev.pc.years &rev.pc.y$year >= 1866,]

exp.pc.years <- intersect(exp.pc$year,exp.pc.y$year) # common exp.pc years in treated and control

exp.pc.x.train <- exp.pc[exp.pc$year %in% exp.pc.years & exp.pc$year < 1859,]
exp.pc.x.val <- exp.pc[exp.pc$year %in% exp.pc.years & (exp.pc$year >= 1859  & exp.pc$year < 1866),]
exp.pc.x.test <- exp.pc[exp.pc$year %in% exp.pc.years & exp.pc$year >= 1866,]

exp.pc.y.train <- exp.pc.y[exp.pc.y$year %in% exp.pc.years & exp.pc.y$year < 1859,]
exp.pc.y.val <- exp.pc.y[exp.pc.y$year %in% exp.pc.years & (exp.pc.y$year >= 1859 & exp.pc.y$year < 1866),]
exp.pc.y.test <- exp.pc.y[exp.pc.y$year %in% exp.pc.years &exp.pc.y$year >= 1866,]

educ.pc.years <- intersect(educ.pc$year,educ.pc.y$year) # common educ.pc years in treated and control

educ.pc.x.train <- educ.pc[educ.pc$year %in% educ.pc.years & educ.pc$year < 1859,]
educ.pc.x.val <- educ.pc[educ.pc$year %in% educ.pc.years & (educ.pc$year >= 1859  & educ.pc$year < 1866),]
educ.pc.x.test <- educ.pc[educ.pc$year %in% educ.pc.years & educ.pc$year >= 1866,]

educ.pc.y.train <- educ.pc.y[educ.pc.y$year %in% educ.pc.years & educ.pc.y$year < 1859,]
educ.pc.y.val <- educ.pc.y[educ.pc.y$year %in% educ.pc.years & (educ.pc.y$year >= 1859 & educ.pc.y$year < 1866),]
educ.pc.y.test <- educ.pc.y[educ.pc.y$year %in% educ.pc.years &educ.pc.y$year >= 1866,]

# Preprocess
rev.pc.x.train <- data.frame(sapply(rev.pc.x.train, as.numeric))
rev.pc.x.train[is.na(rev.pc.x.train)] <- 0 # fill NA with 0 before scale
rev.pc.pre.train <- preProcess(rev.pc.x.train[!colnames(rev.pc.x.train) %in% c("year")], method = c("center", "scale","medianImpute"))
rev.pc.x.train[!colnames(rev.pc.x.train) %in% c("year")] <- predict(rev.pc.pre.train, rev.pc.x.train[!colnames(rev.pc.x.train) %in% c("year")] )

rev.pc.x.val <- data.frame(sapply(rev.pc.x.val, as.numeric))
rev.pc.x.val[!colnames(rev.pc.x.val) %in% c("year")] <- predict(rev.pc.pre.train, rev.pc.x.val[!colnames(rev.pc.x.val) %in% c("year")] ) # use training values for val set 

rev.pc.x.test <- data.frame(sapply(rev.pc.x.test, as.numeric))
rev.pc.x.test[!colnames(rev.pc.x.test) %in% c("year")] <- predict(rev.pc.pre.train, rev.pc.x.test[!colnames(rev.pc.x.test) %in% c("year")] ) # use training values for test set 

exp.pc.x.train <- data.frame(sapply(exp.pc.x.train, as.numeric))
exp.pc.x.train[is.na(exp.pc.x.train)] <- 0 # fill NA with 0 before scale
exp.pc.pre.train <- preProcess(exp.pc.x.train[!colnames(exp.pc.x.train) %in% c("year")], method = c("center", "scale","medianImpute"))
exp.pc.x.train[!colnames(exp.pc.x.train) %in% c("year")] <- predict(exp.pc.pre.train, exp.pc.x.train[!colnames(exp.pc.x.train) %in% c("year")] )

exp.pc.x.val <- data.frame(sapply(exp.pc.x.val, as.numeric))
exp.pc.x.val[!colnames(exp.pc.x.val) %in% c("year")] <- predict(exp.pc.pre.train, exp.pc.x.val[!colnames(exp.pc.x.val) %in% c("year")] ) # use training values for val set 

exp.pc.x.test <- data.frame(sapply(exp.pc.x.test, as.numeric))
exp.pc.x.test[!colnames(exp.pc.x.test) %in% c("year")] <- predict(exp.pc.pre.train, exp.pc.x.test[!colnames(exp.pc.x.test) %in% c("year")] ) # use training values for test set 

educ.pc.x.train <- data.frame(sapply(educ.pc.x.train, as.numeric))
educ.pc.x.train[is.na(educ.pc.x.train)] <- 0 # fill NA with 0 before scale
educ.pc.pre.train <- preProcess(educ.pc.x.train[!colnames(educ.pc.x.train) %in% c("year")], method = c("center", "scale","medianImpute"))
educ.pc.x.train[!colnames(educ.pc.x.train) %in% c("year")] <- predict(educ.pc.pre.train, educ.pc.x.train[!colnames(educ.pc.x.train) %in% c("year")] )

educ.pc.x.val <- data.frame(sapply(educ.pc.x.val, as.numeric))
educ.pc.x.val[!colnames(educ.pc.x.val) %in% c("year")] <- predict(educ.pc.pre.train, educ.pc.x.val[!colnames(educ.pc.x.val) %in% c("year")] ) # use training values for val set 

educ.pc.x.test <- data.frame(sapply(educ.pc.x.test, as.numeric))
educ.pc.x.test[!colnames(educ.pc.x.test) %in% c("year")] <- predict(educ.pc.pre.train, educ.pc.x.test[!colnames(educ.pc.x.test) %in% c("year")] ) # use training values for test set 

# Export each as csv (labels, features)
data.directory <- "~/Dropbox/github/drnns-prediction/data/capacity/analysis-12/treated/"

write.csv(rev.pc.x.train[!colnames(rev.pc.x.train) %in% c("year")], paste0(data.directory,"revpc-x-train.csv"), row.names=FALSE) 
write.csv(rev.pc.x.val[!colnames(rev.pc.x.val) %in% c("year")] , paste0(data.directory,"revpc-x-val.csv"), row.names=FALSE) 
write.csv(rev.pc.x.test[!colnames(rev.pc.x.test) %in% c("year")] , paste0(data.directory,"revpc-x-test.csv"), row.names=FALSE) 
write.csv(rev.pc.y.train[!colnames(rev.pc.y.train) %in% c("year")], paste0(data.directory,"revpc-y-train.csv"), row.names=FALSE) 
write.csv(rev.pc.y.val[!colnames(rev.pc.y.val) %in% c("year")], paste0(data.directory,"revpc-y-val.csv"), row.names=FALSE) 
write.csv(rev.pc.y.test[!colnames(rev.pc.y.test) %in% c("year")], paste0(data.directory,"revpc-y-test.csv"), row.names=FALSE) 

write.csv(exp.pc.x.train[!colnames(exp.pc.x.train) %in% c("year")], paste0(data.directory,"exppc-x-train.csv"), row.names=FALSE) 
write.csv(exp.pc.x.val[!colnames(exp.pc.x.val) %in% c("year")] , paste0(data.directory,"exppc-x-val.csv"), row.names=FALSE) 
write.csv(exp.pc.x.test[!colnames(exp.pc.x.test) %in% c("year")] , paste0(data.directory,"exppc-x-test.csv"), row.names=FALSE) 
write.csv(exp.pc.y.train[!colnames(exp.pc.y.train) %in% c("year")], paste0(data.directory,"exppc-y-train.csv"), row.names=FALSE) 
write.csv(exp.pc.y.val[!colnames(exp.pc.y.val) %in% c("year")], paste0(data.directory,"exppc-y-val.csv"), row.names=FALSE) 
write.csv(exp.pc.y.test[!colnames(exp.pc.y.test) %in% c("year")], paste0(data.directory,"exppc-y-test.csv"), row.names=FALSE) 

write.csv(educ.pc.x.train[!colnames(educ.pc.x.train) %in% c("year")], paste0(data.directory,"educpc-x-train.csv"), row.names=FALSE) 
write.csv(educ.pc.x.val[!colnames(educ.pc.x.val) %in% c("year")] , paste0(data.directory,"educpc-x-val.csv"), row.names=FALSE) 
write.csv(educ.pc.x.test[!colnames(educ.pc.x.test) %in% c("year")] , paste0(data.directory,"educpc-x-test.csv"), row.names=FALSE) 
write.csv(educ.pc.y.train[!colnames(educ.pc.y.train) %in% c("year")], paste0(data.directory,"educpc-y-train.csv"), row.names=FALSE) 
write.csv(educ.pc.y.val[!colnames(educ.pc.y.val) %in% c("year")], paste0(data.directory,"educpc-y-val.csv"), row.names=FALSE) 
write.csv(educ.pc.y.test[!colnames(educ.pc.y.test) %in% c("year")], paste0(data.directory,"educpc-y-test.csv"), row.names=FALSE) 
}

if(analysis==3){
## Analysis 3: Effect of HSA restriction on treated, intervention: Mar 1889
# Treated is southern public land states (not MO)
# Controls are MO, state land states

# Summarize by category

funds$cat <- NA
funds$cat[funds$state %in% southern.pub] <- "Treated"
funds$cat[funds$state %in% c("MO",state.land.states)] <- "Control"

# Create control and treated sums
cats.funds <- funds %>% 
  filter(!is.na(cat)) %>% # rm non-southern state land states
  group_by(year,cat) %>% 
  summarise_each(funs(mean(., na.rm = TRUE))) %>%
  select(-state)

cats.funds.r <- reshape(data.frame(cats.funds), idvar = "year", timevar = "cat", direction = "wide")

funds.control <- funds[!is.na(funds$cat) & funds$cat=="Control",][c("state","year","rev.pc","exp.pc","educ.pc")] # discard treated since we have treated time-series

rev.pc <- reshape(data.frame(funds.control), idvar = "year", timevar = "state", direction = "wide")
exp.pc <- reshape(data.frame(funds.control), idvar = "year", timevar = "state", direction = "wide")
educ.pc <- reshape(data.frame(funds.control), idvar = "year", timevar = "state", direction = "wide")

# Labels

rev.pc.y <- cats.funds.r[c("year", "rev.pc.Treated")]
rev.pc.y <- rev.pc.y[!is.na(rev.pc.y$rev.pc.Treated),]

exp.pc.y <- cats.funds.r[c("year", "exp.pc.Treated")]
exp.pc.y <- exp.pc.y[!is.na(exp.pc.y$exp.pc.Treated),]

educ.pc.y <- cats.funds.r[c("year", "educ.pc.Treated")]
educ.pc.y <- educ.pc.y[!is.na(educ.pc.y$educ.pc.Treated),]

# Splits

rev.pc.years <- intersect(rev.pc$year,rev.pc.y$year) # common rev.pc years in treated and control

rev.pc.x.train <- rev.pc[rev.pc$year %in% rev.pc.years & rev.pc$year < 1886,]
rev.pc.x.val <- rev.pc[rev.pc$year %in% rev.pc.years & (rev.pc$year >= 1886 & rev.pc$year < 1889),] # 1886, 1886, 1888 for validation
rev.pc.x.test <- rev.pc[rev.pc$year %in% rev.pc.years & rev.pc$year >= 1889,]

rev.pc.y.train <- rev.pc.y[rev.pc.y$year %in% rev.pc.years & rev.pc.y$year < 1886,]
rev.pc.y.val <- rev.pc.y[rev.pc.y$year %in% rev.pc.years & (rev.pc.y$year >= 1886 & rev.pc.y$year < 1889),]
rev.pc.y.test <- rev.pc.y[rev.pc.y$year %in% rev.pc.years & rev.pc.y$year >= 1889,]

exp.pc.years <- intersect(exp.pc$year,exp.pc.y$year) # common exp.pc years in treated and control

exp.pc.x.train <- exp.pc[exp.pc$year %in% exp.pc.years & exp.pc$year < 1886,]
exp.pc.x.val <- exp.pc[exp.pc$year %in% exp.pc.years & (exp.pc$year >= 1886 & exp.pc$year < 1889),]
exp.pc.x.test <- exp.pc[exp.pc$year %in% exp.pc.years & exp.pc$year >= 1889,]

exp.pc.y.train <- exp.pc.y[exp.pc.y$year %in% exp.pc.years & exp.pc.y$year < 1886,]
exp.pc.y.val <- exp.pc.y[exp.pc.y$year %in% exp.pc.years & (exp.pc.y$year >= 1886 & exp.pc.y$year < 1889),]
exp.pc.y.test <- exp.pc.y[exp.pc.y$year %in% exp.pc.years & exp.pc.y$year >= 1889,]

educ.pc.years <- intersect(educ.pc$year,educ.pc.y$year) # common educ.pc years in treated and control

educ.pc.x.train <- educ.pc[educ.pc$year %in% educ.pc.years & educ.pc$year < 1886,]
educ.pc.x.val <- educ.pc[educ.pc$year %in% educ.pc.years & (educ.pc$year >= 1886 & educ.pc$year < 1889),]
educ.pc.x.test <- educ.pc[educ.pc$year %in% educ.pc.years & educ.pc$year >= 1889,]

educ.pc.y.train <- educ.pc.y[educ.pc.y$year %in% educ.pc.years & educ.pc.y$year < 1886,]
educ.pc.y.val <- educ.pc.y[educ.pc.y$year %in% educ.pc.years & (educ.pc.y$year >= 1886 & educ.pc.y$year < 1889),]
educ.pc.y.test <- educ.pc.y[educ.pc.y$year %in% educ.pc.years & educ.pc.y$year >= 1889,]

# Preprocess

rev.pc.x.train <- data.frame(sapply(rev.pc.x.train, as.numeric))
rev.pc.x.train[is.na(rev.pc.x.train)] <- 0 # fill NA with 0 before scale
rev.pc.pre.train <- preProcess(rev.pc.x.train[!colnames(rev.pc.x.train) %in% c("year")], method = c("center", "scale","medianImpute"))
rev.pc.x.train[!colnames(rev.pc.x.train) %in% c("year")] <- predict(rev.pc.pre.train, rev.pc.x.train[!colnames(rev.pc.x.train) %in% c("year")] )

rev.pc.x.val <- data.frame(sapply(rev.pc.x.val, as.numeric))
rev.pc.x.val[!colnames(rev.pc.x.val) %in% c("year")] <- predict(rev.pc.pre.train, rev.pc.x.val[!colnames(rev.pc.x.val) %in% c("year")] ) # use training values for val set 

rev.pc.x.test <- data.frame(sapply(rev.pc.x.test, as.numeric))
rev.pc.x.test[!colnames(rev.pc.x.test) %in% c("year")] <- predict(rev.pc.pre.train, rev.pc.x.test[!colnames(rev.pc.x.test) %in% c("year")] ) # use training values for test set 

exp.pc.x.train <- data.frame(sapply(exp.pc.x.train, as.numeric))
exp.pc.x.train[is.na(exp.pc.x.train)] <- 0 # fill NA with 0 before scale
exp.pc.pre.train <- preProcess(exp.pc.x.train[!colnames(exp.pc.x.train) %in% c("year")], method = c("center", "scale","medianImpute"))
exp.pc.x.train[!colnames(exp.pc.x.train) %in% c("year")] <- predict(exp.pc.pre.train, exp.pc.x.train[!colnames(exp.pc.x.train) %in% c("year")] )

exp.pc.x.val <- data.frame(sapply(exp.pc.x.val, as.numeric))
exp.pc.x.val[!colnames(exp.pc.x.val) %in% c("year")] <- predict(exp.pc.pre.train, exp.pc.x.val[!colnames(exp.pc.x.val) %in% c("year")] ) # use training values for val set 

exp.pc.x.test <- data.frame(sapply(exp.pc.x.test, as.numeric))
exp.pc.x.test[!colnames(exp.pc.x.test) %in% c("year")] <- predict(exp.pc.pre.train, exp.pc.x.test[!colnames(exp.pc.x.test) %in% c("year")] ) # use training values for test set 

educ.pc.x.train <- data.frame(sapply(educ.pc.x.train, as.numeric))
educ.pc.x.train[is.na(educ.pc.x.train)] <- 0 # fill NA with 0 before scale
educ.pc.pre.train <- preProcess(educ.pc.x.train[!colnames(educ.pc.x.train) %in% c("year")], method = c("center", "scale","medianImpute"))
educ.pc.x.train[!colnames(educ.pc.x.train) %in% c("year")] <- predict(educ.pc.pre.train, educ.pc.x.train[!colnames(educ.pc.x.train) %in% c("year")] )

educ.pc.x.val <- data.frame(sapply(educ.pc.x.val, as.numeric))
educ.pc.x.val[!colnames(educ.pc.x.val) %in% c("year")] <- predict(educ.pc.pre.train, educ.pc.x.val[!colnames(educ.pc.x.val) %in% c("year")] ) # use training values for val set 

educ.pc.x.test <- data.frame(sapply(educ.pc.x.test, as.numeric))
educ.pc.x.test[!colnames(educ.pc.x.test) %in% c("year")] <- predict(educ.pc.pre.train, educ.pc.x.test[!colnames(educ.pc.x.test) %in% c("year")] ) # use training values for test set 

# Export each as csv (labels, features)
data.directory <- "~/Dropbox/github/drnns-prediction/data/capacity/analysis-34/treated/"

write.csv(rev.pc.x.train[!colnames(rev.pc.x.train) %in% c("year")], paste0(data.directory,"revpc-x-train.csv"), row.names=FALSE) 
write.csv(rev.pc.x.val[!colnames(rev.pc.x.val) %in% c("year")] , paste0(data.directory,"revpc-x-val.csv"), row.names=FALSE) 
write.csv(rev.pc.x.test[!colnames(rev.pc.x.test) %in% c("year")] , paste0(data.directory,"revpc-x-test.csv"), row.names=FALSE) 
write.csv(rev.pc.y.train[!colnames(rev.pc.y.train) %in% c("year")], paste0(data.directory,"revpc-y-train.csv"), row.names=FALSE) 
write.csv(rev.pc.y.val[!colnames(rev.pc.y.val) %in% c("year")], paste0(data.directory,"revpc-y-val.csv"), row.names=FALSE) 
write.csv(rev.pc.y.test[!colnames(rev.pc.y.test) %in% c("year")], paste0(data.directory,"revpc-y-test.csv"), row.names=FALSE) 

write.csv(exp.pc.x.train[!colnames(exp.pc.x.train) %in% c("year")], paste0(data.directory,"exppc-x-train.csv"), row.names=FALSE) 
write.csv(exp.pc.x.val[!colnames(exp.pc.x.val) %in% c("year")] , paste0(data.directory,"exppc-x-val.csv"), row.names=FALSE) 
write.csv(exp.pc.x.test[!colnames(exp.pc.x.test) %in% c("year")] , paste0(data.directory,"exppc-x-test.csv"), row.names=FALSE) 
write.csv(exp.pc.y.train[!colnames(exp.pc.y.train) %in% c("year")], paste0(data.directory,"exppc-y-train.csv"), row.names=FALSE) 
write.csv(exp.pc.y.val[!colnames(exp.pc.y.val) %in% c("year")], paste0(data.directory,"exppc-y-val.csv"), row.names=FALSE) 
write.csv(exp.pc.y.test[!colnames(exp.pc.y.test) %in% c("year")], paste0(data.directory,"exppc-y-test.csv"), row.names=FALSE) 

write.csv(educ.pc.x.train[!colnames(educ.pc.x.train) %in% c("year")], paste0(data.directory,"educpc-x-train.csv"), row.names=FALSE) 
write.csv(educ.pc.x.val[!colnames(educ.pc.x.val) %in% c("year")] , paste0(data.directory,"educpc-x-val.csv"), row.names=FALSE) 
write.csv(educ.pc.x.test[!colnames(educ.pc.x.test) %in% c("year")] , paste0(data.directory,"educpc-x-test.csv"), row.names=FALSE) 
write.csv(educ.pc.y.train[!colnames(educ.pc.y.train) %in% c("year")], paste0(data.directory,"educpc-y-train.csv"), row.names=FALSE) 
write.csv(educ.pc.y.val[!colnames(educ.pc.y.val) %in% c("year")], paste0(data.directory,"educpc-y-val.csv"), row.names=FALSE) 
write.csv(educ.pc.y.test[!colnames(educ.pc.y.test) %in% c("year")], paste0(data.directory,"educpc-y-test.csv"), row.names=FALSE) 
}

if(analysis==4){
  ## Analysis 4: Effect of HSA restriction on treated, intervention: Mar 1889
  # Treated is western public land states (not MO)
  # Controls are MO, state land states
  
  # Summarize by category
  
  funds$cat <- NA
  funds$cat[funds$state %in% setdiff(setdiff(pub.states,southern.pub), "MO")] <- "Treated"
  funds$cat[funds$state %in% c("MO",state.land.states)] <- "Control"
  
  # Create control and treated sums
  cats.funds <- funds %>% 
    filter(!is.na(cat)) %>% # rm non-southern state land states
    group_by(year,cat) %>% 
    summarise_each(funs(mean(., na.rm = TRUE))) %>%
    select(-state)
  
  cats.funds.r <- reshape(data.frame(cats.funds), idvar = "year", timevar = "cat", direction = "wide")
  
  funds.control <- funds[!is.na(funds$cat) & funds$cat=="Control",][c("state","year","rev.pc","exp.pc","educ.pc")] # discard treated since we have treated time-series
  
  rev.pc <- reshape(data.frame(funds.control), idvar = "year", timevar = "state", direction = "wide")
  exp.pc <- reshape(data.frame(funds.control), idvar = "year", timevar = "state", direction = "wide")
  educ.pc <- reshape(data.frame(funds.control), idvar = "year", timevar = "state", direction = "wide")
  
  # Labels
  
  rev.pc.y <- cats.funds.r[c("year", "rev.pc.Treated")]
  rev.pc.y <- rev.pc.y[!is.na(rev.pc.y$rev.pc.Treated),]
  
  exp.pc.y <- cats.funds.r[c("year", "exp.pc.Treated")]
  exp.pc.y <- exp.pc.y[!is.na(exp.pc.y$exp.pc.Treated),]
  
  educ.pc.y <- cats.funds.r[c("year", "educ.pc.Treated")]
  educ.pc.y <- educ.pc.y[!is.na(educ.pc.y$educ.pc.Treated),]
  
  # Splits
  
  rev.pc.years <- intersect(rev.pc$year,rev.pc.y$year) # common rev.pc years in treated and control
  
  rev.pc.x.train <- rev.pc[rev.pc$year %in% rev.pc.years & rev.pc$year < 1886,]
  rev.pc.x.val <- rev.pc[rev.pc$year %in% rev.pc.years & (rev.pc$year >= 1886 & rev.pc$year < 1889),] # 1886, 1886, 1888 for validation
  rev.pc.x.test <- rev.pc[rev.pc$year %in% rev.pc.years & rev.pc$year >= 1889,]
  
  rev.pc.y.train <- rev.pc.y[rev.pc.y$year %in% rev.pc.years & rev.pc.y$year < 1886,]
  rev.pc.y.val <- rev.pc.y[rev.pc.y$year %in% rev.pc.years & (rev.pc.y$year >= 1886 & rev.pc.y$year < 1889),]
  rev.pc.y.test <- rev.pc.y[rev.pc.y$year %in% rev.pc.years & rev.pc.y$year >= 1889,]
  
  exp.pc.years <- intersect(exp.pc$year,exp.pc.y$year) # common exp.pc years in treated and control
  
  exp.pc.x.train <- exp.pc[exp.pc$year %in% exp.pc.years & exp.pc$year < 1886,]
  exp.pc.x.val <- exp.pc[exp.pc$year %in% exp.pc.years & (exp.pc$year >= 1886 & exp.pc$year < 1889),]
  exp.pc.x.test <- exp.pc[exp.pc$year %in% exp.pc.years & exp.pc$year >= 1889,]
  
  exp.pc.y.train <- exp.pc.y[exp.pc.y$year %in% exp.pc.years & exp.pc.y$year < 1886,]
  exp.pc.y.val <- exp.pc.y[exp.pc.y$year %in% exp.pc.years & (exp.pc.y$year >= 1886 & exp.pc.y$year < 1889),]
  exp.pc.y.test <- exp.pc.y[exp.pc.y$year %in% exp.pc.years & exp.pc.y$year >= 1889,]
  
  educ.pc.years <- intersect(educ.pc$year,educ.pc.y$year) # common educ.pc years in treated and control
  
  educ.pc.x.train <- educ.pc[educ.pc$year %in% educ.pc.years & educ.pc$year < 1886,]
  educ.pc.x.val <- educ.pc[educ.pc$year %in% educ.pc.years & (educ.pc$year >= 1886 & educ.pc$year < 1889),]
  educ.pc.x.test <- educ.pc[educ.pc$year %in% educ.pc.years & educ.pc$year >= 1889,]
  
  educ.pc.y.train <- educ.pc.y[educ.pc.y$year %in% educ.pc.years & educ.pc.y$year < 1886,]
  educ.pc.y.val <- educ.pc.y[educ.pc.y$year %in% educ.pc.years & (educ.pc.y$year >= 1886 & educ.pc.y$year < 1889),]
  educ.pc.y.test <- educ.pc.y[educ.pc.y$year %in% educ.pc.years & educ.pc.y$year >= 1889,]
  
  # Preprocess
  
  rev.pc.x.train <- data.frame(sapply(rev.pc.x.train, as.numeric))
  rev.pc.x.train[is.na(rev.pc.x.train)] <- 0 # fill NA with 0 before scale
  rev.pc.pre.train <- preProcess(rev.pc.x.train[!colnames(rev.pc.x.train) %in% c("year")], method = c("center", "scale","medianImpute"))
  rev.pc.x.train[!colnames(rev.pc.x.train) %in% c("year")] <- predict(rev.pc.pre.train, rev.pc.x.train[!colnames(rev.pc.x.train) %in% c("year")] )
  
  rev.pc.x.val <- data.frame(sapply(rev.pc.x.val, as.numeric))
  rev.pc.x.val[!colnames(rev.pc.x.val) %in% c("year")] <- predict(rev.pc.pre.train, rev.pc.x.val[!colnames(rev.pc.x.val) %in% c("year")] ) # use training values for val set 
  
  rev.pc.x.test <- data.frame(sapply(rev.pc.x.test, as.numeric))
  rev.pc.x.test[!colnames(rev.pc.x.test) %in% c("year")] <- predict(rev.pc.pre.train, rev.pc.x.test[!colnames(rev.pc.x.test) %in% c("year")] ) # use training values for test set 
  
  exp.pc.x.train <- data.frame(sapply(exp.pc.x.train, as.numeric))
  exp.pc.x.train[is.na(exp.pc.x.train)] <- 0 # fill NA with 0 before scale
  exp.pc.pre.train <- preProcess(exp.pc.x.train[!colnames(exp.pc.x.train) %in% c("year")], method = c("center", "scale","medianImpute"))
  exp.pc.x.train[!colnames(exp.pc.x.train) %in% c("year")] <- predict(exp.pc.pre.train, exp.pc.x.train[!colnames(exp.pc.x.train) %in% c("year")] )
  
  exp.pc.x.val <- data.frame(sapply(exp.pc.x.val, as.numeric))
  exp.pc.x.val[!colnames(exp.pc.x.val) %in% c("year")] <- predict(exp.pc.pre.train, exp.pc.x.val[!colnames(exp.pc.x.val) %in% c("year")] ) # use training values for val set 
  
  exp.pc.x.test <- data.frame(sapply(exp.pc.x.test, as.numeric))
  exp.pc.x.test[!colnames(exp.pc.x.test) %in% c("year")] <- predict(exp.pc.pre.train, exp.pc.x.test[!colnames(exp.pc.x.test) %in% c("year")] ) # use training values for test set 
  
  educ.pc.x.train <- data.frame(sapply(educ.pc.x.train, as.numeric))
  educ.pc.x.train[is.na(educ.pc.x.train)] <- 0 # fill NA with 0 before scale
  educ.pc.pre.train <- preProcess(educ.pc.x.train[!colnames(educ.pc.x.train) %in% c("year")], method = c("center", "scale","medianImpute"))
  educ.pc.x.train[!colnames(educ.pc.x.train) %in% c("year")] <- predict(educ.pc.pre.train, educ.pc.x.train[!colnames(educ.pc.x.train) %in% c("year")] )
  
  educ.pc.x.val <- data.frame(sapply(educ.pc.x.val, as.numeric))
  educ.pc.x.val[!colnames(educ.pc.x.val) %in% c("year")] <- predict(educ.pc.pre.train, educ.pc.x.val[!colnames(educ.pc.x.val) %in% c("year")] ) # use training values for val set 
  
  educ.pc.x.test <- data.frame(sapply(educ.pc.x.test, as.numeric))
  educ.pc.x.test[!colnames(educ.pc.x.test) %in% c("year")] <- predict(educ.pc.pre.train, educ.pc.x.test[!colnames(educ.pc.x.test) %in% c("year")] ) # use training values for test set 
  
  # Export each as csv (labels, features)
  data.directory <- "~/Dropbox/github/drnns-prediction/data/capacity/analysis-41/treated/"
  
  write.csv(rev.pc.x.train[!colnames(rev.pc.x.train) %in% c("year")], paste0(data.directory,"revpc-x-train.csv"), row.names=FALSE) 
  write.csv(rev.pc.x.val[!colnames(rev.pc.x.val) %in% c("year")] , paste0(data.directory,"revpc-x-val.csv"), row.names=FALSE) 
  write.csv(rev.pc.x.test[!colnames(rev.pc.x.test) %in% c("year")] , paste0(data.directory,"revpc-x-test.csv"), row.names=FALSE) 
  write.csv(rev.pc.y.train[!colnames(rev.pc.y.train) %in% c("year")], paste0(data.directory,"revpc-y-train.csv"), row.names=FALSE) 
  write.csv(rev.pc.y.val[!colnames(rev.pc.y.val) %in% c("year")], paste0(data.directory,"revpc-y-val.csv"), row.names=FALSE) 
  write.csv(rev.pc.y.test[!colnames(rev.pc.y.test) %in% c("year")], paste0(data.directory,"revpc-y-test.csv"), row.names=FALSE) 
  
  write.csv(exp.pc.x.train[!colnames(exp.pc.x.train) %in% c("year")], paste0(data.directory,"exppc-x-train.csv"), row.names=FALSE) 
  write.csv(exp.pc.x.val[!colnames(exp.pc.x.val) %in% c("year")] , paste0(data.directory,"exppc-x-val.csv"), row.names=FALSE) 
  write.csv(exp.pc.x.test[!colnames(exp.pc.x.test) %in% c("year")] , paste0(data.directory,"exppc-x-test.csv"), row.names=FALSE) 
  write.csv(exp.pc.y.train[!colnames(exp.pc.y.train) %in% c("year")], paste0(data.directory,"exppc-y-train.csv"), row.names=FALSE) 
  write.csv(exp.pc.y.val[!colnames(exp.pc.y.val) %in% c("year")], paste0(data.directory,"exppc-y-val.csv"), row.names=FALSE) 
  write.csv(exp.pc.y.test[!colnames(exp.pc.y.test) %in% c("year")], paste0(data.directory,"exppc-y-test.csv"), row.names=FALSE) 
  
  write.csv(educ.pc.x.train[!colnames(educ.pc.x.train) %in% c("year")], paste0(data.directory,"educpc-x-train.csv"), row.names=FALSE) 
  write.csv(educ.pc.x.val[!colnames(educ.pc.x.val) %in% c("year")] , paste0(data.directory,"educpc-x-val.csv"), row.names=FALSE) 
  write.csv(educ.pc.x.test[!colnames(educ.pc.x.test) %in% c("year")] , paste0(data.directory,"educpc-x-test.csv"), row.names=FALSE) 
  write.csv(educ.pc.y.train[!colnames(educ.pc.y.train) %in% c("year")], paste0(data.directory,"educpc-y-train.csv"), row.names=FALSE) 
  write.csv(educ.pc.y.val[!colnames(educ.pc.y.val) %in% c("year")], paste0(data.directory,"educpc-y-val.csv"), row.names=FALSE) 
  write.csv(educ.pc.y.test[!colnames(educ.pc.y.test) %in% c("year")], paste0(data.directory,"educpc-y-test.csv"), row.names=FALSE) 
}