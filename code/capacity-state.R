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

funds <- merge(funds, census.ts.state[c('year','state','ns.pop',"land.gini","aland.gini","ns.pop","adultm","farms","farmsize","tenancy","wages","output")], 
               by.x=c('year2','state'), by.y=c('year','state'),all.x=TRUE)

funds["rev.pc"] <- NA
funds["rev.pc"] <- log(funds["1"]/funds$ns.pop+ .Machine$double.eps)

funds["exp.pc"] <- NA
funds["exp.pc"] <- log(funds["3"]/funds$ns.pop+ .Machine$double.eps)

funds["educ.pc"] <- NA
funds["educ.pc"] <- log(funds["31"]/funds$ns.pop+ .Machine$double.eps)

# clean feature set
funds <- funds[colnames(funds) %in% c("state","year","year2",
                                      'ns.pop',"land.gini","aland.gini","ns.pop","adultm","farms","farmsize","tenancy","wages","output",
                                      "rev.pc","exp.pc","educ.pc")]