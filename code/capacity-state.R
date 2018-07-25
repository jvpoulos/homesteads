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
require(readr)
require(imputeTS)

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

# Get historical CPI

USCPI_1783_1982 <- read_csv(paste0(data.directory,"USCPI_1783-1982.csv"))

USCPI_1783_1982$adj_factor <- USCPI_1783_1982$`U.S. Consumer Price Index`/USCPI_1783_1982$`U.S. Consumer Price Index`[USCPI_1783_1982$Year == 1982] # adj. factor relative to 1982

funds <- merge(funds, USCPI_1783_1982, by.x="year", by.y="Year")

## Make log per-capita measures

funds$year2 <- signif(funds$year,3) # merge by nearest decennial
funds$year2[funds$year<=1785] <- 1790 # VA
funds$year2[funds$year2<1880 & funds$state=="AK"] <- 1880
funds$year2[funds$year2==1980] <- 1983

funds <- merge(funds, census.ts.state[c('year','state','ns.pop',"land.gini","aland.gini","ns.pop","adultm","farms","farmsize","tenancy","wages","output")], 
               by.x=c('year2','state'), by.y=c('year','state'),all.x=TRUE)

funds["rev.pc"] <- NA
funds["rev.pc"] <- log((funds["1"]/funds$adj_factor)/funds$ns.pop+ .Machine$double.eps)

funds["exp.pc"] <- NA
funds["exp.pc"] <- log((funds["3"]/funds$adj_factor)/funds$ns.pop+ .Machine$double.eps)

funds["educ.pc"] <- NA
funds["educ.pc"] <- log((funds["31"]/funds$adj_factor)/funds$ns.pop+ .Machine$double.eps)

# clean feature set
funds <- funds[colnames(funds) %in% c("state","year","year2",
                                      'ns.pop',"land.gini","aland.gini","ns.pop","adultm","farms","farmsize","tenancy","wages","output",
                                      "rev.pc","exp.pc","educ.pc")]

# reshape and split for RNNs

# Summarize by category

funds$cat <- NA
funds$cat[funds$state %in% setdiff(pub.states,southern.pub)] <- "Treated.West"
funds$cat[funds$state %in% setdiff(pub.states,western.pub)] <- "Treated.South"
funds$cat[funds$state %in% state.land.states] <- "Control"

# Create control and treated means
cats.funds <- funds %>% 
  filter(!is.na(cat)) %>% 
  group_by(year,cat) %>% 
  summarise_each(funs(mean(., na.rm = TRUE))) %>%
  dplyr::select(-state)

cats.funds.r <- reshape(data.frame(cats.funds), idvar = "year", timevar = "cat", direction = "wide")

funds.control <- funds[!is.na(funds$cat) & funds$cat=="Control",][c("state","year","rev.pc","exp.pc","educ.pc")] # discard treated since we have treated time-series

rev.pc <- reshape(data.frame(funds.control), idvar = "year", timevar = "state", direction = "wide")
exp.pc <- reshape(data.frame(funds.control), idvar = "year", timevar = "state", direction = "wide")
educ.pc <- reshape(data.frame(funds.control), idvar = "year", timevar = "state", direction = "wide")

#Labels

# west
rev.pc.y.west <- cats.funds.r[c("year", "rev.pc.Treated.West")]
rev.pc.y.west <- rev.pc.y.west[!is.na(rev.pc.y.west$rev.pc.Treated.West),]

exp.pc.y.west <- cats.funds.r[c("year", "exp.pc.Treated.West")]
exp.pc.y.west <- exp.pc.y.west[!is.na(exp.pc.y.west$exp.pc.Treated.West),]

educ.pc.y.west <- cats.funds.r[c("year", "educ.pc.Treated.West")]
educ.pc.y.west <- educ.pc.y.west[!is.na(educ.pc.y.west$educ.pc.Treated.West),]

# south
rev.pc.y.south <- cats.funds.r[c("year", "rev.pc.Treated.South")]
rev.pc.y.south <- rev.pc.y.south[!is.na(rev.pc.y.south$rev.pc.Treated.South),]

exp.pc.y.south <- cats.funds.r[c("year", "exp.pc.Treated.South")]
exp.pc.y.south <- exp.pc.y.south[!is.na(exp.pc.y.south$exp.pc.Treated.South),]

educ.pc.y.south <- cats.funds.r[c("year", "educ.pc.Treated.South")]
educ.pc.y.south <- educ.pc.y.south[!is.na(educ.pc.y.south$educ.pc.Treated.South),]

# Splits

rev.pc.years.west <- intersect(rev.pc$year,rev.pc.y.west$year) # common rev.pc years in treated and control
rev.pc.years.south <- intersect(rev.pc$year,rev.pc.y.south$year) 

rev.pc.x.west <- rev.pc[rev.pc$year %in% rev.pc.years.west,]
rev.pc.x.south <- rev.pc[rev.pc$year %in% rev.pc.years.south,]

rev.pc.y.west <- rev.pc.y.west[rev.pc.y.west$year %in% rev.pc.years.west,]
rev.pc.y.south <- rev.pc.y.south[rev.pc.y.south$year %in% rev.pc.years.south,]

exp.pc.years.west <- intersect(exp.pc$year,exp.pc.y.west$year) # common exp.pc years in treated and control
exp.pc.years.south <- intersect(exp.pc$year,exp.pc.y.south$year) 

exp.pc.x.west <- exp.pc[exp.pc$year %in% exp.pc.years.west,]
exp.pc.x.south <- exp.pc[exp.pc$year %in% exp.pc.years.south,]

exp.pc.y.west <- exp.pc.y.west[exp.pc.y.west$year %in% exp.pc.years.west,]
exp.pc.y.south <- exp.pc.y.south[exp.pc.y.south$year %in% exp.pc.years.south,]

educ.pc.years.west <- intersect(educ.pc$year,educ.pc.y.west$year) # common educ.pc years in treated and control
educ.pc.years.south <- intersect(educ.pc$year,educ.pc.y.south$year) 

educ.pc.x.west <- educ.pc[educ.pc$year %in% educ.pc.years.west,]
educ.pc.x.south <- educ.pc[educ.pc$year %in% educ.pc.years.south,]

educ.pc.y.west <- educ.pc.y.west[educ.pc.y.west$year %in% educ.pc.years.west,]
educ.pc.y.south <- educ.pc.y.south[educ.pc.y.south$year %in% educ.pc.years.south,]

# Impute missing control values via linear interpolation 

rev.pc.x.west.imp <- na.interpolation(rev.pc.x.west, option = "linear")
rev.pc.x.south.imp <- na.interpolation(rev.pc.x.south, option = "linear")

exp.pc.x.west.imp <- na.interpolation(exp.pc.x.west, option = "linear")
exp.pc.x.south.imp <- na.interpolation(exp.pc.x.south, option = "linear")

educ.pc.x.west.imp <- na.interpolation(educ.pc.x.west, option = "linear")
educ.pc.x.south.imp <- na.interpolation(educ.pc.x.south, option = "linear")

# Export each as csv (labels, features)
data.directory <- "~/Dropbox/github/land-reform/data/"

write.csv(rev.pc.x.west.imp[!colnames(rev.pc.x.west.imp) %in% c("year")], paste0(data.directory,"funds/west-revpc-x.csv"), row.names=FALSE) 
write.csv(rev.pc.x.south.imp[!colnames(rev.pc.x.south.imp) %in% c("year")], paste0(data.directory,"funds/south-revpc-x.csv"), row.names=FALSE) 

write.csv(exp.pc.x.west.imp[!colnames(exp.pc.x.west.imp) %in% c("year")], paste0(data.directory,"funds/west-exppc-x.csv"), row.names=FALSE) 
write.csv(exp.pc.x.south.imp[!colnames(exp.pc.x.south.imp) %in% c("year")], paste0(data.directory,"funds/south-exppc-x.csv"), row.names=FALSE) 

write.csv(educ.pc.x.west.imp[!colnames(educ.pc.x.west.imp) %in% c("year")], paste0(data.directory,"funds/west-educpc-x.csv"), row.names=FALSE) 
write.csv(educ.pc.x.south.imp[!colnames(educ.pc.x.south.imp) %in% c("year")], paste0(data.directory,"funds/south-educpc-x.csv"), row.names=FALSE) 

write.csv(rev.pc.y.west[!colnames(rev.pc.y.west) %in% c("year")], paste0(data.directory,"funds/west-revpc-y.csv"), row.names=FALSE) 
write.csv(rev.pc.y.south[!colnames(rev.pc.y.south) %in% c("year")], paste0(data.directory,"funds/south-revpc-y.csv"), row.names=FALSE) 

write.csv(exp.pc.y.west[!colnames(exp.pc.y.west) %in% c("year")], paste0(data.directory,"funds/west-exppc-y.csv"), row.names=FALSE) 
write.csv(exp.pc.y.south[!colnames(exp.pc.y.south) %in% c("year")], paste0(data.directory,"funds/south-exppc-y.csv"), row.names=FALSE) 

write.csv(educ.pc.y.west[!colnames(educ.pc.y.west) %in% c("year")], paste0(data.directory,"funds/west-educpc-y.csv"), row.names=FALSE) 
write.csv(educ.pc.y.south[!colnames(educ.pc.y.south) %in% c("year")], paste0(data.directory,"funds/south-educpc-y.csv"), row.names=FALSE) 