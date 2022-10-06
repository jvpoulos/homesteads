############################################################################
# Prepare state capacity data                                             #
############################################################################

require(data.table)
require(reshape2)
require(stringr)
require(dplyr)
require(weights)
require(tidyr)
require(readr)
require(imputeTS)
require(mice)
require(mtsdi)
require(Amelia)
require(missForest)
require(caret)
require(VIM)
require(Hmisc)

homesteads <- TRUE # set to FALSE for rnns-causal project

iso.funds <- c(1,3,31) # target iso codes

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

funds.9728$`1`[is.nan(funds.9728$`1`)] <- NA # make NaN NA
funds.9728$`3`[is.nan(funds.9728$`3`)] <- NA 
funds.9728$`31`[is.nan(funds.9728$`31`)] <- NA 

# Clean state codes
funds.9728$state[funds.9728$state=="MIS"] <- "MS"
funds.9728$state[funds.9728$state=="IO"] <- "IA" 
funds.9728$state[funds.9728$state=="KA"] <- "KS" 

# ME data is in thousands

funds.9728$`1`[funds.9728$state=="ME"] <- funds.9728$`1`[funds.9728$state=="ME"]*1000
funds.9728$`3`[funds.9728$state=="ME"] <- funds.9728$`3`[funds.9728$state=="ME"]*1000
funds.9728$`31`[funds.9728$state=="ME"] <- funds.9728$`31`[funds.9728$state=="ME"]*1000

# RI negative exp value

funds.9728$`3`[funds.9728$state=="RI" & funds.9728$`3` < 0] <- abs(funds.9728$`3`[funds.9728$state=="RI" & funds.9728$`3` < 0])

# Zeros are NA

funds.9728$`1`[funds.9728$`1` == 0] <- NA
funds.9728$`3`[funds.9728$`3` == 0] <- NA
funds.9728$`31`[funds.9728$`31` == 0] <- NA

# Merge pop. data

funds.9728$year2 <- signif(funds.9728$year,3) # merge by nearest decennial
funds.9728$year2[funds.9728$year<=1785] <- 1790 # VA
funds.9728$year2[funds.9728$year2<1880 & funds.9728$state=="AK"] <- 1880
funds.9728$year2[funds.9728$year2==1980] <- 1983

if(homesteads){
  funds.9728 <- merge(funds.9728, census.ts.state[c('year','state','ns.pop',"land.gini","aland.gini","adultm","farms","farmsize","tenancy","wages","output","slave.share","aa.share","native.share","white.share")],
                      by.x=c('year2','state'), by.y=c('year','state'),all.x=TRUE)
} else{
  funds.9728 <- merge(funds.9728, census.ts.state[c('year','state','ns.pop')],
               by.x=c('year2','state'), by.y=c('year','state'),all.x=TRUE)
}

funds.9728$ns.pop[funds.9728$state=="AK" & funds.9728$year2<1900] <- funds.9728$ns.pop[funds.9728$state=="AK" & funds.9728$year2==1900][1] # assume pre-1900 AK pop = 1900 pop. 

## State and Local Government [United States]: Sources and Uses of Funds, State Financial Statistics, 1933-1937 (ICPSR 6306)

# Set location of files
setwd("../../../")
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

funds.6306 <- funds.6306[funds.6306$iso %in% iso.funds,] 

# Make data wide

funds.6306$row <- 1:nrow(funds.6306)
funds.6306 <- spread(funds.6306, key = iso, value = value)

# Collapse by state/year

funds.6306 <- funds.6306 %>% 
  group_by(state,year) %>% 
  summarise_each(funs(mean(., na.rm = TRUE))) 

funds.6306$`1`[is.nan(funds.6306$`1`)] <- NA # make NaN NA
funds.6306$`3`[is.nan(funds.6306$`3`)] <- NA 
funds.6306$`31`[is.nan(funds.6306$`31`)] <- NA 

# Clean state codes
funds.6306$state[funds.6306$state=="NB"] <- "NE"

# Merge pop. data

funds.6306$year2 <- signif(funds.6306$year,3) # merge by nearest decennial
funds.6306$year2[funds.6306$year<=1785] <- 1790 # VA
funds.6306$year2[funds.6306$year2<1880 & funds.6306$state=="AK"] <- 1880
funds.6306$year2[funds.6306$year2==1980] <- 1983

if(homesteads){
  funds.6306 <- merge(funds.6306, census.ts.state[c('year','state','ns.pop',"land.gini","aland.gini","adultm","farms","farmsize","tenancy","wages","output","slave.share","aa.share","native.share","white.share")], 
                      by.x=c('year2','state'), by.y=c('year','state'),all.x=TRUE)
}else{
  funds.6306 <- merge(funds.6306, census.ts.state[c('year','state','ns.pop')], 
                      by.x=c('year2','state'), by.y=c('year','state'),all.x=TRUE)
}

funds.6306$ns.pop[funds.6306$state=="AK" & funds.6306$year2<1900] <- funds.6306$ns.pop[funds.6306$state=="AK" & funds.6306$year2==1900][1] # assume pre-1900 AK pop = 1900 pop. 

## State and Local Government: Sources and Uses of Funds, 1902, 1913, 1932, 1942, 1962, 1972, and 1982 (ICPSR 6304)

# Set location of files
setwd("../../../")
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

funds.6304$value[funds.6304$year>=1932] <- funds.6304$value[funds.6304$year>=1932]*1000 # 1932 to 1982 census are reported in thousands of dollars

# Make data wide

funds.6304$row <- 1:nrow(funds.6304)
funds.6304 <- spread(funds.6304, key = iso, value = value)

# Collapse by state/year

funds.6304 <- funds.6304 %>% 
  select(-type) %>% 
  group_by(state,year) %>% 
  summarise_each(funs(mean(., na.rm = TRUE))) 

funds.6304$`1`[is.nan(funds.6304$`1`)] <- NA # make NaN NA
funds.6304$`3`[is.nan(funds.6304$`3`)] <- NA 
funds.6304$`31`[is.nan(funds.6304$`31`)] <- NA 

# Merge pop. data

funds.6304$year2 <- signif(funds.6304$year,3) # merge by nearest decennial
funds.6304$year2[funds.6304$year<=1785] <- 1790 # VA
funds.6304$year2[funds.6304$year2<1880 & funds.6304$state=="AK"] <- 1880
funds.6304$year2[funds.6304$year2==1980] <- 1983

if(homesteads){
  funds.6304 <- merge(funds.6304, census.ts.state[c('year','state','ns.pop',"land.gini","aland.gini","adultm","farms","farmsize","tenancy","wages","output","slave.share","aa.share","native.share","white.share")], 
                      by.x=c('year2','state'), by.y=c('year','state'),all.x=TRUE)
}else{
  funds.6304 <- merge(funds.6304, census.ts.state[c('year','state','ns.pop')], 
                      by.x=c('year2','state'), by.y=c('year','state'),all.x=TRUE)
}

funds.6304$ns.pop[funds.6304$state=="AK" & funds.6304$year2<1900] <- funds.6304$ns.pop[funds.6304$state=="AK" & funds.6304$year2==1900][1] # assume pre-1900 AK pop = 1900 pop. 

#  Data Base on Historical Finances of State Governments: "State_Govt_Finances"	 1942, 1944, 1946, 1948, and 1950 (limited detail) and 1951 to 2008

setwd("../../../")
State_Govt_Fin <- mdb.get('data/capacity/State_Govt_Fin/State_Govt_Finances.mdb')
State_Govt_Fin_Revenues <- State_Govt_Fin$`1_Revenues` # Total.Revenue
State_Govt_Fin_Expenditures <- State_Govt_Fin$`2_ExpendituresA` # Total.Expenditure # Total.Educ.Total.Exp

funds.State_Govt_Fin <- merge(State_Govt_Fin_Revenues,State_Govt_Fin_Expenditures, by=c("State.Code","Year4"), all=TRUE)
funds.State_Govt_Fin <- merge(funds.State_Govt_Fin, State_Govt_Fin$Reference, by=c("State.Code"), all.x=TRUE)  # get state abbr

funds.State_Govt_Fin <- funds.State_Govt_Fin[,c("State.Abbrv","Year4","Population..000.","Total.Revenue","Total.Expenditure","Total.Educ.Total.Exp")][funds.State_Govt_Fin$State.Code!=0,]

funds.State_Govt_Fin <- tibble("year2"=signif(as.numeric(funds.State_Govt_Fin$Year4),3), # merge by nearest decennial ,
                        "state"= as.character(funds.State_Govt_Fin$State.Abbrv), # put in same format as other datasets
                        "year"=as.numeric(funds.State_Govt_Fin$Year4),
                        "row"=1:nrow(funds.State_Govt_Fin),
                        `1`=  as.numeric(funds.State_Govt_Fin$Total.Revenue) *1000, # reported in thousands of dollars,
                        `3`= as.numeric(funds.State_Govt_Fin$Total.Expenditure) *1000,
                        `31` = as.numeric(funds.State_Govt_Fin$Total.Educ.Total.Exp) *1000,
                        "ns.pop"=as.numeric(funds.State_Govt_Fin$Population..000.)*1000)

# Data Base on Historical Finances of Federal, State and Local Governments: "HIST_FIN" Fiscal Years 1902 - 2008 (national totals of state government finances)

Hist_Fin <- mdb.get('data/capacity/hist_fin/Hist_Fin.mdb')
funds.Hist_Fin <- Hist_Fin$`4_State Governments` 
funds.Hist_Fin <- funds.Hist_Fin[funds.Hist_Fin$Id%in%c(62,101,301,406),] # "Population (000)", 'Total Revenue', 'Total Expenditure', 'Total Educ-Total Exp'
funds.Hist_Fin <- funds.Hist_Fin[-c(1:4,6)]

funds.Hist_Fin <- tibble("state"= rep("US",length(funds.Hist_Fin)-1), # put in same format as other datasets
                               "year"= as.numeric(gsub("STA.","",colnames(funds.Hist_Fin)[2:length(funds.Hist_Fin)])),
                               `1`=  as.numeric(funds.Hist_Fin[funds.Hist_Fin$Name=="Total Revenue",][,-c(1)]) *1000, # reported in thousands of dollars
                               `3`= as.numeric(funds.Hist_Fin[funds.Hist_Fin$Name=="Total Expenditure",][,-c(1)]) *1000,
                               `31` = as.numeric(funds.Hist_Fin[funds.Hist_Fin$Name=="Total Educ-Total Exp",][,-c(1)]) *1000,
                               "ns.pop"= as.numeric(funds.Hist_Fin[funds.Hist_Fin$Name=="Population (000)",][,-c(1)]) *1000)

## Append datasets

funds <- bind_rows(funds.9728,funds.6306,funds.6304,funds.State_Govt_Fin) %>% 
  group_by(state,year) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)))  # takes mean of duplicates from 1902,1913,1932,1942,1962,1972,1982 

funds <- funds[with(funds, order(state, year)), ] # order by state and year

funds$`1`[is.nan(funds$`1`)] <- NA # make NaN NA
funds$`3`[is.nan(funds$`3`)] <- NA
funds$`31`[is.nan(funds$`31`)] <- NA

funds <- funds[funds$state!="DC",]

funds.us <- funds.Hist_Fin  # national totals

funds.us <- funds.us[with(funds.us, order(state, year)), ] # order by state and year

funds.us$`1`[is.nan(funds.us$`1`)] <- NA # make NaN NA
funds.us$`3`[is.nan(funds.us$`3`)] <- NA
funds.us$`31`[is.nan(funds.us$`31`)] <- NA

# Interpolate revenue (1) based on actual expenditure (3)

funds <- funds %>%
  group_by(state,year) %>%
  mutate(rev.share = sum(`1`)/sum(`3`))

funds <- funds %>%
  group_by(state) %>%
  mutate(rev.share = na_locf(rev.share)) %>%
  mutate(rev.interp = rev.share*`3`)

funds$rev <- funds$`1`
funds$rev[is.na(funds$rev)] <- funds$rev.interp[is.na(funds$rev)]

funds <- funds[!is.na(funds$rev),] # rm obs w.o. funds data

# Interpolate missing expenditure (3) based on actual or interpolated revenue

funds <- funds %>%
  group_by(state,year) %>%
  mutate(exp.share = sum(`3`)/sum(rev))

funds <- funds %>%
  group_by(state) %>%
  mutate(exp.share = na_locf(exp.share)) %>%
  mutate(exp.interp = exp.share*rev)

funds$exp <- funds$`3`
funds$exp[is.na(funds$exp)] <- funds$exp.interp[is.na(funds$exp)]

# Interpolate missing education based on actual or interpolated expenditure
funds <- funds %>%
  group_by(state,year) %>%
  mutate(educ.share = sum(`31`)/sum(exp))

funds <- funds %>%
  group_by(state) %>%
  mutate(educ.share = na_locf(educ.share)) %>%
  mutate(educ.interp = educ.share*exp)

funds$educ <- funds$`31`
funds$educ[is.na(funds$educ)] <- funds$educ.interp[is.na(funds$educ)]

print(paste("share interpolated: ", (sum(is.na(funds$`31`))-sum(is.na(funds$educ))) / nrow(funds)))  # share interpolated: 9.29%

# Get historical CPI

USCPI_1783_2008 <- read_csv(paste0(data.directory,"USCPI_1783-2008.csv"))

USCPI_1783_2008$adj_factor_08 <- USCPI_1783_2008$`U.S. Consumer Price Index`/USCPI_1783_2008$`U.S. Consumer Price Index`[USCPI_1783_2008$Year == 1942] # adj. factor relative to 1942

funds <- merge(funds, USCPI_1783_2008, by.x="year", by.y="Year")
funds.us <- merge(funds.us, USCPI_1783_2008, by.x="year", by.y="Year")

funds["rev.pc"] <- NA
funds["rev.pc"] <- (funds$rev/funds$adj_factor_08)/funds$ns.pop

funds["exp.pc"] <- NA
funds["exp.pc"] <- (funds$exp/funds$adj_factor_08)/funds$ns.pop

funds["educ.pc"] <- NA
funds["educ.pc"] <- (funds$educ/funds$adj_factor_08)/funds$ns.pop

funds.us["rev.pc"] <- NA
funds.us["rev.pc"] <- (funds.us$`1`/funds.us$adj_factor_08)/funds.us$ns.pop

funds.us["exp.pc"] <- NA
funds.us["exp.pc"] <- (funds.us$`3`/funds.us$adj_factor_08)/funds.us$ns.pop

funds.us["educ.pc"] <- NA
funds.us["educ.pc"] <- (funds.us$`31`/funds.us$adj_factor_08)/funds.us$ns.pop

# Get farmsize

funds$year2 <- signif(funds$year,3) # merge by nearest decennial
funds$year2[funds$year<=1785] <- 1790 # VA
funds$year2[funds$year2<1880 & funds$state=="AK"] <- 1880
funds$year2[funds$year2==1980] <- 1983

if(homesteads==FALSE){
  funds <- merge(funds, census.ts.state[c('year','state',"farmsize")],
                 by.x=c('year2','state'), by.y=c('year','state'),all.x=TRUE)
  
  # clean feature set
  funds <- funds[colnames(funds) %in% c("state","year","year2","farmsize",
                                        "rev.pc","exp.pc","educ.pc")]
  
  funds.us <- funds.us[colnames(funds.us) %in% c("state","year","year2",
                                                 "rev.pc","exp.pc","educ.pc")]
}

# setup for MC

funds$treat <- NA
funds$treat[funds$state %in% pub.states] <- "Treated"
funds$treat[funds$state %in% state.land.states] <- "Control"

funds <- funds[with(funds, order(treat, year)), ] # order by treatment status + year

#Outcomes
rev.pc <- reshape(data.frame(funds[c("state","year","rev.pc")]), idvar = "year", timevar = "state", direction = "wide")
colnames(rev.pc) <- sub("rev.pc.","", colnames(rev.pc))
rownames(rev.pc) <- rev.pc$year
rev.pc <- rev.pc[!colnames(rev.pc)%in% "year"]

rev.pc[apply(rev.pc,2,is.nan)] <- NA # replace NaN with NA

exp.pc <- reshape(data.frame(funds[c("state","year","exp.pc")]), idvar = "year", timevar = "state", direction = "wide")
colnames(exp.pc) <- sub("exp.pc.","", colnames(exp.pc))
rownames(exp.pc) <- exp.pc$year
exp.pc <- exp.pc[!colnames(exp.pc)%in% "year"]

exp.pc[apply(exp.pc,2,is.nan)] <- NA # replace NaN with NA

educ.pc <- reshape(data.frame(funds[c("state","year","educ.pc")]), idvar = "year", timevar = "state", direction = "wide")
colnames(educ.pc) <- sub("educ.pc.","", colnames(educ.pc))
rownames(educ.pc) <- educ.pc$year
educ.pc <- educ.pc[!colnames(educ.pc)%in% "year"]

educ.pc[apply(educ.pc,2,is.nan)] <- NA # replace NaN with NA
educ.pc <- educ.pc[match(sort(rownames(educ.pc)), row.names(educ.pc)),] # sort rows
educ.pc <- educ.pc[,match(sort(colnames(educ.pc)), colnames(educ.pc)),] # sort columns

# Covariates:
# farm values
# farm size
# RR access 
# ratio of slaves to the total population in 1860
# share of whites, free African Americans, Asian, or Native Americans to non-slave pop. in 1860
# water transport or rail access in 1860

faval <- reshape(data.frame(farmval.state[c("state.abb","year","faval")]), idvar = "year", timevar = "state.abb", direction = "wide")
colnames(faval) <- sub("faval.","", colnames(faval))
rownames(faval) <- faval$year
faval <- faval[!colnames(faval)%in% "year"]

farmsize <- reshape(data.frame(funds[c("state","year2","farmsize")]), idvar = "year2", timevar = "state", direction = "wide")
colnames(farmsize) <- sub("farmsize.","", colnames(farmsize))
rownames(farmsize) <- farmsize$year2
farmsize <- farmsize[!colnames(farmsize)%in% "year2"]

slave.share <- reshape(data.frame(funds[c("state","year2","slave.share")]), idvar = "year2", timevar = "state", direction = "wide")
colnames(slave.share) <- sub("slave.share.","", colnames(slave.share))
rownames(slave.share) <- slave.share$year2
slave.share <- slave.share[!colnames(slave.share)%in% "year2"]

aa.share <- reshape(data.frame(funds[c("state","year2","aa.share")]), idvar = "year2", timevar = "state", direction = "wide")
colnames(aa.share) <- sub("aa.share.","", colnames(aa.share))
rownames(aa.share) <- aa.share$year2
aa.share <- aa.share[!colnames(aa.share)%in% "year2"]

native.share <- reshape(data.frame(funds[c("state","year2","native.share")]), idvar = "year2", timevar = "state", direction = "wide")
colnames(native.share) <- sub("native.share.","", colnames(native.share))
rownames(native.share) <- native.share$year2
native.share <- native.share[!colnames(native.share)%in% "year2"]

white.share <- reshape(data.frame(funds[c("state","year2","white.share")]), idvar = "year2", timevar = "state", direction = "wide")
colnames(white.share) <- sub("white.share.","", colnames(white.share))
rownames(white.share) <- white.share$year2
white.share <- white.share[!colnames(white.share)%in% "year2"]

CapacityMatrices <- function(d, imp=c("none","knn","locf","linear","ma","mean","mice","random","rf","amelia","mtsdi"),faval,farmsize,access,slave.share,aa.share,native.share,white.share,homesteads=TRUE){
  
  if(homesteads==TRUE){

    nzv.states.rm <- c("AK","HI") # rms AK + HI
    
    d <- d[,!colnames(d)%in%nzv.states.rm]
    
  }else{
    parity.diff <- dim(d[,colnames(d)%in%pub.states])[2]-dim(d[,colnames(d)%in%state.land.states])[2] # 10 states
    
    nzv.states <- nearZeroVar(d, saveMetrics = TRUE)
    
    nzv.states.rm <- rownames(nzv.states[order(nzv.states$percentUnique),])[1:(parity.diff+2)] # rms 12 PLS + HI
    
    d <- d[,!colnames(d)%in%nzv.states.rm]
    
    stopifnot(dim(d[,colnames(d)%in%pub.states])[2] == dim(d[,colnames(d)%in%state.land.states])[2]) # ensure in parity in # features
  }
  
  # Remove zero variance years
  nzv.years <- nearZeroVar(t(d)) # 15 years
  d <- t(t(d)[, -nzv.years])
  
  ## Need covars to have same dimensions
  farmsize <- farmsize[,colnames(farmsize)%in%colnames(d)][rownames(farmsize)%in%rownames(d),]
  faval <- faval[,colnames(faval)%in%colnames(d)][rownames(faval)%in%rownames(d),]
  access <- access[,colnames(access)%in%colnames(d)][rownames(access)%in%rownames(d),]
  access <- access[,colnames(access)%in%colnames(d)][rownames(access)%in%rownames(d),]
  slave.share <- slave.share[,colnames(slave.share)%in%colnames(d)][rownames(slave.share)%in%rownames(d),]
  aa.share <- aa.share[,colnames(aa.share)%in%colnames(d)][rownames(aa.share)%in%rownames(d),]
  native.share <- native.share[,colnames(native.share)%in%colnames(d)][rownames(native.share)%in%rownames(d),]
  white.share <- white.share[,colnames(white.share)%in%colnames(d)][rownames(white.share)%in%rownames(d),]
  
  if(imp=="locf"){
    print(paste("farmsize missing: ",sum(is.na(farmsize[rownames(farmsize)=="1860",]))/(dim(farmsize[rownames(farmsize)=="1860",])[1]*dim(farmsize[rownames(farmsize)=="1860",])[2]))) # % missing in 1860: 26.31%
    print(paste("faval missing: ",sum(is.na(faval[rownames(faval)%in%c("1850","1860"),]))/(dim(faval[rownames(faval)%in%c("1850","1860"),])[1]*dim(faval[rownames(faval)%in%c("1850","1860"),])[2]))) # % missing in 1850+1860: 15.78%
    print(paste("slave.share missing: ",sum(is.na(slave.share[rownames(slave.share)=="1860",]))/(dim(slave.share[rownames(slave.share)=="1860",])[1]*dim(slave.share[rownames(slave.share)=="1860",])[2]))) # % missing in 1860: 39.58%
    print(paste("aa.share missing: ",sum(is.na(aa.share[rownames(aa.share)=="1860",]))/(dim(aa.share[rownames(aa.share)=="1860",])[1]*dim(aa.share[rownames(aa.share)=="1860",])[2]))) # % missing in 1860: 39.58%
    print(paste("native.share missing: ",sum(is.na(native.share[rownames(native.share)=="1860",]))/(dim(native.share[rownames(native.share)=="1860",])[1]*dim(native.share[rownames(native.share)=="1860",])[2]))) # % missing in 1860: 41.67%
    print(paste("white.share missing: ",sum(is.na(white.share[rownames(white.share)=="1860",]))/(dim(white.share[rownames(white.share)=="1860",])[1]*dim(white.share[rownames(white.share)=="1860",])[2]))) # % missing in 1860: 39.58%
  }
  
  # Masked matrix for which 1=observed, NA=missing/imputed
  d.M.missing <- t(as.matrix(d))
  d.M.missing[is.nan(d.M.missing)] <- NA
  d.M.missing[!is.na(d.M.missing)] <-1
  
  if(imp=="locf"){
    print(paste("missing total: ",sum(is.na(d))/(dim(d)[1]*dim(d)[2]))) # 39.98% # (NxT) = (48 x 203) (# else: 34.15% # (NxT) = (38 x 203)
  }
  
  # impute missing
  d.imp <- d
  
  faval.imp <- faval
  farmsize.imp <- farmsize
  slave.share.imp <- slave.share
  aa.share.imp <- aa.share
  native.share.imp <- native.share
  white.share.imp <- white.share

  if(imp=="none"){
    d.imp[is.na(d.imp)] <- 0 # NAs are 0
    
    faval.imp[is.na(faval.imp)] <- 0
    farmsize.imp[is.na(farmsize.imp)] <- 0
    slave.share.imp[is.na(slave.share.imp)] <- 0
    aa.share.imp[is.na(aa.share.imp)] <- 0
    native.share.imp[is.na(native.share.imp)] <- 0
    white.share.imp[is.na(white.share.imp)] <- 0
  }
  if(imp=="knn"){
    d.imp[,!colnames(d.imp)%in%pub.states] <- as.matrix(kNN(d.imp[,!colnames(d.imp)%in%pub.states])[,1:ncol(d.imp[,!colnames(d.imp)%in%pub.states])])
    d.imp[,colnames(d.imp)%in%pub.states]<- as.matrix(kNN(d.imp[,colnames(d.imp)%in%pub.states])[,1:ncol(d.imp[,colnames(d.imp)%in%pub.states])])
    
    faval.imp[,!colnames(faval.imp)%in%pub.states] <- as.matrix(kNN(faval.imp[,!colnames(faval.imp)%in%pub.states])[,1:ncol(faval.imp[,!colnames(faval.imp)%in%pub.states])])
    faval.imp[,colnames(faval.imp)%in%pub.states]<- as.matrix(kNN(faval.imp[,colnames(faval.imp)%in%pub.states])[,1:ncol(faval.imp[,colnames(faval.imp)%in%pub.states])])
    
    farmsize.imp[,!colnames(farmsize.imp)%in%pub.states] <- as.matrix(kNN(farmsize.imp[,!colnames(farmsize.imp)%in%pub.states])[,1:ncol(farmsize.imp[,!colnames(farmsize.imp)%in%pub.states])])
    farmsize.imp[,colnames(farmsize.imp)%in%pub.states]<- as.matrix(kNN(farmsize.imp[,colnames(farmsize.imp)%in%pub.states])[,1:ncol(farmsize.imp[,colnames(farmsize.imp)%in%pub.states])])
  }
  if(imp=="locf"){
    d.imp[,!colnames(d.imp)%in%pub.states] <- na_locf(d.imp[,!colnames(d.imp)%in%pub.states], option = "locf", na_remaining = "rev") 
    d.imp[,colnames(d.imp)%in%pub.states] <- na_locf(d.imp[,colnames(d.imp)%in%pub.states], option = "locf", na_remaining = "rev")  
    
    faval.imp[,!colnames(faval.imp)%in%pub.states] <- na_locf(faval.imp[,!colnames(faval.imp)%in%pub.states], option = "locf", na_remaining = "rev") 
    faval.imp[,colnames(faval.imp)%in%pub.states] <- na_locf(faval.imp[,colnames(faval.imp)%in%pub.states], option = "locf", na_remaining = "rev")
    
    farmsize.imp[,!colnames(farmsize.imp)%in%pub.states] <- na_locf(farmsize.imp[,!colnames(farmsize.imp)%in%pub.states], option = "locf", na_remaining = "rev") 
    farmsize.imp[,colnames(farmsize.imp)%in%pub.states] <- na_locf(farmsize.imp[,colnames(farmsize.imp)%in%pub.states], option = "locf", na_remaining = "rev")
  }
  if(imp=="linear"){
    d.imp[,!colnames(d.imp)%in%pub.states]  <- na_interpolation(d.imp[,!colnames(d.imp)%in%pub.states], option = "linear") # equiv to na.interp in forecast
    d.imp[,colnames(d.imp)%in%pub.states] <- na_interpolation(d.imp[,colnames(d.imp)%in%pub.states], option = "linear")  
    
    faval.imp[,!colnames(faval.imp)%in%pub.states] <- na_interpolation(faval.imp[,!colnames(faval.imp)%in%pub.states], option = "linear") 
    faval.imp[,colnames(faval.imp)%in%pub.states] <- na_interpolation(faval.imp[,colnames(faval.imp)%in%pub.states], option = "linear")
    
    farmsize.imp[,!colnames(farmsize.imp)%in%pub.states] <- na_interpolation(farmsize.imp[,!colnames(farmsize.imp)%in%pub.states], option = "linear") 
    farmsize.imp[,colnames(farmsize.imp)%in%pub.states] <- na_interpolation(farmsize.imp[,colnames(farmsize.imp)%in%pub.states], option = "linear")
  }
  if(imp=="ma"){
    d.imp[!colnames(d.imp)%in%pub.states] <- na_ma(d.imp[!colnames(d.imp)%in%pub.states])
    d.imp[colnames(d.imp)%in%pub.states]<- na_ma(d.imp[colnames(d.imp)%in%pub.states])  
    
    faval.imp[,!colnames(faval.imp)%in%pub.states] <- na_ma(faval.imp[,!colnames(faval.imp)%in%pub.states]) 
    faval.imp[,colnames(faval.imp)%in%pub.states] <- na_ma(faval.imp[,colnames(faval.imp)%in%pub.states])
    
    farmsize.imp[,!colnames(farmsize.imp)%in%pub.states] <- na_ma(farmsize.imp[,!colnames(farmsize.imp)%in%pub.states]) 
    farmsize.imp[,colnames(farmsize.imp)%in%pub.states] <- na_ma(farmsize.imp[,colnames(farmsize.imp)%in%pub.states])
  }
  if(imp=="mean"){
    d.imp[!colnames(d.imp)%in%pub.states] <- na_mean(d.imp[!colnames(d.imp)%in%pub.states])
    d.imp[colnames(d.imp)%in%pub.states]<- na_mean(d.imp[colnames(d.imp)%in%pub.states])  
    
    faval.imp[,!colnames(faval.imp)%in%pub.states] <- na_mean(faval.imp[,!colnames(faval.imp)%in%pub.states]) 
    faval.imp[,colnames(faval.imp)%in%pub.states] <- na_mean(faval.imp[,colnames(faval.imp)%in%pub.states])
    
    farmsize.imp[,!colnames(farmsize.imp)%in%pub.states] <- na_mean(farmsize.imp[,!colnames(farmsize.imp)%in%pub.states]) 
    farmsize.imp[,colnames(farmsize.imp)%in%pub.states] <- na_mean(farmsize.imp[,colnames(farmsize.imp)%in%pub.states])
  }
  if(imp=="mice"){
    # outcomes
    train.imp <- mice(d.imp[,!colnames(d.imp)%in%pub.states])
    test.imp <- mice(d.imp[,colnames(d.imp)%in%pub.states])  
    
    d.imp[,!colnames(d.imp)%in%pub.states]  <- as.matrix(mice::complete(train.imp))
    d.imp[,colnames(d.imp)%in%pub.states] <-  as.matrix(mice::complete(test.imp))
    
    if(sum(is.na(d.imp[,!colnames(d.imp)%in%pub.states]))>0){ # rerun if remaining missing
      train.imp <- mice(d.imp[,!colnames(d.imp)%in%pub.states])
      
      d.imp[,!colnames(d.imp)%in%pub.states]  <- as.matrix(mice::complete(train.imp))
    }
    if(sum(is.na(d.imp[,colnames(d.imp)%in%pub.states]))>0){ # rerun if remaining missing
      test.imp <- mice(d.imp[,colnames(d.imp)%in%pub.states])  
      
      d.imp[,colnames(d.imp)%in%pub.states] <-  as.matrix(mice::complete(test.imp))
    }
    # faval
    
    train.faval.imp <- mice(faval.imp[,!colnames(faval.imp)%in%pub.states])
    test.faval.imp <- mice(faval.imp[,colnames(faval.imp)%in%pub.states])  
    
    faval.imp[,!colnames(faval.imp)%in%pub.states]  <- as.matrix(mice::complete(train.faval.imp))
    faval.imp[,colnames(faval.imp)%in%pub.states] <-  as.matrix(mice::complete(test.faval.imp))
    
    if(sum(is.na(faval.imp[,!colnames(faval.imp)%in%pub.states]))>0){ # rerun if remaining missing
      train.faval.imp <- mice(faval.imp[,!colnames(faval.imp)%in%pub.states])
      
      faval.imp[,!colnames(faval.imp)%in%pub.states]  <- as.matrix(mice::complete(train.faval.imp))
    }
    if(sum(is.na(faval.imp[,colnames(faval.imp)%in%pub.states]))>0){ # rerun if remaining missing
      test.faval.imp <- mice(faval.imp[,colnames(faval.imp)%in%pub.states])  
      
      faval.imp[,colnames(faval.imp)%in%pub.states] <-  as.matrix(mice::complete(test.faval.imp))
    }
    
    # farmsize
    
    train.farmsize.imp <- mice(farmsize.imp[,!colnames(farmsize.imp)%in%pub.states])
    test.farmsize.imp <- mice(farmsize.imp[,colnames(farmsize.imp)%in%pub.states])  
    
    farmsize.imp[,!colnames(farmsize.imp)%in%pub.states]  <- as.matrix(mice::complete(train.farmsize.imp))
    farmsize.imp[,colnames(farmsize.imp)%in%pub.states] <-  as.matrix(mice::complete(test.farmsize.imp))
    
    if(sum(is.na(farmsize.imp[,!colnames(farmsize.imp)%in%pub.states]))>0){ # rerun if remaining missing
      train.farmsize.imp <- mice(farmsize.imp[,!colnames(farmsize.imp)%in%pub.states])
      
      farmsize.imp[,!colnames(farmsize.imp)%in%pub.states]  <- as.matrix(mice::complete(train.farmsize.imp))
    }
    if(sum(is.na(farmsize.imp[,colnames(farmsize.imp)%in%pub.states]))>0){ # rerun if remaining missing
      test.farmsize.imp <- mice(farmsize.imp[,colnames(farmsize.imp)%in%pub.states])  
      
      farmsize.imp[,colnames(farmsize.imp)%in%pub.states] <-  as.matrix(mice::complete(test.farmsize.imp))
    }
  }
  if(imp=="random"){
    d.imp[!colnames(d.imp)%in%pub.states] <- na_random(d.imp[!colnames(d.imp)%in%pub.states])
    d.imp[colnames(d.imp)%in%pub.states]<- na_random(d.imp[colnames(d.imp)%in%pub.states])  
    
    faval.imp[,!colnames(faval.imp)%in%pub.states] <- na_random(faval.imp[,!colnames(faval.imp)%in%pub.states]) 
    faval.imp[,colnames(faval.imp)%in%pub.states] <- na_random(faval.imp[,colnames(faval.imp)%in%pub.states])
    
    farmsize.imp[,!colnames(farmsize.imp)%in%pub.states] <- na_random(farmsize.imp[,!colnames(farmsize.imp)%in%pub.states]) 
    farmsize.imp[,colnames(farmsize.imp)%in%pub.states] <- na_random(farmsize.imp[,colnames(farmsize.imp)%in%pub.states])
  }
  if(imp=="rf"){
    d.imp[,!colnames(d.imp)%in%pub.states] <- missForest(d.imp[,!colnames(d.imp)%in%pub.states])$ximp
    d.imp[,colnames(d.imp)%in%pub.states]<- missForest(d.imp[,colnames(d.imp)%in%pub.states])$ximp
    
    faval.imp[,!colnames(faval.imp)%in%pub.states] <- missForest(faval.imp[,!colnames(faval.imp)%in%pub.states])$ximp
    faval.imp[,colnames(faval.imp)%in%pub.states]<- missForest(faval.imp[,colnames(faval.imp)%in%pub.states])$ximp
    
    farmsize.imp[,!colnames(farmsize.imp)%in%pub.states] <- missForest(farmsize.imp[,!colnames(farmsize.imp)%in%pub.states])$ximp
    farmsize.imp[,colnames(farmsize.imp)%in%pub.states]<- missForest(farmsize.imp[,colnames(farmsize.imp)%in%pub.states])$ximp
  }
  
  if(imp=="amelia"){
    # outcomes
    train.imp <- amelia(melt(d.imp[,!colnames(d.imp)%in%pub.states]), ts="Var1",cs="Var2", polytime = 2, bounds=matrix(c(3,0,max(d[,!colnames(d.imp)%in%pub.states], na.rm = TRUE)), 1,3))
    test.imp <- amelia(melt(d.imp[,colnames(d.imp)%in%pub.states]), ts="Var1",cs="Var2", polytime = 2, bounds=matrix(c(3,0,max(d[,colnames(d.imp)%in%pub.states], na.rm = TRUE)), 1,3))
    
    d.imp[,!colnames(d.imp)%in%pub.states]  <- as.matrix(dcast(train.imp$imputations$imp1, Var1~Var2))[,-1]
    d.imp[,colnames(d.imp)%in%pub.states] <-  as.matrix(dcast(test.imp$imputations$imp1, Var1~Var2))[,-1]
    
    # covariates: impute with mice
    # faval
    
    train.faval.imp <- mice(faval.imp[,!colnames(faval.imp)%in%pub.states])
    test.faval.imp <- mice(faval.imp[,colnames(faval.imp)%in%pub.states])  
    
    faval.imp[,!colnames(faval.imp)%in%pub.states]  <- as.matrix(mice::complete(train.faval.imp))
    faval.imp[,colnames(faval.imp)%in%pub.states] <-  as.matrix(mice::complete(test.faval.imp))
    
    if(sum(is.na(faval.imp[,!colnames(faval.imp)%in%pub.states]))>0){ # rerun if remaining missing
      train.faval.imp <- mice(faval.imp[,!colnames(faval.imp)%in%pub.states])
      
      faval.imp[,!colnames(faval.imp)%in%pub.states]  <- as.matrix(mice::complete(train.faval.imp))
    }
    if(sum(is.na(faval.imp[,colnames(faval.imp)%in%pub.states]))>0){ # rerun if remaining missing
      test.faval.imp <- mice(faval.imp[,colnames(faval.imp)%in%pub.states])  
      
      faval.imp[,colnames(faval.imp)%in%pub.states] <-  as.matrix(mice::complete(test.faval.imp))
    }
    
    # farmsize
    
    train.farmsize.imp <- mice(farmsize.imp[,!colnames(farmsize.imp)%in%pub.states])
    test.farmsize.imp <- mice(farmsize.imp[,colnames(farmsize.imp)%in%pub.states])  
    
    farmsize.imp[,!colnames(farmsize.imp)%in%pub.states]  <- as.matrix(mice::complete(train.farmsize.imp))
    farmsize.imp[,colnames(farmsize.imp)%in%pub.states] <-  as.matrix(mice::complete(test.farmsize.imp))
    
    if(sum(is.na(farmsize.imp[,!colnames(farmsize.imp)%in%pub.states]))>0){ # rerun if remaining missing
      train.farmsize.imp <- mice(farmsize.imp[,!colnames(farmsize.imp)%in%pub.states])
      
      farmsize.imp[,!colnames(farmsize.imp)%in%pub.states]  <- as.matrix(mice::complete(train.farmsize.imp))
    }
    if(sum(is.na(farmsize.imp[,colnames(farmsize.imp)%in%pub.states]))>0){ # rerun if remaining missing
      test.farmsize.imp <- mice(farmsize.imp[,colnames(farmsize.imp)%in%pub.states])  
      
      farmsize.imp[,colnames(farmsize.imp)%in%pub.states] <-  as.matrix(mice::complete(test.farmsize.imp))
    }
  }
  
  if(imp=="mtsdi"){ # Continue here
    # outcomes
    train.imp <- mice(d.imp[,!colnames(d.imp)%in%pub.states])
    test.imp <- mice(d.imp[,colnames(d.imp)%in%pub.states])  
    
    d.imp[,!colnames(d.imp)%in%pub.states]  <- as.matrix(mice::complete(train.imp))
    d.imp[,colnames(d.imp)%in%pub.states] <-  as.matrix(mice::complete(test.imp))
    
    if(sum(is.na(d.imp[,!colnames(d.imp)%in%pub.states]))>0){ # rerun if remaining missing
      train.imp <- mice(d.imp[,!colnames(d.imp)%in%pub.states])
      
      d.imp[,!colnames(d.imp)%in%pub.states]  <- as.matrix(mice::complete(train.imp))
    }
    if(sum(is.na(d.imp[,colnames(d.imp)%in%pub.states]))>0){ # rerun if remaining missing
      test.imp <- mice(d.imp[,colnames(d.imp)%in%pub.states])  
      
      d.imp[,colnames(d.imp)%in%pub.states] <-  as.matrix(mice::complete(test.imp))
    }
    # faval
    
    train.faval.imp <- mice(faval.imp[,!colnames(faval.imp)%in%pub.states])
    test.faval.imp <- mice(faval.imp[,colnames(faval.imp)%in%pub.states])  
    
    faval.imp[,!colnames(faval.imp)%in%pub.states]  <- as.matrix(mice::complete(train.faval.imp))
    faval.imp[,colnames(faval.imp)%in%pub.states] <-  as.matrix(mice::complete(test.faval.imp))
    
    if(sum(is.na(faval.imp[,!colnames(faval.imp)%in%pub.states]))>0){ # rerun if remaining missing
      train.faval.imp <- mice(faval.imp[,!colnames(faval.imp)%in%pub.states])
      
      faval.imp[,!colnames(faval.imp)%in%pub.states]  <- as.matrix(mice::complete(train.faval.imp))
    }
    if(sum(is.na(faval.imp[,colnames(faval.imp)%in%pub.states]))>0){ # rerun if remaining missing
      test.faval.imp <- mice(faval.imp[,colnames(faval.imp)%in%pub.states])  
      
      faval.imp[,colnames(faval.imp)%in%pub.states] <-  as.matrix(mice::complete(test.faval.imp))
    }
    
    # farmsize
    
    train.farmsize.imp <- mice(farmsize.imp[,!colnames(farmsize.imp)%in%pub.states])
    test.farmsize.imp <- mice(farmsize.imp[,colnames(farmsize.imp)%in%pub.states])  
    
    farmsize.imp[,!colnames(farmsize.imp)%in%pub.states]  <- as.matrix(mice::complete(train.farmsize.imp))
    farmsize.imp[,colnames(farmsize.imp)%in%pub.states] <-  as.matrix(mice::complete(test.farmsize.imp))
    
    if(sum(is.na(farmsize.imp[,!colnames(farmsize.imp)%in%pub.states]))>0){ # rerun if remaining missing
      train.farmsize.imp <- mice(farmsize.imp[,!colnames(farmsize.imp)%in%pub.states])
      
      farmsize.imp[,!colnames(farmsize.imp)%in%pub.states]  <- as.matrix(mice::complete(train.farmsize.imp))
    }
    if(sum(is.na(farmsize.imp[,colnames(farmsize.imp)%in%pub.states]))>0){ # rerun if remaining missing
      test.farmsize.imp <- mice(farmsize.imp[,colnames(farmsize.imp)%in%pub.states])  
      
      farmsize.imp[,colnames(farmsize.imp)%in%pub.states] <-  as.matrix(mice::complete(test.farmsize.imp))
    }
  }
  
  # Take logs

  if(imp=="none"){
    d.imp <- log1p(d.imp)
  }else{
    stopifnot(min(d.imp)>0) # ensure imputed data is pos. and nonzero and no NAs
    stopifnot(sum(is.na(d.imp))==0)
    d.imp <- log(d.imp)
  }
  
  # Matrix of observed entries (N x T)
  d.M <- t(as.matrix(d.imp))
  d.M[is.nan(d.M )] <- NA
  
  rownames(d.M) <- colnames(d)
  colnames(d.M) <- rownames(d)
  
  # Ensure covariate and outcome dims match
  capacity.states <- sort(unique(c(colnames(rev.pc),colnames(exp.pc),colnames(educ.pc))))
  
  # Masked matrix which is 0 for control units and treated units before treatment and 1 for treated units after treatment.
  
  d.mask <- matrix(0, nrow = nrow(d.M), 
                   ncol= ncol(d.M),
                   dimnames = list(rownames(d.M), colnames(d.M)))
  
  d.mask[,colnames(d.mask)>=1869][rownames(d.mask)%in%c("CA","CO","IA","KS","MI","MN","MO","NE","OH","OR","SD","WA","WI"),] <- 1 # earliest WPL 
  d.mask[,colnames(d.mask)>=1870][rownames(d.mask)%in%c("IL","NV"),] <- 1 
  d.mask[,colnames(d.mask)>=1871][rownames(d.mask)%in%c("ID"),] <- 1 
  d.mask[,colnames(d.mask)>=1872][rownames(d.mask)%in%c("MT","ND","UT","AL","MS"),] <- 1 # earliest SPL 
  d.mask[,colnames(d.mask)>=1873][rownames(d.mask)%in%c("AR","FL","LA"),] <- 1 
  d.mask[,colnames(d.mask)>=1875][rownames(d.mask)%in%c("IN","NM","WY"),] <- 1 
  d.mask[,colnames(d.mask)>=1878][rownames(d.mask)%in%c("AZ"),] <- 1 
  d.mask[,colnames(d.mask)>=1890][rownames(d.mask)%in%c("OK"),] <- 1 
  d.mask[,colnames(d.mask)>=1902][rownames(d.mask)%in%c("AK"),] <- 1 
  
  return(list("M"=d.M, "M.missing"=d.M.missing, "mask"=d.mask,
              "farmsize"=t(farmsize.imp[colnames(farmsize.imp) %in% capacity.states]),
              "faval"=t(faval.imp[colnames(faval.imp) %in% capacity.states]),
              "access"=t(access[colnames(access) %in% capacity.states]),
              "slave.share"=t(slave.share[colnames(slave.share) %in% capacity.states]),
              "aa.share"=t(aa.share[colnames(aa.share) %in% capacity.states]),
              "native.share"=t(native.share[colnames(native.share) %in% capacity.states]),
              "white.share"=t(white.share[colnames(white.share) %in% capacity.states])))
}

if(homesteads){
  capacity.outcomes <- list("rev.pc"=rev.pc,"exp.pc"=exp.pc)
  capacity.outcomes.none <- lapply(capacity.outcomes, CapacityMatrices, imp="none",faval,farmsize,access,slave.share,aa.share,native.share,white.share)
  capacity.outcomes.locf <- lapply(capacity.outcomes, CapacityMatrices, imp="locf",faval,farmsize,access,slave.share,aa.share,native.share,white.share)
  capacity.outcomes.linear <- lapply(capacity.outcomes, CapacityMatrices, imp="linear",faval,farmsize,access,slave.share,aa.share,native.share,white.share)
  capacity.outcomes.ma <- lapply(capacity.outcomes, CapacityMatrices, imp="ma",faval,farmsize,access,slave.share,aa.share,native.share,white.share)
  capacity.outcomes.mean <- lapply(capacity.outcomes, CapacityMatrices, imp="mean",faval,farmsize,access,slave.share,aa.share,native.share,white.share)
  capacity.outcomes.mice <- lapply(capacity.outcomes, CapacityMatrices, imp="mice",faval,farmsize,access,slave.share,aa.share,native.share,white.share)
  capacity.outcomes.random <- lapply(capacity.outcomes, CapacityMatrices, imp="random",faval,farmsize,access,slave.share,aa.share,native.share,white.share)
  capacity.outcomes.rf <- lapply(capacity.outcomes, CapacityMatrices, imp="rf",faval,farmsize,access,slave.share,aa.share,native.share,white.share)
}else{
  capacity.outcomes <- list("educ.pc"=educ.pc)
  
  capacity.outcomes.none <- lapply(capacity.outcomes, CapacityMatrices, imp="none",faval,farmsize,access, homesteads=FALSE)
  capacity.outcomes.locf <- lapply(capacity.outcomes, CapacityMatrices, imp="locf",faval,farmsize,access, homesteads=FALSE)
  capacity.outcomes.linear <- lapply(capacity.outcomes, CapacityMatrices, imp="linear",faval,farmsize,access, homesteads=FALSE)
  capacity.outcomes.ma <- lapply(capacity.outcomes, CapacityMatrices, imp="ma",faval,farmsize,access, homesteads=FALSE)
  capacity.outcomes.mean <- lapply(capacity.outcomes, CapacityMatrices, imp="mean",faval,farmsize,access, homesteads=FALSE)
  capacity.outcomes.mice <- lapply(capacity.outcomes, CapacityMatrices, imp="mice",faval,farmsize,access, homesteads=FALSE)
  capacity.outcomes.random <- lapply(capacity.outcomes, CapacityMatrices, imp="random",faval,farmsize,access, homesteads=FALSE)
  capacity.outcomes.rf <- lapply(capacity.outcomes, CapacityMatrices, imp="rf",faval,farmsize,access, homesteads=FALSE)
  capacity.outcomes.knn <- lapply(capacity.outcomes, CapacityMatrices, imp="knn",faval,farmsize,access, homesteads=FALSE)
  
  saveRDS(capacity.outcomes.knn, paste0(data.directory,"capacity-outcomes-knn.rds"))
}

# Save

saveRDS(capacity.outcomes.none, paste0(data.directory,"capacity-outcomes-none.rds"))
saveRDS(capacity.outcomes.locf, paste0(data.directory,"capacity-outcomes-locf.rds"))
saveRDS(capacity.outcomes.linear, paste0(data.directory,"capacity-outcomes-linear.rds"))
saveRDS(capacity.outcomes.ma, paste0(data.directory,"capacity-outcomes-ma.rds"))
saveRDS(capacity.outcomes.mean, paste0(data.directory,"capacity-outcomes-mean.rds"))
saveRDS(capacity.outcomes.mice, paste0(data.directory,"capacity-outcomes-mice.rds"))
saveRDS(capacity.outcomes.random, paste0(data.directory,"capacity-outcomes-random.rds"))
saveRDS(capacity.outcomes.rf, paste0(data.directory,"capacity-outcomes-rf.rds"))