############################################################################
# Prepare capacity data (state-level or aggregated at county level)        #
############################################################################

require(data.table)
require(reshape2)
require(stringr)
require(dplyr)
require(weights)
require(tidyr)
require(readr)
require(imputeTS)
require(caret)

## STATE-LEVEL DATA

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

# Clean state codes
funds.9728$state[funds.9728$state=="MIS"] <- "MS"
funds.9728$state[funds.9728$state=="IO"] <- "IA" 
funds.9728$state[funds.9728$state=="KA"] <- "KS" 

# Clean MS revenue in 1843

funds.9728$`1`[funds.9728$state=="MS" & funds.9728$year==1843] <- funds.9728$`1`[funds.9728$state=="MS" & funds.9728$year==1843]/10

# ME data is in thousands

funds.9728$`1`[funds.9728$state=="ME"] <- funds.9728$`1`[funds.9728$state=="ME"]*1000
funds.9728$`3`[funds.9728$state=="ME"] <- funds.9728$`3`[funds.9728$state=="ME"]*1000
funds.9728$`31`[funds.9728$state=="ME"] <- funds.9728$`31`[funds.9728$state=="ME"]*1000
 
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

funds.6304$value[funds.6304$year>=1932] <- funds.6304$value[funds.6304$year>=1932]*1000 # 1932 to 1982 census are reported in thousands of dollars

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

# Make 0 or negative values missing

funds$`1`[funds$`1`<=0] <- NA
funds$`3`[funds$`3`<=0] <- NA
funds$`31`[funds$`31`<=0] <- NA

# Make outliers NA
funds[(funds$state=="WA" & funds$year>=1907 & funds$year<=1918),] <- NA

# Get historical CPI

USCPI_1783_1982 <- read_csv(paste0(data.directory,"USCPI_1783-1982.csv"))

USCPI_1783_1982$adj_factor_82 <- USCPI_1783_1982$`U.S. Consumer Price Index`/USCPI_1783_1982$`U.S. Consumer Price Index`[USCPI_1783_1982$Year == 1982] # adj. factor relative to 1982
USCPI_1783_1982$adj_factor_42 <- USCPI_1783_1982$`U.S. Consumer Price Index`/USCPI_1783_1982$`U.S. Consumer Price Index`[USCPI_1783_1982$Year == 1942]

funds <- merge(funds, USCPI_1783_1982, by.x="year", by.y="Year")

## Make per-capita measures

funds$year2 <- signif(funds$year,3) # merge by nearest decennial
funds$year2[funds$year<=1785] <- 1790 # VA
funds$year2[funds$year2<1880 & funds$state=="AK"] <- 1880
funds$year2[funds$year2==1980] <- 1983

funds <- merge(funds, census.ts.state[c('year','state','ns.pop',"land.gini","aland.gini","ns.pop","adultm","farms","farmsize","tenancy","wages","output")], 
               by.x=c('year2','state'), by.y=c('year','state'),all.x=TRUE)

funds["rev.pc"] <- NA
funds["rev.pc"] <- (funds["1"]/funds$adj_factor_82)/funds$ns.pop

funds["exp.pc"] <- NA
funds["exp.pc"] <- (funds["3"]/funds$adj_factor_82)/funds$ns.pop

funds["educ.pc"] <- NA
funds["educ.pc"] <- (funds["31"]/funds$adj_factor_42)/funds$ns.pop

# clean feature set
funds <- funds[colnames(funds) %in% c("state","year","year2",
                                      "land.gini","aland.gini","ns.pop","adultm","farms","farmsize","tenancy","wages","output",
                                      "rev.pc","exp.pc","educ.pc")]

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

rev.pc <- rev.pc[,-c(19,25,37:49)] # drop states with 0 pretreatment variance
rev.pc <- rev.pc[-which(rownames(rev.pc)=="1931"),] # discard 1931 (no variance)
rev.pc[apply(rev.pc,2,is.nan)] <- NA # replace NaN with NA

exp.pc <- reshape(data.frame(funds[c("state","year","exp.pc")]), idvar = "year", timevar = "state", direction = "wide")
colnames(exp.pc) <- sub("exp.pc.","", colnames(exp.pc))
rownames(exp.pc) <- exp.pc$year
exp.pc <- exp.pc[!colnames(exp.pc)%in% "year"]

exp.pc <- exp.pc[,-c(11,19,25,37:49)] # drop states with 0 pretreatment variance
exp.pc[apply(exp.pc,2,is.nan)] <- NA # replace NaN with NA

educ.pc <- reshape(data.frame(funds[c("state","year","educ.pc")]), idvar = "year", timevar = "state", direction = "wide")
colnames(educ.pc) <- sub("educ.pc.","", colnames(educ.pc))
rownames(educ.pc) <- educ.pc$year
educ.pc <- educ.pc[!colnames(educ.pc)%in% "year"]

educ.pc <- educ.pc[,-c(11,19,25,37:49)] # drop states with 0 pretreatment variance
educ.pc <- educ.pc[1:(nrow(educ.pc)-3),] # discard last 3 years (no variance)
educ.pc[apply(educ.pc,2,is.nan)] <- NA # replace NaN with NA

# Unit-specific covariates:
# farm values @ 1850/1860
# farm size @ 1860
# RR access @ 1850/1860

faval <- reshape(data.frame(farmval.state[c("state.abb","year","faval")]), idvar = "year", timevar = "state.abb", direction = "wide")
colnames(faval) <- sub("faval.","", colnames(faval))
rownames(faval) <- paste0("faval.",faval$year)
faval <- faval[!colnames(faval)%in% "year"]

faval[which(rownames(faval)=="faval.1850"),] <- t(na.roughfix(t(faval[which(rownames(faval)=="faval.1850"),]))) # replace missing 1850 values with column medians

farmsize <- reshape(data.frame(census.ts.state[c("state","year","farmsize")]), idvar = "year", timevar = "state", direction = "wide")
colnames(farmsize) <- sub("farmsize.","", colnames(farmsize))
rownames(farmsize) <- paste0("farmsize.",farmsize$year)
farmsize <- farmsize[!colnames(farmsize)%in% "year"]

access <- reshape(data.frame(rr.inter.m.state[c("year","state","track2")]), idvar = "year", timevar = "state", direction = "wide")
colnames(access) <- sub("track2.","", colnames(access))
rownames(access) <- paste0("track2.",access$year)
access <- access[!colnames(access)%in% "year"]

capacity.states <- sort(unique(c(colnames(rev.pc),colnames(exp.pc),colnames(educ.pc))))

capacity.covariates <- rbind(faval[colnames(faval) %in% capacity.states][which(rownames(faval)=="faval.1850"),],
                             faval[colnames(faval) %in% capacity.states][which(rownames(faval)=="faval.1860"),], 
                             farmsize[colnames(farmsize) %in% capacity.states][which(rownames(farmsize)=="farmsize.1860"),],
                             access[colnames(access) %in% capacity.states][which(rownames(access)=="track2.1850"),],
                             access[colnames(access) %in% capacity.states][which(rownames(access)=="track2.1860"),])

capacity.covariates.placebo <- rbind(faval[colnames(faval) %in% capacity.states][which(rownames(faval)=="faval.1850"),], 
                             access[colnames(access) %in% capacity.states][which(rownames(access)=="track2.1850"),])

capacity.outcomes <- list("rev.pc"=rev.pc,"exp.pc"=exp.pc, "educ.pc"=educ.pc)

CapacityMatrices <- function(d, outcomes=TRUE, imp=c("locf","linear","random","median")) {
  
  # Masked matrix for which 1=observed, NA=missing/imputed
  d.M.missing <- t(as.matrix(d))
  d.M.missing[is.nan(d.M.missing)] <- NA
  d.M.missing[!is.na(d.M.missing)] <-1
  
  # impute missing
  d.imp <- d
  if(outcomes){
    if(imp=="locf"){
      d.imp[1:which(rownames(d)=="1868"),] <- na.locf(d[1:which(rownames(d)=="1868"),], option = "locf", na.remaining = "rev")
      d.imp[which(rownames(d)=="1868"): nrow(d),] <- na.locf(d[which(rownames(d)=="1868"): nrow(d),], option = "locf", na.remaining = "rev")  
    }
    if(imp=="linear"){
      d.imp[1:which(rownames(d)=="1868"),] <- na.interpolation(d[1:which(rownames(d)=="1868"),], option = "linear")
      d.imp[which(rownames(d)=="1868"): nrow(d),] <- na.interpolation(d[which(rownames(d)=="1868"): nrow(d),], option = "linear")  
    }
    if(imp=="random"){
      d.imp[1:which(rownames(d)=="1868"),] <- na.random(d[1:which(rownames(d)=="1868"),])
      d.imp[which(rownames(d)=="1868"): nrow(d),] <- na.random(d[which(rownames(d)=="1868"): nrow(d),])  
    }
    if(imp=="median"){
      preProcValues <- preProcess(d[1:which(rownames(d)=="1868"),], method = c("medianImpute"), verbose=TRUE) # use training set median
      d.imp <- predict(preProcValues, d)
    }
  } 
  
  d.imp <- log(d.imp+.Machine
               $double.eps) # take log
  
  # Matrix of observed entries (N x T)
  d.M <- t(as.matrix(d.imp))
  d.M[is.nan(d.M )] <- NA
  
  rownames(d.M) <- colnames(d)
  colnames(d.M) <- rownames(d)
  
  if(outcomes){
    
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
  
  return(list("M"=d.M, "M.missing"=d.M.missing, "mask"=d.mask))
  } else{
    return(d.M)
  }
}

capacity.outcomes.locf <- lapply(capacity.outcomes, CapacityMatrices, outcomes=TRUE, imp="locf")
capacity.covariates.locf <- CapacityMatrices(capacity.covariates, outcomes=FALSE, imp=NULL)
capacity.covariates.placebo.locf <- CapacityMatrices(capacity.covariates.placebo, outcomes=FALSE, imp=NULL)

saveRDS(capacity.outcomes.locf, "/media/jason/Dropbox/github/land-reform/data/capacity-outcomes-locf.rds")
saveRDS(capacity.covariates.locf, "/media/jason/Dropbox/github/land-reform/data/capacity-covariates.rds")
saveRDS(capacity.covariates.placebo.locf, "/media/jason/Dropbox/github/land-reform/data/capacity-covariates-placebo.rds")

capacity.outcomes.linear <- lapply(capacity.outcomes, CapacityMatrices, outcomes=TRUE, imp="linear")
capacity.outcomes.random <- lapply(capacity.outcomes, CapacityMatrices, outcomes=TRUE, imp="random")
capacity.outcomes.median <- lapply(capacity.outcomes, CapacityMatrices, outcomes=TRUE, imp="median")

saveRDS(capacity.outcomes.linear, "/media/jason/Dropbox/github/land-reform/data/capacity-outcomes-linear.rds")
saveRDS(capacity.outcomes.random, "/media/jason/Dropbox/github/land-reform/data/capacity-outcomes-random.rds")
saveRDS(capacity.outcomes.median, "/media/jason/Dropbox/github/land-reform/data/capacity-outcomes-median.rds")
