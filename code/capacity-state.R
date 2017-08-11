############################################################################
# Prepare capacity data (state-level or aggregated at county level)        #
############################################################################

require(doParallel)
require(data.table)
require(reshape2)
require(stringr)
library(dplyr)
library(weights)
library(caret)
library(zoo)
library(tidyr)

## STATE-LEVEL DATA

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

# Keep iso codes of interest

iso.funds <- c(1,3,31)

names.funds <- c("total.rev",'total.exp','ed.exp')

funds.9728 <- funds.9728[funds.9728$iso %in% iso.funds,] # keep obs with valid iso codes

# Make data wide

funds.9728$row <- 1:nrow(funds.9728)
funds.9728 <- spread(funds.9728, key = iso, value = total)

colnames(funds.9728) <- c("state","year","row", names.funds)

# Collapse by state/year

funds.9728 <- funds.9728 %>% 
  group_by(state,year) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)),total.rev,total.exp,ed.exp) 

funds.9728$total.exp[funds.9728$total.exp<0] <- NA # replace neg value with NA

# Clean state codes
funds.9728$state[funds.9728$state=="MIS"] <- "MS"
funds.9728$state[funds.9728$state=="IO"] <- "IA" 
funds.9728$state[funds.9728$state=="KA"] <- "KS" 

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

funds.6306 <- funds.6306[with(funds.6306, order(state, year)), ] # order by state and year

# Keep iso codes of interest

funds.6306 <- funds.6306[funds.6306$iso %in% iso.funds,] # keep obs with valid iso codes

# Make data wide

funds.6306$row <- 1:nrow(funds.6306)
funds.6306 <- spread(funds.6306, key = iso, value = value)

colnames(funds.6306) <- c("state","year","row", names.funds)

# Collapse by state/year

funds.6306 <- funds.6306 %>% 
  group_by(state,year) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)),total.rev,total.exp,ed.exp) 

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

funds.6304 <- funds.6304[funds.6304$type=='SSS' & funds.6304$iso %in% c(1,3,3100),] # keep state gov't, obs with valid iso codes

# Make data wide

funds.6304$row <- 1:nrow(funds.6304)
funds.6304 <- spread(funds.6304, key = iso, value = value)

colnames(funds.6304) <- c("state","year","type","row", names.funds)

# Collapse by state/year

funds.6304 <- funds.6304 %>% 
  group_by(state,year) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)),total.rev,total.exp,ed.exp) 

## Append Wallis datasets

funds <- rbind(rbind(funds.9728,funds.6306),funds.6304) %>% 
  group_by(state,year) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)),total.rev,total.exp,ed.exp)  # takes mean of duplicates from 1902,1913,1932

funds <- funds[with(funds, order(state, year)), ] # order by state and year

## Make per-capita measures

# Source state-level census data