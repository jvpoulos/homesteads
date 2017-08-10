###################################
# Prepare capacity data            #
###################################

require(doParallel)
require(data.table)
require(reshape2)
require(stringr)
library(dplyr)
library(weights)
library(caret)
library(zoo)
library(tidyr)

## State and Local Government: Sources and Uses of Funds, City and County Data, Nineteenth Century (ICPSR 6305)

# Set location of files
setwd(paste0(data.directory, "capacity/","ICPSR_06305"))

county.vars <- c("county.name","state","type","year","iso","value")

# Import files 

data.files.6305 <- list.files(pattern = "*Data.txt", recursive = TRUE)

rev.spend.6305 <- do.call(rbind,lapply(data.files.6305[3:4],read.csv, # county  expenditures  and  revenues
                                    header=FALSE,
                                    row.names=NULL,
                                    sep = ",",
                                    stringsAsFactors=FALSE,
                                    col.names = county.vars)) 

debt.6305 <- do.call(rbind,lapply(data.files.6305[5],read.csv, # county debt totals
                                 header=FALSE,
                                 row.names=NULL,
                                 sep = ",",
                                 stringsAsFactors=FALSE,
                                 col.names = county.vars))

property.6305 <- do.call(rbind,lapply(data.files.6305[6],read.csv, # county debt totals
                                       header=FALSE,
                                       row.names=NULL,
                                       sep = ",",
                                       stringsAsFactors=FALSE,
                                       col.names = county.vars))

# Clean
rev.spend.6305 <- rev.spend.6305[!is.na(rev.spend.6305$year),] # drop empty rows
rev.spend.6305 <- rev.spend.6305[with(rev.spend.6305, order(state,county.name, year)), ] # order

property.6305 <- property.6305[with(property.6305, order(state,county.name, year)), ] # order

debt.6305 <- debt.6305[with(debt.6305, order(state,county.name, year)), ] # order

# Make data wide

rev.spend.6305 <- spread(rev.spend.6305, key = iso, value = value)
rev.spend.6305 <- subset(rev.spend.6305, select=c(county.vars[c(1:4)],"1","10","11",
                                                  "3"))  
colnames(rev.spend.6305) <- c(county.vars[c(1:4)],
                              "total.rev","tax","property.tax", # total revenue ---> taxes  ---> property taxes
                                       "total.exp") # total expenditures 
            
property.6305 <- spread(property.6305, key = iso, value = value)
colnames(property.6305)[-c(1:4)] <- c("assessed.value","real.value","personal.value")

