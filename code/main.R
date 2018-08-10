###################################
# Main                            #
###################################

# Setup parallel processing 
library(parallel)
library(doParallel)

cores <- detectCores() # specify number of cores to use

registerDoParallel(cores) # register cores

RNGkind("L'Ecuyer-CMRG") # ensure random number generation

# Set directories
data.directory <-"~/Dropbox/github/land-reform/data/"
code.directory <-"~/Dropbox/github/land-reform/code/"
gans.code.directory <-"~/Dropbox/github/gans-causal/code/"
homestead.data.directory <-"~/Dropbox/github/homestead/data/"
homestead.code.directory <-"~/Dropbox/github/homestead/code/"
results.directory <-"~/Dropbox/github/land-reform/results/"
census.data.directory <- "~/Dropbox/github/ok-lottery/data/"

# State groups

pub.states <- c("AK","AL","AR","AZ","CA","CO","FL","IA","ID","IL","IN","KS","LA","MI","MN","MO","MS","MT","ND","NE","NM","NV","OH","OK","OR","SD","UT","WA","WI","WY") # 30 public land states
state.land.states <- state.abb[!state.abb %in% pub.states] # 20 state land states
southern.pub <- c("AL", "AR", "FL", "LA", "MS") # 5 southern public land states
western.pub <- setdiff(pub.states,southern.pub)
southern.state <- c("GA","NC","SC","TN","TX","VA") # 6 southern state land states

# Get census and patents data
source(paste0(code.directory,'census-county-state.R'))

source(paste0(homestead.code.directory,'patents-homestead.R'))
source(paste0(homestead.code.directory,'prepare-homestead.R'))

source(paste0(code.directory,'patents.R'))

# Prepare state capacity data

source(paste0(code.directory,'capacity-state.R')) # state-level "rev.pc","exp.pc","educ.pc"
source(paste0(code.directory,'prepare-capacity.R')) # prepare for DD # run homesteads first

source(paste0(code.directory,'prepare-farmval-state.R')) # farm vals for states

# DD estimates on capacity data

source(paste0(code.directory,'dd-capacity.R')) # state-level capacity

## Encoder-decoder estimates on capacity data

source(paste0(gans.code.directory,'utils.R'))

source(paste0(code.directory,'encoder-decoder-south-educpc.R')) 
source(paste0(code.directory,'encoder-decoder-south-exppc.R')) 
source(paste0(code.directory,'encoder-decoder-south-revpc.R')) 

source(paste0(code.directory,'encoder-decoder-west-educpc.R')) 
source(paste0(code.directory,'encoder-decoder-west-exppc.R')) 
source(paste0(code.directory,'encoder-decoder-west-revpc.R')) 

source(paste0(code.directory,'mspe-funds-encoder-decoder.R')) # Get MSPE sds

# DD estimates on RR access

source(paste0(code.directory,'railroads.R'))  # rr.inter.m
source(paste0(code.directory,'prepare-railroads.R'))
source(paste0(code.directory,'prepare-farmval.R')) # farm vals counties

source(paste0(code.directory,'dd-railroads.R'))

# DD estimates on inequality

source(paste0(code.directory,'dd-inequality.R')) # run dd-railroads first

# Descriptive plots, scatter plots

source(paste0(code.directory,'homestead-map.R')) # Map homesteads

source(paste0(code.directory,'descriptive.R')) # Descriptive plots
source(paste0(code.directory,'funds-descriptive.R')) # state capacity plots

source(paste0(code.directory,'ineq-capacity.R')) # inequality vs. state capacity