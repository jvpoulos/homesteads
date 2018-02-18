###################################
# Main                            #
###################################

# Setup parallel processing 
require(parallel)
require(doParallel)

cores <- detectCores() # specify number of cores to use

registerDoParallel(cores) # register cores

RNGkind("L'Ecuyer-CMRG") # ensure random number generation

# Set directories
data.directory <-"~/Dropbox/github/land-reform/data/"
code.directory <-"~/Dropbox/github/land-reform/code/"
homestead.data.directory <-"~/Dropbox/github/homestead/data/"
homestead.code.directory <-"~/Dropbox/github/homestead/code/"
results.directory <-"~/Dropbox/github/land-reform/results/"

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

# Estimates on Rhode and Strumpf tax data

source(paste0(code.directory,'taxes-revenues.R')) # prepare taxes data
source(paste0(code.directory,'prepare-taxes.R')) # run homesteads first

source(paste0(code.directory,'gbr-taxes.R')) 
source(paste0(code.directory,'fe-taxes.R')) 

# Estimates on RR access

source(paste0(code.directory,'railroads.R'))  # rr.inter.m
source(paste0(code.directory,'prepare-railroads.R'))

source(paste0(code.directory,'gbr-rr.R')) 
source(paste0(code.directory,'fe-rr.R')) 

# Estimates on farm values

source(paste0(code.directory,'prepare-farmval.R')) 

source(paste0(code.directory,'fe-farm.R')) 

# Descriptive plots, scatter plots

source(paste0(code.directory,'descriptive.R')) # Descriptive plots

source(paste0(code.directory,'ineq-taxes.R')) # inequality vs. fiscal capacity

# Robustness: taxes/inequality

source(paste0(code.directory,'fe-robust.R')) 