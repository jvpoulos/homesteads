###################################
# Main                            #
###################################

# create plots dir. if it doesn't already exist
directory <- getwd()
plots.directory <- "results/plots"

if(!dir.exists(file.path(directory, plots.directory))){
  print("creating results/plots directory")
  dir.create(file.path(directory, plots.directory))
} else{
  print("results/plots directory already exists")
}

## Set directories
data.directory <-"data/"
code.directory <-"code/"
results.directory <-"results/"

## State groups

pub.states <- c("AK","AL","AR","AZ","CA","CO","FL","IA","ID","IL","IN","KS","LA","MI","MN","MO","MS","MT","ND","NE","NM","NV","OH","OK","OR","SD","UT","WA","WI","WY") # 30 public land states
state.land.states <- state.abb[!state.abb %in% pub.states] # 20 state land states
southern.pub <- c("AL", "AR", "FL", "LA", "MS") # 5 southern public land states
western.pub <- setdiff(pub.states,southern.pub)
southern.state <- c("GA","NC","SC","TN","TX","VA") # 6 southern state land states

## Get census and patents data
source(paste0(code.directory,'census-county-state.R'))

source(paste0(code.directory,'patents-homestead.R'))
source(paste0(code.directory,'prepare-homestead.R'))

source(paste0(code.directory,'patents.R'))

## Prepare state capacity, census and RR data

source(paste0(code.directory,'prepare-farmval-state.R')) # farm vals for states

source(paste0(code.directory,'railroads.R'))  # rr.inter.m

source(paste0(code.directory,'capacity-state.R')) # prepare state capacity data
save.image(file=paste0(results.directory,'capacity-state.RData'))

source(paste0(code.directory,'non-response-plot.R')) # Summarize non-response and treatment status

## MC estimates: experiments and causal estimates (see README)

source(paste0(code.directory,'prepare-synth.R')) # prepare synthetic control datasets for experiments

tablesfigures <- FALSE # set to TRUE to produce tables and Figures
if(tablesfigures){
  source(paste0(code.directory,'simultation-plots.R')) # combine simulation plots
}

if(tablesfigures){
  source(paste0(code.directory,'mc-capacity-plots.R')) 
}

# DD estimates on state capacity data

source(paste0(code.directory,'dd-capacity.R')) 

# DD estimates on inequality

source(paste0(code.directory,'dd-inequality.R'))  # N.B.: need to run dd-capacity.R first

if(tablesfigures){
  # Descriptive plots
  source(paste0(code.directory,'ineq-capacity.R')) # inequality vs. state capacity
}