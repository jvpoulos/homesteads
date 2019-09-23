###################################
# Main                            #
###################################

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
source(paste0(code.directory,'homestead-heatmap.R')) 

## Prepare state capacity, census and RR data

source(paste0(code.directory,'prepare-farmval-state.R')) # farm vals for states

source(paste0(code.directory,'railroads.R'))  # rr.inter.m

source(paste0(code.directory,'capacity-state.R')) # prepare for mc
save.image("~/data/capacity.RData")

## MC experiments on synth data

source(paste0(code.directory,'prepare-synth.R')) 

## MC causal estimates on state capacity data

# mc-capacity-placebo.sh --> mc-capacity-placebo.R  # placebo tests
# mc-capacity-placebo-covars.sh --> mc-capacity-placebo-covars.R  # placebo tests

# mc-capacity-estimates.sh --> mc-capacity-estimates.R 
# mc-capacity-estimates-covars.sh --> mc-capacity-estimates-covars.R 

source(paste0(code.directory,'mc-capacity-placebo-table.R')) 
source(paste0(code.directory,'mc-capacity-table.R')) 

# mc-boot.sh --> mc-boot.R
# mc-boot-linear.R
# mc-boot-median.R
# mc-boot-random.R
source(paste0(code.directory,'mc-capacity-plots.R')) 

# Sensitivity: imputation method
# mc-capacity-estimates-covars-linear.sh --> mc-capacity-estimates-covars-linear.R 
# mc-capacity-estimates-covars-random.sh --> mc-capacity-estimates-covars-random.R 
# mc-capacity-estimates-covars-median.sh --> mc-capacity-estimates-covars-median.R 

source(paste0(code.directory,'mc-capacity-table-imp.R')) 

# DD estimates on state capacity data

source(paste0(code.directory,'dd-capacity.R')) 

# DD estimates on inequality

source(paste0(code.directory,'dd-inequality.R')) 

# Descriptive plots

source(paste0(code.directory,'ineq-capacity.R')) # inequality vs. state capacity