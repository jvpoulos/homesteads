###################################
# Main                            #
###################################

# Set directories
data.directory <-"~/Dropbox/github/land-reform/data/"
code.directory <-"~/Dropbox/github/land-reform/code/"
results.directory <-"~/Dropbox/github/land-reform/results/"

# State groups

pub.states <- c("AK","AL","AR","AZ","CA","CO","FL","IA","ID","IL","IN","KS","LA","MI","MN","MO","MS","MT","ND","NE","NM","NV","OH","OK","OR","SD","UT","WA","WI","WY") # 30 public land states
state.land.states <- state.abb[!state.abb %in% pub.states] # 20 state land states
southern.pub <- c("AL", "AR", "FL", "LA", "MS") # 5 southern public land states
southern.state <- c("GA","NC","SC","TN","TX","VA") # 6 southern state land states

# Causal estimates on land patents

source(paste0(code.directory,'patents.R')) # load and export patents data

source(paste0(code.directory,'impact-plots-sales.R')) # effect of 1866 SHA on treated (south) sales

source(paste0(code.directory,'impact-plots-patents.R')) # effect of 1889 HSA restrictions on treated (south) sales and homesteads 

source(paste0(code.directory,'dd-patents.R')) # DD estimates for comparison

# Causal estimates on state capacity

source(paste0(code.directory,'census-county-state.R')) # get state-level pop. data

source(paste0(code.directory,'capacity-state.R'))  # load and export capacity data

source(paste0(code.directory,'impact-plots-rev-exp.R')) # effects of 1866 SHA/1889 HSA restrictions on revenue and expenditure

source(paste0(code.directory,'impact-plots-ed-exp.R')) # effects 866 SHA/1889 HSA restrictions treated (south) education

source(paste0(code.directory,'dd-capacity.R')) 

# DD estimates on inequality and tenancy

source(paste0(code.directory,'dd-census.R')) 

# Causal/DD estimates on railroads

source(paste0(code.directory,'railroads.R')) 

source(paste0(code.directory,'dd-railroads.R')) 

source(paste0(code.directory,'impact-plots-rr.R')) # effects 866 SHA/1889 HSA restrictions treated (south) education

# Descriptive plots

source(paste0(code.directory,'descriptive.R')) 

source(paste0(code.directory,'attention-plot-capacity.R')) # Attention heatmap

source(paste0(code.directory,'attention-plot-patents.R')) # Attention heatmap