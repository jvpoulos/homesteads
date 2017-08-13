###################################
# Main                            #
###################################

# Set directories
data.directory <-"~/Dropbox/github/land-reform/data/"
code.directory <-"~/Dropbox/github/land-reform/code/"
results.directory <-"~/Dropbox/github/land-reform/results/"

# Causal estimates on land patents

source(paste0(code.directory,'patents.R')) # load and export patents data

source(paste0(code.directory,'impact-plots-sales.R')) # effect of 1866 SHA on treated (south) sales

source(paste0(code.directory,'impact-plots-patents.R')) # effect of 1889 HSA restrictions on treated (south) sales and homesteads 

source(paste0(code.directory,'dd-patents.R')) # DD estimates for comparison

# Causal estimates on state capacity

source(paste0(code.directory,'census-county-state.R')) # get state-level pop. data

source(paste0(code.directory,'capacity-state.R'))  # load and export capacity data