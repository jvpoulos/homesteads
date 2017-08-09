###################################
# Main                            #
###################################

# Set directories
data.directory <-"~/Dropbox/github/land-reform/data/"
code.directory <-"~/Dropbox/github/land-reform/code/"
results.directory <-"~/Dropbox/github/land-reform/results/"

# Causal estimates on land patents

source('patents.R') # load and export patents data

source('impact-plots-sales.R') # effect of 1866 SHA on treated (south) sales

source('impact-plots-patents.R') # effect of 1889 HSA restrictions on treated (south) sales and homesteads 

source('dd-patents.R') # DD estimates for comparison

# Causal estimates on state capacity

source('capacity.R')  # load and export capacity data