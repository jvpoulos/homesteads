##########################################
### Prepare average farm values        ###
##########################################

farmval <- read.csv(paste0(census.data.directory,"census-county/farmval.csv"), stringsAsFactors=FALSE)

farmval$year <- farmval$year +1000 # fix year

# keep states
farmval.state <- farmval[farmval$county==0,]

# state abbr
farmval.state$state.abb <- setNames(state.abb, state.name)[tools::toTitleCase(tolower(farmval.state$name))]