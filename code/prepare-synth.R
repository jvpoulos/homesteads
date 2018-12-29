basque <- read.csv("/media/jason/Dropbox/github/gans-causal/data/basque/treated/basque-x.csv")
germany <- read.csv("/media/jason/Dropbox/github/gans-causal/data/germany/treated/germany-x.csv")
california <- read.csv("/media/jason/Dropbox/github/gans-causal/data/california/treated/california-x.csv")

dfList <- list("basque"=basque,"germany"=germany, "california"=california)

dfList <- lapply(dfList, function(d) {

  # Matrix of observed entries (N x T)
  d.M <- t(as.matrix(d))
  d.M[is.nan(d.M )] <- NA
  
  # Masked matrix which is 0 for control units and treated units before treatment and 1 for treated units after treatment.
  
  d.mask <- matrix(0, nrow = nrow(d.M), 
                   ncol= ncol(d.M),
                   dimnames = list(rownames(d.M), colnames(d.M)))
  
  return(list("M"=d.M, "mask"=d.mask))
})

saveRDS(dfList, "/media/jason/Dropbox/github/land-reform/data/synth-data")