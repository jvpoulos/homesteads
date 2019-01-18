basque <- read.csv("/media/jason/Dropbox/github/gans-causal/data/basque/treated/basque-x.csv")
germany <- read.csv("/media/jason/Dropbox/github/gans-causal/data/germany/treated/germany-x.csv")
california <- read.csv("/media/jason/Dropbox/github/gans-causal/data/california/treated/california-x.csv")

basque.yz <- read.csv("/media/jason/Dropbox/github/gans-causal/data/basque/treated/basque-yz.csv") 
basque.xz <- readRDS("/media/jason/Dropbox/github/gans-causal/data/basque/treated/basque-xz.rds")

germany.yz <- read.csv("/media/jason/Dropbox/github/gans-causal/data/germany/treated/germany-yz.csv") 
germany.xz <- readRDS("/media/jason/Dropbox/github/gans-causal/data/germany/treated/germany-xz.rds")

california.yz <- read.csv("/media/jason/Dropbox/github/gans-causal/data/california/treated/california-yz.csv") 
california.xz <- readRDS("/media/jason/Dropbox/github/gans-causal/data/california/treated/california-xz.rds")

control.outcomes <- list("basque"=basque,"germany"=germany, "california"=california)
treat.covars <- list("basque.yz"=basque.yz, "germany.yz"=germany.yz, "california.yz"=california.yz)
control.covars <- list("basque.xz"=basque.xz, "germany.xz"=germany.xz, "california.xz"=california.xz)

controls.outcomes <- lapply(control.outcomes, function(d) {

  # Matrix of observed entries (N x T)
  d.M <- t(as.matrix(d))
  d.M[is.nan(d.M )] <- NA
  
  # Masked matrix which is 0 for control units and treated units before treatment and 1 for treated units after treatment.
  
  d.mask <- matrix(0, nrow = nrow(d.M), 
                   ncol= ncol(d.M),
                   dimnames = list(rownames(d.M), colnames(d.M)))
  
  return(list("M"=d.M, "mask"=d.mask))
})

treat.covars <- lapply(treat.covars, function(d) {
  
  # Matrix containing unit-related covariates (N x T)
  d.X <- t(as.matrix(d))
  d.X[is.nan(d.X)] <- NA
  
  # Matrix containing time-related covariates (T x N)
  d.Z <- t(as.matrix(d))
  d.Z[is.nan(d.Z)] <- NA
  
  return(list("X"=d.X, "Z"=d.Z))
})

control.covars <- lapply(control.covars, function(d) {
    # Matrix containing unit-related covariates (N x T)
    d.X <- t(as.matrix(d[[x]]))
    d.X[is.nan(d.X)] <- NA
  
    # Matrix containing time-related covariates (T x N)
    d.Z <- t(as.matrix(d[[x]]))
    d.Z[is.nan(d.Z)] <- NA
  return(list("X"=d.X, "Z"=d.Z))
})

saveRDS(controls.outcomes, "/media/jason/Dropbox/github/land-reform/data/synth-control-outcomes.rds")
saveRDS(treat.covars, "/media/jason/Dropbox/github/land-reform/data/treat-covars.rds")
saveRDS(control.covars, "/media/jason/Dropbox/github/land-reform/data/control-covars.rds")