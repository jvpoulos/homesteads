###################################
# MC Estimation  #
###################################

## Loading Source files
library(MCPanel)
library(glmnet)
library(ggplot2)
library(missForest)
library(latex2exp)
library(softImpute)
library(bcaboot)

# Setup parallel processing 
library(parallel)
library(doParallel)

cores <- detectCores()

registerDoParallel(cores) # register cores (<p)

RNGkind("L'Ecuyer-CMRG") # ensure random number generation

# Load data
load("capacity-state.RData")

## Treated indices
indices <- cbind("id"=1:nrow(Y), "name"=rownames(Y))
treat_indices_order <- c("CA", "CO", "IA", "KS", "MI", "MN", "MO", "NE", "OH", "OR", "SD", "WA", "WI", "IL", "NV", "ID", "MT", "ND",  "UT", "AL", "MS", "AR", "FL", "LA", "IN", "NM", "WY", "AZ", "OK", "AK")
treat_indices <- as.numeric(indices[order(match(indices[,2], treat_indices_order))][1:length(treat_indices_order)]) # sort indices increasingly based on T0

N_t <- length(treat_indices) # Number of treated units desired
T0 <- which(colnames(Y)=="1869")# The first treatment time

## Reading data

for(d in c('rev.pc','exp.pc')){
  Y <- dfList[[d]]$M # NxT 
  treat <- dfList[[d]]$mask  # NxT masked matrix 
  years <- colnames(Y) # T-length 
  
  treat_mat <- stag_adapt(Y, N_t, T0, treat_indices)
  
  treat_mat_NA <- treat_mat
  treat_mat_NA[treat_mat==0] <- NA
  
  missing_mat <- dfList[[d]]$M.missing
  
  Y_obs <- Y * treat_mat
  
  ## ------
  ## MC-NNM
  ## ------
  
  est_model_MCPanel <- mcnnm_cv(Y_obs, treat_mat, to_estimate_u = 1, to_estimate_v = 1, num_folds = 5)
  est_model_MCPanel$Mhat <- est_model_MCPanel$L + replicate(T,est_model_MCPanel$u) + t(replicate(N,est_model_MCPanel$v))
  est_model_MCPanel$impact <- (est_model_MCPanel$Mhat - Y*missing_mat)*(1-treat_mat) # use non-imputed Y
  
}