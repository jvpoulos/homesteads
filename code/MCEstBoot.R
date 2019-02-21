MCEstBoot <- function(tseries, M.missing, mask, t0, simul=FALSE) {
  
  Y <- t(tseries) # NxT 
  Y.missing <- M.missing # NxT 
  
  treat <- mask # NxT masked matrix 
  
  N <- nrow(treat)
  T <- ncol(treat)
  
  ## Treated indices
  indices <- cbind("id"=1:nrow(Y), "name"=rownames(Y))
  treat_indices_order <- c("CA", "CO", "IA", "KS", "MI", "MN", "MO", "NE", "OH", "OR", "SD", "WA", "WI", "IL", "NV", "ID", "MT", "ND",  "UT", "AL", "MS", "AR", "FL", "LA", "IN", "NM", "WY", "AZ", "OK", "AK")
  treat_indices <- as.numeric(indices[order(match(indices[,2], treat_indices_order))][1:length(treat_indices_order)]) # sort indices increasingly based on T0
  
  N_t <- length(treat_indices) # Number of treated units desired
  T0 <- t0 #which(colnames(Y)=="1869")# The first treatment time
  
  ## Simultaneuous (simul_adapt) or Staggered adoption (stag_adapt)
  if(simul){
    treat_mat <- simul_adapt(Y, N_t, T0, treat_indices)
  }else{
    treat_mat <- stag_adapt(Y, N_t, T0, treat_indices)
  }
  
  Y_obs <- Y * treat_mat

    ## ------
    ## MC-NNM
    ## ------
    
    est_model_MCPanel <- mcnnm_fit(Y_obs, treat_mat, lambda_L=1, to_estimate_u = 1, to_estimate_v = 1) 
    est_model_MCPanel$Mhat <- est_model_MCPanel$L + replicate(T,est_model_MCPanel$u) + t(replicate(N,est_model_MCPanel$v))
    est_model_MCPanel$impact <- (est_model_MCPanel$Mhat- Y) 
    
    return(est_model_MCPanel$impact)
}