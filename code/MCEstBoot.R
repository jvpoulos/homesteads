MCEstBoot <- function(tseries,M.missing,mask,p.weights,imputed=FALSE) {
  
  Y <- t(tseries) # NxT 
  Y.missing <- M.missing # NxT
  p.weights <- p.weights # NxT 
  
  treat <- mask # NxT masked matrix 
  
  N <- nrow(treat)
  T <- ncol(treat)
  
  treat_mat <- 1-treat
  
  Y_obs <- Y * treat_mat
  
  ## ------
  ## MC-NNM
  ## ------
  
  est_model_MCPanel <- mcnnm_cv(Y_obs, mask=treat_mat, W=p.weights, to_estimate_u = 1, to_estimate_v = 1, 
                                num_lam_L = 30, num_folds = 5, niter = 200, rel_tol = 1e-05)
  est_model_MCPanel$Mhat <- est_model_MCPanel$L + replicate(T,est_model_MCPanel$u) + t(replicate(N,est_model_MCPanel$v))
  
  if(imputed){
    est_model_MCPanel$impact <- (Y*Y.missing-est_model_MCPanel$Mhat)
  } else{
    est_model_MCPanel$impact <- (Y-est_model_MCPanel$Mhat)
  }
  
  return(est_model_MCPanel$impact)
}