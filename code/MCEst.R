MCEst <- function(outcomes,imputed=FALSE) {
  
  Y <- outcomes$M # NxT 
  Y.missing <- outcomes$M.missing # NxT 
  p.weights <- outcomes$p.weights # NxT 
  
  treat <- outcomes$mask # NxT masked matrix 
  
  N <- nrow(treat)
  T <- ncol(treat)
  
  treat_mat <- 1-treat
  
  Y_obs <- Y * treat_mat
  
  ## ------
  ## MC-NNM
  ## ------
  
  est_model_MCPanel <- mcnnm_cv(Y_obs, mask=treat_mat, W=p.weights, to_estimate_u = 1, to_estimate_v = 1, num_lam_L = 30, num_folds = 5, niter = 200, rel_tol = 1e-05)
  est_model_MCPanel$Mhat <- est_model_MCPanel$L + replicate(T,est_model_MCPanel$u) + t(replicate(N,est_model_MCPanel$v))
  est_model_MCPanel$rankL <- rankMatrix(t(est_model_MCPanel$L), method="qr.R")[1]
  
  if(imputed){
    est_model_MCPanel$impact <- (Y*Y.missing-est_model_MCPanel$Mhat)
  } else{
    est_model_MCPanel$impact <- (Y-est_model_MCPanel$Mhat)
  }
  
  return(list("impact" = est_model_MCPanel$impact, "Mhat" = est_model_MCPanel$Mhat, "min_RMSE"= est_model_MCPanel$min_RMSE, "best_lambda"=est_model_MCPanel$best_lambda, "rankL"=est_model_MCPanel$rankL))
}