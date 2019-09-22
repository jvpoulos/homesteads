MCEst <- function(outcomes,imputed=FALSE,covars=NULL) {
  
  Y <- outcomes$M # NxT 
  Y.missing <- outcomes$M.missing # NxT 

  if(!is.null(covars)){
    covars <- covars[rownames(covars) %in% row.names(Y),]
    covars <- covars[row.names(Y),]  # reorder
  }
  
  treat <- outcomes$mask # NxT masked matrix 
  
  N <- nrow(treat)
  T <- ncol(treat)
  
  treat_mat <- 1-treat
  
  Y_obs <- Y * treat_mat
  
  if(!is.null(covars)){
    ## ------
    ## MC-NNM-W
    ## ------
    
    est_model_MCPanel_w <- mcnnm_wc_cv(M=Y_obs, X = covars, Z=matrix(0L,0,0), mask=treat_mat,
                                       to_normalize = 1, to_estimate_u = 1, to_estimate_v = 1, to_add_ID = 1, 
                                       num_lam_L = 30, num_lam_H = 30, niter = 100, rel_tol = 1e-05, cv_ratio = 0.8, 
                                       num_folds = 1,
                                       is_quiet = 1) 
    
    est_model_MCPanel_w$Mhat <- est_model_MCPanel_w$L + replicate(T,est_model_MCPanel_w$u) + t(replicate(N,est_model_MCPanel_w$v))
    if(imputed){
      est_model_MCPanel_w$impact <- (Y*Y.missing-est_model_MCPanel_w$Mhat)
    } else{
      est_model_MCPanel_w$impact <- (Y-est_model_MCPanel_w$Mhat)
    }
    
    return(list("impact" = est_model_MCPanel_w$impact, "Mhat" = est_model_MCPanel_w$Mhat, "min_RMSE"= est_model_MCPanel_w$min_RMSE, "best_lambda"=est_model_MCPanel_w$best_lambda))
  } else{
    ## ------
    ## MC-NNM
    ## ------
    
    est_model_MCPanel <- mcnnm_cv(Y_obs, treat_mat, to_estimate_u = 1, to_estimate_v = 1, 
                                  num_lam_L = 100, niter = 400, rel_tol = 1e-05, cv_ratio = 0.8, num_folds = 5, is_quiet = 1)
    est_model_MCPanel$Mhat <- est_model_MCPanel$L + replicate(T,est_model_MCPanel$u) + t(replicate(N,est_model_MCPanel$v))
    if(imputed){
      est_model_MCPanel$impact <- (Y*Y.missing-est_model_MCPanel$Mhat)
    } else{
      est_model_MCPanel$impact <- (Y-est_model_MCPanel$Mhat)
    }

    return(list("impact" = est_model_MCPanel$impact, "Mhat" = est_model_MCPanel$Mhat, "min_RMSE"= est_model_MCPanel$min_RMSE, "best_lambda"=est_model_MCPanel$best_lambda))
  }
}