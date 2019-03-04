MCEstBoot <- function(tseries, M.missing, mask, t0, treat_indices_order,imputed=FALSE,simul=FALSE,covars=NULL,pca=FALSE) {
  
  Y <- t(tseries) # NxT 
  Y.missing <- M.missing # NxT 
  
  
  if(!is.null(covars)){
    covars <- covars[rownames(covars) %in% row.names(Y),]
    covars <- covars[row.names(Y),]  # reorder
  }
  
  treat <- mask # NxT masked matrix 
  
  N <- nrow(treat)
  T <- ncol(treat)
  
  ## Treated indices
  indices <- cbind("id"=1:nrow(Y), "name"=rownames(Y))
  treat_indices <- as.numeric(indices[order(match(indices[,2], treat_indices_order))][1:length(treat_indices_order)]) # sort indices increasingly based on T0
  
  N_t <- length(treat_indices) # Number of treated units desired
  T0 <- t0 # The first treatment time
  
  ## Simultaneuous (simul_adapt) or Staggered adoption (stag_adapt)
  if(simul){
    treat_mat <- simul_adapt(Y, N_t, T0, treat_indices)
  }else{
    treat_mat <- stag_adapt(Y, N_t, T0, treat_indices)
  }
  
  Y_obs <- Y * treat_mat
  
  if(!is.null(covars)){
    ## ------
    ## MC-NNM-W
    ## ------
    
    est_model_MCPanel_w <- mcnnm_wc_cv(M=Y_obs, X = covars, Z=matrix(0L,0,0), mask=treat_mat,
                                       to_normalize = 1, to_estimate_u = 1, to_estimate_v = 1, to_add_ID = 1, 
                                       num_lam_L = 10, num_lam_H = 10, niter = 1000, rel_tol = 1e-05, cv_ratio = 0.8, 
                                       num_folds = 1,
                                       is_quiet = 1) 
    
    est_model_MCPanel_w$Mhat <- est_model_MCPanel_w$L + replicate(T,est_model_MCPanel_w$u) + t(replicate(N,est_model_MCPanel_w$v))
    if(imputed){
      est_model_MCPanel_w$impact <- (Y*Y.missing-est_model_MCPanel_w$Mhat)
    } else{
      est_model_MCPanel_w$impact <- (Y-est_model_MCPanel_w$Mhat)
    }
    
    return(est_model_MCPanel_w$impact)
  } 
  
  if(pca){
    
    treat_mat_NA <- treat_mat
    treat_mat_NA[treat_mat==0] <- NA
    
    ## ------
    ## PCA
    ## ------
    
    nb <- estim_ncpPCA(data.frame(Y_obs*treat_mat_NA),ncp.max=5) # cv num components
    
    PCA_Mhat <- imputePCA(data.frame(Y_obs*treat_mat_NA), nb$ncp)$completeObs # regularized iterative PCA
    if(imputed){
      PCA_impact <- (Y*Y.missing-PCA_Mhat) 
    } else{
      PCA_impact <- (Y-PCA_Mhat) 
    }
    
    return(PCA_impact)
    
  } else{
    ## ------
    ## MC-NNM
    ## ------
    
    est_model_MCPanel <- mcnnm_cv(Y_obs, treat_mat, to_estimate_u = 1, to_estimate_v = 1, 
                                  num_lam_L = 100, niter = 1000, rel_tol = 1e-05, cv_ratio = 0.8, num_folds = 5, is_quiet = 1)
    est_model_MCPanel$Mhat <- est_model_MCPanel$L + replicate(T,est_model_MCPanel$u) + t(replicate(N,est_model_MCPanel$v))
    if(imputed){
      est_model_MCPanel$impact <- (Y*Y.missing-est_model_MCPanel$Mhat)
    } else{
      est_model_MCPanel$impact <- (Y-est_model_MCPanel$Mhat)
    }
    
    return(est_model_MCPanel$impact)
  }
}