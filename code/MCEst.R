MCEst <- function(outcomes,t0,imputed=TRUE,sim=FALSE,covars=NULL,pca=FALSE) {
  
  Y <- outcomes$M # NxT 
  Y.missing <- outcomes$M.missing # NxT 

  if(!is.null(covars)){
    Z <- rbind(covars$Z,"AK"=rep(0,ncol(covars$Z))) # NxT # missing AK
    Z <- Z[row.names(Y),]  # reorder
  }
  
  treat <- outcomes$mask # NxT masked matrix 
  
  N <- nrow(treat)
  T <- ncol(treat)
  
  ## Treated indices
  indices <- cbind("id"=1:nrow(Y), "name"=rownames(Y))
  treat_indices_order <- c("CA", "CO", "IA", "KS", "MI", "MN", "MO", "NE", "OH", "OR", "SD", "WA", "WI", "IL", "NV", "ID", "MT", "ND",  "UT", "AL", "MS", "AR", "FL", "LA", "IN", "NM", "WY", "AZ", "OK", "AK")
  treat_indices <- as.numeric(indices[order(match(indices[,2], treat_indices_order))][1:length(treat_indices_order)]) # sort indices increasingly based on T0
  
  N_t <- length(treat_indices) # Number of treated units desired
  T0 <- t0 #which(colnames(Y)=="1869")# The first treatment time
  
  ## Simultaneuous (simul_adapt) or Staggered adoption (stag_adapt)
  if(sim){
    treat_mat <- simul_adapt(Y, N_t, T0, treat_indices)
  }else{
    treat_mat <- stag_adapt(Y, N_t, T0, treat_indices)
  }
  
  Y_obs <- Y * treat_mat
  
  if(!is.null(covars)){
    ## ------
    ## MC-NNM-W
    ## ------
    
    est_model_MCPanel_w <- mcnnm_wc_fit(M=Y_obs, X = Z, Z=matrix(0L,0,0), mask=treat_mat, lambda_L=1, to_estimate_u = 1, to_estimate_v = 1)
    est_model_MCPanel_w$Mhat <- est_model_MCPanel_w$L + replicate(T,est_model_MCPanel_w$u) + t(replicate(N,est_model_MCPanel_w$v))
    if(imputed){
      est_model_MCPanel_w$impact <- (Y*Y.missing-est_model_MCPanel_w$Mhat)
    } else{
      est_model_MCPanel_w$impact <- (Y-est_model_MCPanel_w$Mhat)
    }
    
    return(list("impact" = est_model_MCPanel_w$impact, "Mhat" = est_model_MCPanel_w$Mhat))
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
    
    return(list("impact" = PCA_impact, "Mhat" = PCA_Mhat))
    
  } else{
    ## ------
    ## MC-NNM
    ## ------
    
    est_model_MCPanel <- mcnnm_fit(Y_obs, treat_mat, lambda_L=1, to_estimate_u = 1, to_estimate_v = 1) 
    est_model_MCPanel$Mhat <- est_model_MCPanel$L + replicate(T,est_model_MCPanel$u) + t(replicate(N,est_model_MCPanel$v))
    if(imputed){
      est_model_MCPanel$impact <- (Y*Y.missing-est_model_MCPanel$Mhat)
    } else{
      est_model_MCPanel$impact <- (Y-est_model_MCPanel$Mhat)
    }

    return(list("impact" = est_model_MCPanel$impact, "Mhat" = est_model_MCPanel$Mhat))
  }
}