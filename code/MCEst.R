MCEst <- function(outcomes, covars.x, t0, ST, estimator, estimand=NULL, tseries=NULL) {
  # tseries and estimand supplied if estimating time-series bootstraps
  if(!is.null(tseries)){
    Y <- t(tseries)
    
    Y_obs <- Y * outcomes$mask
    
    # combine data back into list
    outcomes[["Y"]] <- Y
    outcomes[["Y_obs"]] <- Y_obs
  }
  ## Estimate propensity scores
  
  p.weights <- NULL
  if(estimator%in%c("mc_weights")){
    p.mod <- cv.glmnet(x=cbind(covars.x,outcomes$Y_obs[,1:(t0-1)]), y=(1-outcomes$mask), family="mgaussian", alpha=1,nfolds=10,intercept=FALSE)
    W <- predict(p.mod, cbind(covars.x,outcomes$Y_obs[,1:(t0-1)]))[,,1]
    
    p.weights <- matrix(NA, nrow=nrow(outcomes$mask), ncol=ncol(outcomes$mask), dimnames = list(rownames(outcomes$mask), colnames(outcomes$mask)))
    p.weights <- boundProbs(W)/(1-boundProbs(W))
  }
  
  rankL <- NULL
  
  ## ------ ------ ------ ------ ------
  ## MC-NNM plain (no weighting, no covariate)
  ## ------ ------ ------ ------ ------
  
  if(estimator=="mc_plain"){
    est_mc_plain <- mcnnm_cv(M = outcomes$Y_obs, mask = outcomes$mask, W = matrix(1, nrow(outcomes$mask),ncol(outcomes$mask)), to_estimate_u = 1, to_estimate_v = 1, rel_tol = 1e-05, is_quiet = 1)
    
    rankL <- rankMatrix(t(est_mc_plain$L), method="qr.R")[1]
    
    Mhat <- est_mc_plain$L + replicate(dim(outcomes$Y_obs)[2],est_mc_plain$u) + t(replicate(dim(outcomes$Y_obs)[1],est_mc_plain$v))
    tau <- (outcomes$Y-Mhat) # estimated treatment effect, Y(ST) - Y(NT)
    
    att <- apply(tau*(1-outcomes$mask),1,nzmean)[ST]
    att.bar <- mean(att)
  }
  
  ## ------ ------ ------ ------ ------
  ## MC-NNM weights (no covariate)
  ## ------ ------ ------ ------ ------
  
  if(estimator=="mc_weights"){
    est_mc_weights <- mcnnm_cv(M = outcomes$Y_obs, mask = outcomes$mask, W = p.weights, to_estimate_u = 1, to_estimate_v = 1, rel_tol = 1e-05, is_quiet = 1)
    
    rankL <- rankMatrix(t(est_mc_weights$L), method="qr.R")[1]
    
    Mhat <- est_mc_weights$L + replicate(dim(outcomes$Y_obs)[2],est_mc_weights$u) + t(replicate(dim(outcomes$Y_obs)[1],est_mc_weights$v))
    tau <- (outcomes$Y-Mhat) # estimated treatment effect, Y(ST) - Y(NT)
    
    att <- apply(tau*(1-outcomes$mask),1,nzmean)[ST]
    att.bar <- mean(att)
  }
  
  ## -----
  ## ADH
  ## -----
  
  if(estimator=="ADH"){
    Mhat <- adh_mp_rows(outcomes$Y_obs, outcomes$mask)
    tau <- (outcomes$Y-Mhat) # estimated treatment effect

    att <- apply(tau*(1-outcomes$mask),1,nzmean)[ST]
    att.bar <- mean(att)
  }
  
  ## -----
  ## VT-EN : It does Not cross validate on alpha (only on lambda) and keep alpha = 1 (LASSO).
  ## -----
  
  if(estimator=="ENT"){
    Mhat <- try(t(en_mp_rows(t(outcomes$Y_obs), t(outcomes$mask), num_alpha = 1)))
    if(inherits(Mhat, "try-error")){
      print("use instead n-fold CV (LOOCV)")
      Mhat <- try(t(en_mp_rows(t(outcomes$Y_obs), t(outcomes$mask), num_alpha = 1, num_folds = nrow(t(outcomes$Y_obs)) )))
    } 
    tau <- (outcomes$Y-Mhat) # estimated treatment effect
    
    att <- apply(tau*(1-outcomes$mask),1,nzmean)[ST]
    att.bar <- mean(att)
  }
  
  ## -----
  ## DID
  ## -----

  if(estimator=="DID"){
    Mhat <- DID(outcomes$Y_obs, outcomes$mask)
    tau <- (outcomes$Y-Mhat) # estimated treatment effect
    
    att <- apply(tau*(1-outcomes$mask),1,nzmean)[ST]
    att.bar <- mean(att)
  }
  
  ## ---------------
  ## IFEs
  ## ---------------
  
  if(estimator=="IFE"){
    Mhat <- IFE(outcomes$Y_obs, outcomes$mask, k=2)
    tau <- (outcomes$Y-Mhat) # estimated treatment effect
    
    att <- apply(tau*(1-outcomes$mask),1,nzmean)[ST]
    att.bar <- mean(att)
  }
  
  if(!is.null(tseries) & !is.null(estimand)){
    if(estimand=="tau"){
      return(tau)
    }else if(estimand=="att"){
      return(att)
    }else if(estimand=="att.bar"){
      return(att.bar)
    }
  }else{
    return(list("Mhat"=Mhat,"tau"=tau,"att"=att,"att.bar"=att.bar,"estimator"=estimator,"rankL"=rankL,"p.weights"=p.weights))
  }
}