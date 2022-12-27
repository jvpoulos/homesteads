#####################################################
# IFE estimator with missing data                   #
# https://duane321.github.io/emfactor-site/#Demotoy #
#####################################################

IFE <- function(M, mask, k=2, retrospective=TRUE){
  M_NA <- t(M) # T x N
  M_NA[t(mask)==0] <- NA
  
  if(retrospective){
    out_idx <- 1:max(rowSums(1-mask)) # first initial treatment time
  }else{
    out_idx <- max(rowSums(1-mask)):ncol(mask) # first initial treatment time
  }

  units_observed <- which(rowSums(mask) == T) # AT
  units_to_predict <- which(rowSums(mask) < T) # ST

  est_model_IFE <- emfactor(M_NA, k=k)
  g <- solve(diag(rep(1, k)) + t(est_model_IFE$w) %*% diag(1/est_model_IFE$psi) %*% est_model_IFE$w)
  
  to_predict <- M_NA[out_idx, units_to_predict] # no staggered adoption
  predictions <- to_predict
  predictions[] <- NA

  #Form predictions
  for(i in seq(length(out_idx))){
    obs_logi <- !is.na(M_NA[out_idx[i], units_observed]) 
    units_observed_at_i <- units_observed[obs_logi]
    zi <- g %*% t(est_model_IFE$w[units_observed_at_i,]) %*% diag(1/est_model_IFE$psi[units_observed_at_i]) %*%
      (M_NA[out_idx[i],units_observed_at_i] - est_model_IFE$mu[units_observed_at_i])
    predictions[i,] <- est_model_IFE$w[units_to_predict,] %*% zi + est_model_IFE$mu[units_to_predict]
  }

  reconstructed_mat <- M # NxT
  reconstructed_mat[,out_idx][units_to_predict,] <- t(predictions) * (1-mask[,out_idx][units_to_predict,]) +  reconstructed_mat[,out_idx][units_to_predict,]*mask[,out_idx][units_to_predict,] # impute only missing

  return(reconstructed_mat)
}