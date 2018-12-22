en_predict <- function(M, mask, best_lam, best_alpha){
  treated_row <- which(rowMeans(mask) < 1)
  treated_cols <- which(mask[treated_row,]==0)
  control_cols <- setdiff(1:ncol(M),treated_cols)
  M_new <- M
  M_new[treated_row,] <- M[nrow(M),]
  M_new[nrow(M),] <- M[treated_row,]
  Z_train <- M_new[1:(nrow(M_new)-1),control_cols]
  to_pred <- M_new[nrow(M_new),control_cols]
  if(length(which(to_pred==0))==length(control_cols)){
    weights = matrix(0L, nrow(M_new)-1,1)
    intc = 0
  }
  else{
    A = glmnet(t(Z_train), M_new[nrow(M_new),control_cols],'gaussian', lambda=best_lam, alpha=best_alpha, thresh=1e-4, standardize=FALSE)
    weights <- unname(A$beta[,1])
    intc <- unname(A$a0[1])
  }
  M_pred = t(M_new[1:nrow(M_new)-1,]) %*% weights+intc*matrix(1L,ncol(M_new),1);
  return(M_pred)
}

#' Computing Elastic Net Estimator when multiple units are missing.
#' The underlying algorithm is glmnet package in R.
#' It is worth noting that this package was written by Friedman et. al.
#' 
#' @param M Matrix of observed entries. The input should be N (number of units) by T (number of time periods).
#' @param mask Binary mask with the same shape as M containing observed entries.
#' @param num_lam Optional parameter on the number of lambda values (weight penalties). Default is 100.
#' @return The matrix with all missing entries filled.
#' @seealso \code{\link[glmnet]{cv.glmnet}}, written by Jerome Friedman, Trevor Hastie, Noah Simon, Junyang Qian, and Rob Tibshirani
#' @examples
#' en_mp_rows(matrix(c(1,2,3,4),2,2), matrix(c(1,1,1,0),2,2))

en_mp_rows <- function(M, mask, num_lam = 100L){
  M <- M * mask
  treated_rows <- which(rowMeans(mask) < 1)
  control_rows <- setdiff(1:nrow(M), treated_rows)
  num_treated <- length(treated_rows)
  num_control <- length(control_rows)
  M_control_rows <- M[control_rows,]
  M_pred <- M
  for (l in 1:num_treated){
    mask_fake <- matrix(1L,num_control+1,ncol(mask))
    tr_row_pred <- treated_rows[l]
    tr_row_miss <- which(mask[treated_rows[l],]==0)
    M_fake <- rbind(M_control_rows, M[tr_row_pred,])
    mask_fake[nrow(mask_fake),tr_row_miss] = 0
    M_pred_this_row <- en_single_row( M_fake, mask_fake)
    M_pred[treated_rows[l],] <- M_pred_this_row
  }
  return(M_pred)
}

en_single_row <- function(M, mask){

  M <- M * mask
  treated_row <- which(rowMeans(mask) < 1)
  treated_cols <- which(mask[treated_row,] ==0 )
  control_rows = setdiff(nrow(M),treated_row)
  control_cols = setdiff(1:ncol(M),treated_cols)
  num_controls <- length(control_cols)

  M_new <- M
  M_new[treated_row,] <- M[nrow(M),]
  M_new[nrow(M),] <- M[treated_row,]

  Z_train <- M_new[1:(nrow(M_new)-1),control_cols]
  A <- cv.glmnet(t(Z_train), M_new[nrow(M_new),control_cols], family = 'gaussian', alpha = 1, standardize=FALSE, 
                 thresh=1e-4, nfolds=5, lambda=exp(seq(log(0.001), log(5), length.out=100))) # 5-fold cv on lambda
  best_lam <- A$lambda.min
  best_alpha <-1
  
  return(en_predict(M, mask , best_lam, best_alpha))
}


