# ChernoTest return(pval)

## Invert for CIs
ChernoCI <- function(alpha=0.025, l=100, prec=1e-02, outcomes, ns=100, q=c(1,2), treat_indices_order,
                     permtype=c("iid", "moving.block", "iid.block"),t0,imputed=FALSE,covars=NULL,pca=FALSE) {
  require(matrixStats)
  # Calculate randomization test confidence interval.
  #
  # Args:
  #   alpha: Two-sided significance level. Default is 0.025.
  #   l: Number of constant treatment effects. Default is 1000.
  #
  # Returns:
  #   Vector of per-time-step randomization confidence interval
  # Get observed average treatment effects
  mc.est <- MCEst(outcomes,imputed=FALSE,covars=NULL,pca=FALSE)
  
  pointwise <- mc.est$impact 
  
  real.att <- colMeans(pointwise[rownames(pointwise)%in%treat_indices_order,])
  c.range <- round(range(real.att),2)
  
  t_final <- ncol(outcomes$M) # all periods
  t_star <- t_star <- length(t0:t_final)
  
  # Create vector to store CIs
  CI.q1 <- matrix(NA, t_star, l)
  CI.q2 <- matrix(NA, t_star, l)
  for(i in 1:l){
    # Sample sequence of treatment effects under the null
    delta.c <- sample(seq(c.range[1],c.range[2],by=prec),t_star,replace=FALSE)
    # Run permuation test
    results <- ChernoTest(outcomes, ns, q, t.stat=delta.c, treat_indices_order, permtype, t0, imputed=FALSE, covars=NULL, pca=FALSE)$p
    # If result not significant, delta.c is in confidence interval
    if(results[1]>(2*alpha)){
      CI.q1[,i] <- delta.c
    }else{
      CI.q1[,i] <- NA
    }
    if(results[2]>(2*alpha)){
      CI.q2[,i] <- delta.c
    }else{
      CI.q2[,i] <- NA
    }
  }
  return(list("q1"=CI.q1,"q2"=CI.q2))
}