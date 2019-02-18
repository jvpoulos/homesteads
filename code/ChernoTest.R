## Chernozhukov method
## https://arxiv.org/abs/1712.09089
## Remark 3: We note that if the estimator used in approximating P t N is invariant to permutations of the data {Z_t } across the time series dimension 
##, permuting {û_t } is equivalent to permuting {Z_t }.
## modified from https://github.com/ebenmichael/ents

ChernoTest <- function(outcomes, ns=1000, q=1, t.stat=NULL, treated.indices,
                        permtype=c("iid", "moving.block", "iid.block"),t0,sim=FALSE,covars=NULL,pca=FALSE) {
  
  t_final <- ncol(outcomes$M) # all periods
  t_star <- t_final-t0
  
  if(permtype == "iid") {
    teststats <- matrix(NA, nrow=ns, ncol=length(q))
    
    for(i in 1:ns) {
      
      ## sample from permutation distribution
      reorder <- sample(1:t_final, t_final)
      
      ## fit MC with reordered time periods
      
      new_mc_data <- outcomes
      
      new_mc_data <- sapply(names(new_mc_data), function(x){
        new_mc_data[[x]] <- new_mc_data[[x]][,reorder,drop=FALSE]
      }, simplify = FALSE,USE.NAMES = TRUE)
      
      mc.fit <-  MCEst(new_mc_data, t0)
      
      ## get treatment effect estimates
      
      att <- as.matrix(colMeans(mc.fit$impact[,t0:t_final,drop=FALSE][rownames(mc.fit$impact) %in% treated.indices,])) # get mean post-period impact on treated
      
      teststats[i,] <- ((1/sqrt(t_star)) * sum(abs(att)^q))^(1/q)   
    }
  } else if(permtype=="moving.block") {
    teststats <- matrix(NA, nrow=(t_final-1), ncol=length(q)) # exclude real order
    
    for(i in 1:(t_final-1)) {
      ## increment time by one step and wrap
      reorder <- (1:t_final -(i+1)) %% t_final + 1

      ## fit MC with reordered time periods
      
      new_mc_data <- outcomes
      
      new_mc_data <- sapply(names(new_mc_data), function(x){
        new_mc_data[[x]] <- new_mc_data[[x]][,reorder,drop=FALSE]
      }, simplify = FALSE,USE.NAMES = TRUE)
      
      mc.fit <-  MCEst(new_mc_data,t0)
      
      ## get treatment effect estimates
      
      att <- as.matrix(colMeans(mc.fit$impact[,t0:t_final,drop=FALSE][rownames(mc.fit$impact) %in% treated.indices,])) # get mean post-period impact on treated
      
      teststats[i,] <-((1/sqrt(t_star)) * sum(abs(att)^q))^(1/q)        
    }
  } else if(permtype=="iid.block") {
    
    source("PolitisWhite.R")
    m <- b.star(outcomes$M,round=TRUE)[[1]]  # get optimal bootstrap lengths
    teststats <- matrix(NA, nrow=ns, ncol=length(q))
    
    for(i in 1:ns) {
      ## permute by blocks
      blocks <-split(1:t_final, ceiling(seq_along(1:t_final)/m))
      reorder <-  unlist(sample(blocks),use.names = FALSE)
      
      if(identical(reorder,(1:t_final))){ # draw again if permuted order same as real order
        while(!identical(reorder,(1:t_final))){ 
          reorder <-  unlist(sample(blocks),use.names = FALSE)
        }
      }

      ## fit MC with reordered time periods
      
      new_mc_data <- outcomes
      
      new_mc_data <- sapply(names(new_mc_data), function(x){
        new_mc_data[[x]] <- new_mc_data[[x]][,reorder,drop=FALSE]
      }, simplify = FALSE,USE.NAMES = TRUE)
      
      mc.fit <-  MCEst(new_mc_data,t0)
      
      ## get treatment effect estimates
      
      att <- as.matrix(colMeans(mc.fit$impact[,t0:t_final,drop=FALSE][rownames(mc.fit$impact) %in% treated.indices,])) # get mean post-period impact on treated
      
      teststats[i,] <-((1/sqrt(t_star)) * sum(abs(att)^q))^(1/q)   
    }
  }else {
    stop("permtype must be one of c('iid', 'moving.block', 'iid.block')")
  }
  ## compute test stat for actual data
  
  if(!is.null(t.stat)){
    real_att <- t.stat
  } else{
    mc.fit.actual <-  MCEst(outcomes,t0,sim=FALSE,covars=NULL,pca=FALSE)
    real_att <- as.matrix(colMeans(mc.fit.actual$impact[,t0:t_final,drop=FALSE][rownames(mc.fit.actual$impact) %in% treated.indices,])) # get mean post-period impact on treated
  }
  real_teststat <- ((1/sqrt(t_star)) * sum(abs(real_att)^q))^(1/q)
  pval <- 1- ((1/length(teststats) * sum(teststats < real_teststat)))
  
  return(pval)
}

## Invert for CIs

ChernoCI <- function(t_star, sd=1, alpha=0.025, l=1000, prec=1e-02, outcomes, ns=100, q=1, treated.indices, 
                     permtype=c("iid", "moving.block", "iid.block"),t0,sim=FALSE,covars=NULL,pca=FALSE) {
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
  mc.est <- MCEst(outcomes, t0=t0,sim=FALSE, covars=NULL,pca=FALSE)
  pointwise <- mc.est$impact 
  
  real.att <- colMeans(pointwise[rownames(pointwise)%in%treated.indices,])
  c.range <- round(range(real.att),2)*2
  
  # Create vector to store CIs
  p.weights <- dnorm(seq(c.range[1],c.range[2],by=prec), mean(c.range), sd) # penalize larger effects
  CI <- matrix(NA, t_star, l)
  for(i in 1:l){
    # Sample sequence of treatment effects under the null
    delta.c <- sample(seq(c.range[1],c.range[2],by=prec),t_star,replace=FALSE, prob=p.weights)
    # Run permuation test
    results <- ChernoTest(outcomes, ns, q, t.stat=delta.c, treated.indices, permtype, t0, sim=FALSE, covars=NULL, pca=FALSE)
    # If result not significant, delta.c is in confidence interval
    if(results>(2*alpha)){
      CI[,i] <- delta.c
    }else{
      CI[,i] <- NA
    }
  }
  return(rowRanges(CI,na.rm=TRUE))
}