## Chernozhukov method
## https://arxiv.org/abs/1712.09089
## Remark 3: We note that if the estimator used in approximating P t N is invariant to permutations of the data {Z_t } across the time series dimension 
##, permuting {û_t } is equivalent to permuting {Z_t }.
## modified from https://github.com/ebenmichael/ents

ChernoTest <- function(outcomes, ns=1000, q=1, t.stat=NULL, m=NULL, treated.indices,
                        permtype=c("iid", "moving.block", "iid.block")) {
  
  t0 <- which(colnames(outcomes$M)=="1869")-1 # n pre-treatment periods
  t_final <- ncol(outcomes$M) # all periods
  t_star <- t_final-t0
  
  if(permtype == "iid") {
    teststats <- matrix(0, nrow=ns, ncol=length(q))
    
    for(i in 1:ns) {
      
      ## sample from permutation distribution
      reorder <- sample(1:t_final, t_final)
      
      ## fit MC with reordered time periods
      
      new_mc_data <- outcomes
      
      new_mc_data <- sapply(names(new_mc_data), function(x){
        new_mc_data[[x]] <- new_mc_data[[x]][,reorder,drop=FALSE]
      }, simplify = FALSE,USE.NAMES = TRUE)
      
      mc.fit <-  MCEst(new_mc_data)
      
      ## get treatment effect estimates
      
      att <- as.matrix(colMeans(mc.fit$impact[,(t0+1):t_final,drop=FALSE][rownames(mc.fit$impact) %in% treated.indices,], na.rm=TRUE)) # get mean post-period impact on treated
      
      teststats[i,] <- ((1/sqrt(t_star)) * sum(abs(att)^q,na.rm=TRUE))^(1/q)   
    }
  } else if(permtype=="moving.block") {
    teststats <- matrix(0, nrow=t_final, ncol=length(q))
    
    for(i in 1:t_final) {
      ## increment time by one step and wrap
      reorder <- (1:t_final -(i+1)) %% t_final + 1
      
      ## fit MC with reordered time periods
      
      new_mc_data <- outcomes
      
      new_mc_data <- sapply(names(new_mc_data), function(x){
        new_mc_data[[x]] <- new_mc_data[[x]][,reorder,drop=FALSE]
      }, simplify = FALSE,USE.NAMES = TRUE)
      
      mc.fit <-  MCEst(new_mc_data)
      
      ## get treatment effect estimates
      
      att <- as.matrix(colMeans(mc.fit$impact[,(t0+1):t_final,drop=FALSE][rownames(mc.fit$impact) %in% treated.indices,], na.rm=TRUE)) # get mean post-period impact on treated
      
      teststats[i,] <-((1/sqrt(t_star)) * sum(abs(att)^q,na.rm=TRUE))^(1/q)        
    }
  } else if(permtype=="iid.block") {
    
    if(is.null(m)){
 #     source("PolitisWhite.R")
      m <- b.star(outcomes$M,round=TRUE)[[1]]  # get optimal bootstrap lengths
    }

    teststats <- matrix(0, nrow=t_final, ncol=length(q))
    
    for(i in 1:t_final) {
      ## permute by blocks
      blocks <-split(1:t_final, ceiling(seq_along(1:t_final)/m))
      reorder <-  unlist(sample(blocks),use.names = FALSE)
      
      ## fit MC with reordered time periods
      
      new_mc_data <- outcomes
      
      new_mc_data <- sapply(names(new_mc_data), function(x){
        new_mc_data[[x]] <- new_mc_data[[x]][,reorder,drop=FALSE]
      }, simplify = FALSE,USE.NAMES = TRUE)
      
      mc.fit <-  MCEst(new_mc_data)
      
      ## get treatment effect estimates
      
      att <- as.matrix(colMeans(mc.fit$impact[,(t0+1):t_final,drop=FALSE][rownames(mc.fit$impact) %in% treated.indices,], na.rm=TRUE)) # get mean post-period impact on treated
      
      teststats[i,] <-((1/sqrt(t_star)) * sum(abs(att)^q,na.rm=TRUE))^(1/q)          
    }
  }else {
    stop("permtype must be one of c('iid', 'moving.block', 'iid.block')")
  }
  ## compute test stat for actual data

  mc.fit.actual <-  MCEst(outcomes)
  
  if(!is.null(t.stat)){
    real_att <- t.stat
  } else{
    real_att <- as.matrix(colMeans(mc.fit.actual$impact[,(t0+1):t_final,drop=FALSE][rownames(mc.fit.actual$impact) %in% treated.indices,], na.rm=TRUE)) # get mean post-period impact on treated
  }
  
  real_teststat <- ((1/sqrt(t_star)) * sum(abs(real_att)^q,na.rm = TRUE))^(1/q)
  
  pvals <- 1- ((1/length(teststats) * sum(abs(teststats) < abs(real_teststat), na.rm=TRUE)))
  
  return(pvals)
}

## Invert for CIs

ChernoCI <- function(t_star,c.range=c(-2,2), alpha=0.025, l=10, prec=1e-05, outcomes, ns=1000, q=1, m=NULL, permtype="iid") {
  require(matrixStats)
  # Calculate randomization test confidence interval.
  #
  # Args:
  #   c.range: Range of constant treatment effects. Default is c(-1,1).
  #   alpha: Two-sided significance level. Default is 0.025.
  #   l: Number of constant treatment effects. Default is 100.
  #   prec: Level of precision in constant treatment effects. Default is 1e-05.
  #
  # Returns:
  #   Vector of per-time-step randomization confidence interval
  # Create vector to store CIs
  p.weights <- abs(log(abs(seq(c.range[1],c.range[2],by=prec) + .Machine$double.eps))) # penalize larger effects
  CI <- matrix(NA, t_star, l)
  for(i in 1:l){
    # Sample sequence of treatment effects under the null
    delta.c <- sample(seq(c.range[1],c.range[2],by=prec),t_star,replace=FALSE, prob=p.weights)
    # Run permuation test
    results <- ChernoTest(outcomes, ns, q, t.stat=delta.c, m, permtype)
    # If result not significant, delta.c is in confidence interval
    if(results>(2*alpha)){
      CI[,i] <- delta.c
    }else{
      CI[,i] <- NAF
    }
  }
  return(rowRanges(CI,na.rm=TRUE))
}