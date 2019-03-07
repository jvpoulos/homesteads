## Chernozhukov method
## https://arxiv.org/abs/1712.09089
## Remark 3: We note that if the estimator used in approximating P t N is invariant to permutations of the data {Z_t } across the time series dimension 
##, permuting {û_t } is equivalent to permuting {Z_t }.
## modified from https://github.com/ebenmichael/ents

ChernoTest <- function(outcomes, ns=1000, q=c(1,2), t.stat=NULL, treat_indices_order,permtype=c("iid", "moving.block", "iid.block"),t0,imputed=FALSE,covars=NULL,pca=FALSE) {
  
  t_final <- ncol(outcomes$M) # all periods
  
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
      
      mc.fit <-  MCEst(new_mc_data)
      
      ## get treatment effect estimates
      
      att <- as.matrix(colMeans(mc.fit$impact[,t0:t_final,drop=FALSE][rownames(mc.fit$impact) %in% treat_indices_order,], na.rm = TRUE)) # get mean post-period impact on treated
      if(imputed){
        att <- na.omit(att)
      }
      
      teststats[i,] <- sapply(q,
                               function(j) ((1/sqrt(length(att))) * sum(abs(att)^q[j]))^(1/q[j]) )
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
      
      mc.fit <-  MCEst(new_mc_data)
      
      ## get treatment effect estimates
      
      att <- as.matrix(colMeans(mc.fit$impact[,t0:t_final,drop=FALSE][rownames(mc.fit$impact) %in% treat_indices_order,], na.rm = TRUE)) # get mean post-period impact on treated
      if(imputed){
        att <- na.omit(att)
      }
      
      teststats[i,] <- sapply(q,
                              function(j) ((1/sqrt(length(att))) * sum(abs(att)^q[j]))^(1/q[j]) )      
    }
  } else if(permtype=="iid.block") {
    
    source("PolitisWhite.R")
    m <- median(b.star(t(outcomes$M),round=TRUE)[,1])  # get optimal bootstrap lengths
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
      
      mc.fit <-  MCEst(new_mc_data)
      
      ## get treatment effect estimates
      
      att <- as.matrix(colMeans(mc.fit$impact[,t0:t_final,drop=FALSE][rownames(mc.fit$impact) %in% treat_indices_order,], na.rm = TRUE)) # get mean post-period impact on treated
      if(imputed){
        att <- na.omit(att)
      }
    
      teststats[i,] <- sapply(q,
                              function(j) ((1/sqrt(length(att))) * sum(abs(att)^q[j]))^(1/q[j]) )   
    }
  }else {
    stop("permtype must be one of c('iid', 'moving.block', 'iid.block')")
  }
  ## compute test stat for actual data
  
  if(!is.null(t.stat)){
    real_att <- t.stat
  } else{
    mc.fit.actual <-  MCEst(outcomes,imputed=FALSE,covars=NULL,pca=FALSE)
    real_att <- as.matrix(colMeans(mc.fit.actual$impact[,t0:t_final,drop=FALSE][rownames(mc.fit.actual$impact) %in% treat_indices_order,], na.rm = TRUE)) # get mean post-period impact on treated
    if(imputed){
      real_att <- na.omit(real_att)
    }
  }
  real_teststat <- sapply(q,
                           function(j) ((1/sqrt(length(real_att))) * sum(abs(real_att)^q[j]))^(1/q[j]))
  pval <- sapply(q, function(i) 1- ((1/length(teststats[,i]) * sum(teststats[,i] < real_teststat[i]))))
  return(list("q"=q,"s"=real_teststat,"teststats"=teststats,"real_att" = real_att[,1],"p"=pval))
}