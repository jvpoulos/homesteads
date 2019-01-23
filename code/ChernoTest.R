## Chernozhukov method
## https://arxiv.org/abs/1712.09089
## Remark 3: We note that if the estimator used in approximating P t N is invariant to permutations of the data {Z_t } across the time series dimension 
##, permuting {û_t } is equivalent to permuting {Z_t }.
## modified from https://github.com/ebenmichael/ents

ChernoTest <- function(dataprep.out, ns=1000, q=1, t.stat=NULL,
                        permtype=c("iid", "moving.block")) {

  ## format data for synth
  syn_data <- dataprep.out
  
  ## pre and post outcomes
  
  trtmat <- syn_data$Y0plot # T X N matrix of outcome data for control units 
  ctrlmat <- syn_data$Y1plot # T X N matrix of outcome data for treated units 
  
  t0 <- nrow(syn_data$Z0) # n pre-treatment periods
  t_final <- nrow(syn_data$Y0plot) # all periods
  t_star <- t_final-t0
  
  if(permtype == "iid") {
    teststats <- matrix(0, nrow=ns, ncol=length(q))
    
    for(i in 1:ns) {
      
      ## sample from permutation distribution
      reorder <- sample(1:t_final, t_final)
      
      ## fit synth with reordered time periods
      
      new_synth_data <- syn_data
      
      new_synth_data$X0 <- syn_data$Y0plot[reorder,,drop=FALSE][1:t0,]
      new_synth_data$X1 <- matrix(syn_data$Y1plot[reorder,,drop=FALSE][1:t0,])
      
      #syn <- fit_synth_formatted(data_out=new_synth_data)
      syn <-  synth(
        data.prep.obj = new_synth_data,
        Margin.ipop=.005,Sigf.ipop=7,Bound.ipop=6, verbose=FALSE
      )
      
      ## get treatment effect estimates
      
      att <- syn_data$Y1plot[reorder,,drop=FALSE][(t0+1):t_final,,drop=FALSE] -
        syn_data$Y0plot[reorder,,drop=FALSE][(t0+1):t_final,,drop=FALSE] %*% syn$solution.w
      
      teststats[i,] <- ((1/sqrt(t_star)) * sum(abs(att)^q))^(1/q)   
    }
  } else if(permtype=="moving.block") {
    teststats <- matrix(0, nrow=t_final, ncol=length(q))
    
    for(i in 1:t_final) {
      ## increment time by one step and wrap
      reorder <- (1:t_final -(i+1)) %% t_final + 1
      ## fit synth with reordered time periods
      new_synth_data <- syn_data
      
      new_synth_data$X0 <- syn_data$Y0plot[reorder,,drop=FALSE][1:t0,]
      new_synth_data$X1 <- matrix(syn_data$Y1plot[reorder,,drop=FALSE][1:t0,])
      
      #syn <- fit_synth_formatted(new_synth_data)
      syn <-  synth(
        data.prep.obj = new_synth_data,
        Margin.ipop=.005,Sigf.ipop=7,Bound.ipop=6, verbose=FALSE
      )
      
      ## get treatment effect estimates
      
      att <- syn_data$Y1plot[reorder,,drop=FALSE][(t0+1):t_final,,drop=FALSE] -
        syn_data$Y0plot[reorder,,drop=FALSE][(t0+1):t_final,,drop=FALSE] %*% syn$solution.w
      
      teststats[i,] <- ((1/sqrt(t_star)) * sum(abs(att)^q))^(1/q)          
    }
  } else {
    stop("permtype must be one of c('iid', 'block')")
  }
  ## compute test stat for actual data
 # syn <- fit_synth_formatted(syn_data)
  syn <-  synth(
    data.prep.obj = syn_data,
    Margin.ipop=.005,Sigf.ipop=7,Bound.ipop=6
  )
  if(!is.null(t.stat)){
    real_att <- t.stat
  } else{
    real_att <-  syn_data$Y1plot[(t0+1):t_final,,drop=FALSE] -
    syn_data$Y0plot[(t0+1):t_final,,drop=FALSE] %*% syn$solution.w
  }
  
  real_teststat <- ((1/sqrt(t_star)) * sum(abs(real_att)^q))^(1/q)
  
  pvals <- 1- ((1/length(teststats) * sum(teststats < real_teststat)))
  
  return(pvals)
}

## Invert for CIs

ChernoCI <- function(t_star,c.range=c(-2,2), alpha=0.025, l=10, d=1e-05, dataprep.out, ns=1000, q=1, permtype="iid") {
  require(matrixStats)
  # Calculate randomization test confidence interval.
  #
  # Args:
  #   c.range: Range of constant treatment effects. Default is c(-1,1).
  #   alpha: Two-sided significance level. Default is 0.025.
  #   l: Number of constant treatment effects. Default is 100.
  #   d: Level of precision in constant treatment effects. Default is 1e-05.
  #
  # Returns:
  #   Vector of per-time-step randomization confidence interval
  # Create vector to store CIs
  p.weights <- abs(log(abs(seq(c.range[1],c.range[2],by=d) + .Machine$double.eps))) # penalize larger effects
  CI <- matrix(NA, t_star, l)
  for(i in 1:l){
    # Sample sequence of treatment effects under the null
    delta.c <- sample(seq(c.range[1],c.range[2],by=d),t_star,replace=FALSE, prob=p.weights)
    # Run permuation test
    results <- ChernoTest(dataprep.out, ns, q, t.stat=delta.c, permtype)
    # If result not significant, delta.c is in confidence interval
    if(results>(2*alpha)){
      CI[,i] <- delta.c
    }else{
      CI[,i] <- NAF
    }
  }
  return(rowRanges(CI,na.rm=TRUE))
}

## Ex.
library(Synth)
library(ents)
data("basque")

ChernoTest(dataprep.out, ns=10, permtype="moving.block") # dataprep.out from synth-basque.R

ChernoCI(t_star, l=10, dataprep.out, ns=10, q=1, permtype="iid")

source(paste0(code.directory,"PolitisWhite.R"))
#bopt.rev.pc <- b.star(dfList$rev.pc$M,round=TRUE)[[1]]  # get optimal bootstrap lengths

