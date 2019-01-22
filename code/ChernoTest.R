## Chernozhukov method
## https://github.com/ebenmichael/ents

ChernoTest <- function(dataprep.out, ns=1000,  q=c(2,1), 
                        permtype=c("iid", "block")) {
  
  
  ## format data for synth
  syn_data <- dataprep.out
  
  ## pre and post outcomes
  
  trtmat <- syn_data$Y0plot # T X N matrix of outcome data for control units 
  ctrlmat <- syn_data$Y1plot # T X N matrix of outcome data for treated units 
  
  t0 <- nrow(syn_data$Z0) # n pre-treatment periods
  t_final <- nrow(syn_data$Y0plot) # all periods
  
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
      
      teststats[i,] <- sapply(1:length(q),
                              function(j) mean(abs(att)^q[j])^(1/q[j]))
    }
  } else if(permtype=="block") {
    teststats <- matrix(0, nrow=t_final, ncol=length(q))
    
    for(i in 1:t_final) {
      ## increment time by one step and wrap
      reorder <- (1:t_final + i) %% t_final + 1
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
      
      teststats[i,] <- sapply(1:length(q),
                              function(j) mean(abs(att)^q[j])^(1/q[j]))            
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
  
  real_att <-  syn_data$Y1plot[(t0+1):t_final,,drop=FALSE] -
    syn_data$Y0plot[(t0+1):t_final,,drop=FALSE] %*% syn$solution.w
  
  real_teststat <- sapply(1:length(q),
                          function(j) mean(abs(real_att)^q[j])^(1/q[j]))
  
  pvals <- sapply(1:length(q), function(i) mean(teststats[,i] >= real_teststat[i]))
  
  return(pvals)
}

## Ex.
library(Synth)
library(ents)
data("basque")

ChernoTest(dataprep.out, ns=10, q=1, permtype="iid") # dataprep.out from synth-basque.R
