## Functions for estimation of ATEs,  CIs

EstAte <- function(df, y){ 
  # Calculate difference-in-means.
  #
  # Args:
  #   df: dataframe containing means of response variable 'y' by treatment status factor 'cat' ('Control' or 'Treated'), and binary 'time' indicating start of treatment
  #   y: string denoting response variable of interest
  #
  # Returns:
  #   ITT difference-in-means
  diff <- (df[df$time==1 & df$cat=='Treated',][y] - df[df$time==1 & df$cat=='Control',][y]) - 
    (df[df$time==0 & df$cat=='Treated',][y] - df[df$time==0 & df$cat=='Control',][y])
  return(diff[[1]])
} 

BootDiff<- function(df,y,R=10000,sc=1) {
  require(resample)
  # Function to compute 95% confidence interval for difference-in-means
  #
  # Args:
  #   df: dataframe containing means of response variable 'y' by treatment status factor 'cat' ('Control' or 'Treated'), and binary 'time' indicating start of treatment
  #   y: string denoting response variable of interest
  #   R: The number of bootstrap replicates. The default is 10,000. 
  #   sc. Smoothing constant. Default is 1. 
  #
  # Returns:
  #   Vector containing difference-in-means, and 95% nonparametric CI.
  # Bootstrap weighted means for response y for each treatment group 
  itt.diff <- bootstrap(df[y], EstAte, R=R, args.stat = list(df,y)) #ITT
  # Smooth bootstrap replicates by adding random normal variate independently to each observation
  itt.diff[[2]] <- sapply(1:R, function(x) {
    itt.diff[[2]][x] + rnorm(1,0,sc*sd(itt.diff[[2]])/sqrt(length(y)))
  })
  # Calculate percentiles of the differences in bootstrapped means
  itt.per <- quantile(itt.diff[[2]], probs = c(0.025, 0.975))
  # Calculate observed differences in means
  meandif.itt <- EstAte(df,y) 
  # Make table for estimates
  res.itt <- c(meandif.itt, itt.per) 
  colnames(res.itt) <- c('Mean Difference','.025','.975') 
  return(res.itt)
}

