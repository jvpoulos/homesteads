##################################
# Utility fns.                  #
##################################

# function to bound probabilities to be used when making predictions
boundProbs <- function(x,bounds=c(0.001, 0.999)){
  x[x>max(bounds)] <- max(bounds)
  x[x<min(bounds)] <- min(bounds)
  return(x)
}

# mean exluding zero values (for calc. ATT)
nzmean <- function(x) {
  if (all(x==0)) 0 else mean(x[x!=0])
}

# confidence interval
CI_test <- function(est_coefficent, real_coefficent, est_var,alpha=0.05){
  return(as.numeric(est_coefficent - qnorm(1 - alpha/2)*sqrt(est_var) <= real_coefficent &
               est_coefficent + qnorm(1 - alpha/2)*sqrt(est_var) >= real_coefficent ))
}

boot_CI <- function(est_coefficent,est_var,alpha=0.05){
  return(list("lb"=est_coefficent - qnorm(1 - alpha/2)*sqrt(est_var),
               "ub"=est_coefficent + qnorm(1 - alpha/2)*sqrt(est_var) ))
}

# Helper function for continuous DID regression bootstrap
RunDiD <- function(data, indices, f1) {
  d <- data[indices,]
  did.model <- glm(f1, data=d)
  return(coef(did.model)[['did']])
}

# For inequality descriptive plot
LmEq <- function(m) {
  
  l <- list(a = format(coef(m)[1], digits = 2),
            b = format(abs(coef(m)[2]), digits = 2),
            p.val = format(summary(m)[[4]][2], digits = 1));
  
  if (coef(m)[2] >= 0)  {
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(p)~"="~p.val,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(p)~"="~p.val,l)    
  }
  
  as.character(as.expression(eq));                 
}

TLag <- function(x, n = 1L, time) { 
  index <- match(time - n, time, incomparables = NA)
  x[index]
}