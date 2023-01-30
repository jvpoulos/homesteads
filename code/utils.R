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