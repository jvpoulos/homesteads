boundProbs <- function(x,bounds=c(0.01,0.99)){
  x[x>max(bounds)] <- max(bounds)
  x[x<min(bounds)] <- min(bounds)
  return(x)
}