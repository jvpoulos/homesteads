boundProbs <- function(x,bounds=c(0.001,0.999)){
  x[x>max(bounds)] <- max(bounds)
  x[x<min(bounds)] <- min(bounds)
  return(x)
}