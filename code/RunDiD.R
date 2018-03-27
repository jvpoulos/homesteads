RunDiD <- function(data, indices, f1) {
  d <- data[indices,]
  # Helper function for DiD regression
  did.model <- lm(f1, d)
  return(coef(did.model)[['did']])
}