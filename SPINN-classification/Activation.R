# Activation
af <- function(x){
  ifelse(x >= 0, x, 0.1 * x)
}

daf <- function(x){
  x[x >= 0] <- 1
  x[x < 0] <- 0.1
  return(x)
}