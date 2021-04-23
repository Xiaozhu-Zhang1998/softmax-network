# NetStructure
NetStructure <- function(X, label){
  n <- nrow(X)
  p <- ncol(X)
  k <- length(levels(label))
  t <- 40
  return(list(n = n, p = p, k = k, t = t))
}
