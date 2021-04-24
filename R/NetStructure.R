# NetStructure
NetStructure <- function(X, label){
  n <- nrow(X)
  p <- ncol(X)
  k <- length(levels(label))
  return(list(n = n, p = p, k = k))
}
