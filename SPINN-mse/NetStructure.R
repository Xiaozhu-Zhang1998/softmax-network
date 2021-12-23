# NetStructure
NetStructure <- function(X, label){
  n <- nrow(X)
  p <- ncol(X)
  return(list(n = n, p = p))
}
