# Accuracy

Accuracy <- function(g, label){
  k <- nrow(g)
  n <- ncol(g)
  pred <- c()

  for(i in 1:n){
    prob <- g[,i]
    prob <- exp(prob) / sum(exp(prob))
    ind <- which(max(prob) == prob)[1]
    pred <- c(pred, ind)
  }
  tab <- table(pred, label)
  rate <- sum(diag(tab)) / sum(tab)
  return(rate)
}