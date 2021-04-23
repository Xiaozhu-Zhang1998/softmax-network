# Accuracy

Accuracy <- function(g, label){
  k <- length(g) + 1
  n <- length(g[[1]])
  pred <- c()

  for(i in 1:n){
    prob <- sapply(1:(k-1), function(j){ g[[j]][i] })
    prob <- c(prob, 0)
    prob <- exp(prob)/sum(exp(prob))
    ind <- which(max(prob) == prob)[1]
    pred <- c(pred, ind)
  }
  tab <- table(pred, label)
  rate <- sum(diag(tab)) / sum(tab)
  return(rate)
}