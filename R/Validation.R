# Cross-validation

# validation
validate <- function(X, label, fold, lambda, M){ 
  # number of folds
  nf <- length(fold)
  # RATE
  RATE <- c()
  # validate
  for(i in 1:nf){
    test.id <- fold[[i]]
    rs <- Network(X[-test.id,], label[-test.id], lambda, M)
    rate <- Accuracy(rs$g, label[-test.id])
    RATE <- c(RATE, rate)
  }
  return(sum(RATE)/nf)
}