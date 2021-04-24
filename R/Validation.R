# Cross-validation

# validation
validate <- function(X, label, fold, lambda, M,
                     t, alpha0, batchsize){ 
  # number of folds
  nf <- length(fold)
  # RATE
  RATE <- c()
  # validate
  for(i in 1:nf){
    test.id <- fold[[i]]
    rs <- Network(X[-test.id,], label[-test.id], lambda, M,
                  t, alpha0, batchsize)
    rate <- Accuracy(rs$g, label[-test.id])
    RATE <- c(RATE, rate)
  }
  return(sum(RATE)/nf)
}