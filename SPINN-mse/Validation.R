# Cross-validation

# validation
validate <- function(X, y, fold, lambda0, lambda, a,
                     t, q, alpha0, batchsize, drop.p, nepoch){ 
  # number of folds
  nf <- length(fold)
  # RATE
  RATE <- c()
  for(i in 1:nf){
    test.id <- fold[[i]]
    rs <- Network(X[-test.id,], y[-test.id], lambda0, lambda, a,
                  t, q, alpha0, batchsize, drop.p, nepoch)
    f.test <- nn.predict(X[test.id,], rs$w1, rs$w2, rs$w3, drop.p)
    rate <- sum((f.test - y[test.id])**2) / length(test.id)
    RATE <- c(RATE, rate)
  }
  return(mean(RATE))
}