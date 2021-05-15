# Cross-validation

# validation
validate <- function(X, label, fold, lambda, M,
                     t, alpha0, batchsize, drop.p, nepoch){ 
  # number of folds
  nf <- length(fold)
  # RATE
  RATE <- c()
  FEA <- c()
  # validate
  for(i in 1:nf){
    test.id <- fold[[i]]
    rs <- Network(X[-test.id,], label[-test.id], lambda, M,
                  t, alpha0, batchsize, drop.p, nepoch)
    g.test <- nn.predict(X[test.id,], rs$w1, rs$w2, rs$b, drop.p)
    rate <- Accuracy(g.test, label[test.id])
    RATE <- c(RATE, rate)
    fea <- sum(rs$b[[1]] == 0) + sum(rs$b[[2]] == 0) + sum(rs$b[[3]] == 0)
    FEA <- c(FEA, fea)
  }
  return(list(rate = sum(RATE)/nf, fea = sum(FEA)/nf))
}