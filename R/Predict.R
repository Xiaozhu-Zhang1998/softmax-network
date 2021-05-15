# Predict
nn.predict <- function(X, w1, w2, b, drop.p){
  k <- length(b)
  diag1 <- lapply(1:k, function(j){
    w1[[j]] %*% t(X)
  })
  ht <- lapply(1:k, function(j){
    af(diag1[[j]]) * drop.p
  })
  diag2 <- lapply(1:k, function(j){
    w2[[j]] %*% ht[[j]]
  })
  g <- lapply(1:k, function(j){
    af(diag2[[j]]) + t(b[[j]]) %*% t(X)
  })
  return(g)
}

