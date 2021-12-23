# Predict
nn.predict <- function(X, w1, w2, drop.p){
  diag1 <- w1 %*% t(X)
  ht <- af(diag1) * drop.p
  diag2 <- w2 %*% ht
  g <- diag2 
  return(g)
}

