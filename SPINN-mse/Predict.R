# Predict
nn.predict <- function(X, w1, w2, w3, drop.p){
  diag1 <- w1 %*% t(X)
  ht <- af(diag1) * drop.p
  diag2 <- w2 %*% ht
  hq <- af(diag2)
  diag3 <- w3 %*% hq
  f <- af(diag3)
  return(f)
}

