# Forward
forward <- function(n, X, y, w1, w2, w3){
  diag1 <- w1 %*% t(X)
  
  ht <- af(diag1)
  diag2 <- w2 %*% ht
  
  hq <- af(diag2)
  diag3 <- w3 %*% hq
  
  f <- af(diag3)
  
  # Components update
  dLf <- 2 * (f - y)
  diag1 <- daf(diag1)
  diag2 <- daf(diag2)
  diag3 <- daf(diag3)
  
  return(list(diag1 = diag1, diag2 = diag2, diag3 = diag3,
              ht = ht, hq = hq, f = f, dLf = dLf))
}