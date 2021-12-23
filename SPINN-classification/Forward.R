# Forward
forward <- function(n, k, X, label, w1, w2, drop.p){
  diag1 <- w1 %*% t(X)
  # dropout 
  # dg.dim <- dim(diag1)
  # U <- (matrix(runif(dg.dim[1] * dg.dim[2]), dg.dim[1], dg.dim[2]) < drop.p)
  # diag1 <- diag1 * U
  
  ht <- af(diag1)
  diag2 <- w2 %*% ht
  
  g <- diag2
  
  # Components update
  dJg <- dJ(label, g, n, clust)
  diag1 <- daf(diag1)
  
  return(list(diag1 = diag1, diag2 = diag2, 
              ht = ht, g = g, dJg = dJg))
}