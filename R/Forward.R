# Forward
forward <- function(n, k, X, label, w1, w2, b, drop.p){
  diag1 <- lapply(1:k, function(j){
    w1[[j]] %*% t(X)
  })
  # dropout 
  diag1 <- lapply(1:k, function(j){
    dg.dim <- dim(diag1[[j]])
    U <- (matrix(runif(dg.dim[1] * dg.dim[2]), dg.dim[1], dg.dim[2]) < drop.p)
    return(diag1[[j]] * U)
  })
  ht <- lapply(1:k, function(j){
    af(diag1[[j]])
  })
  diag2 <- lapply(1:k, function(j){
    w2[[j]] %*% ht[[j]]
  })
  g <- lapply(1:k, function(j){
    af(diag2[[j]]) + t(b[[j]]) %*% t(X)
  })
  
  
  # Components update
  g.mat <- do.call(rbind, g)
  dJg <- dJ(label, g.mat, n)
  
  diag1 <- lapply(1:k, function(j){
    daf(diag1[[j]])
  })
  
  diag2 <- lapply(1:k, function(j){
    daf(diag2[[j]])
  })
  return(list(diag1 = diag1, diag2 = diag2, 
              ht = ht, g = g, dJg = dJg))
}