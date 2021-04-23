# Forward
forward <- function(n, k, X, w1, w2, b){
  diag1 <- lapply(1:(k-1), function(j){
    w1[[j]] %*% t(X)
  })
  ht <- lapply(1:(k-1), function(j){
    af(diag1[[j]])
  })
  diag2 <- lapply(1:(k-1), function(j){
    w2[[j]] %*% ht[[j]]
  })
  g <- lapply(1:(k-1), function(j){
    af(diag2[[j]]) + t(b[[j]]) %*% t(X)
  })
  
  
  # Components update
  g.mat <- do.call(rbind, g)
  dJg <- dJ(label, g.mat, n)
  
  diag1 <- lapply(1:(k-1), function(j){
    daf(diag1[[j]])
  })
  
  diag2 <- lapply(1:(k-1), function(j){
    daf(diag2[[j]])
  })
  return(list(diag1 = diag1, diag2 = diag2, 
              ht = ht, g = g, dJg = dJg))
}

