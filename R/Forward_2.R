# Forward
forward <- function(n, k, X, label, w1, w2, w3, b){
  diag1 <- lapply(1:k, function(j){
    w1[[j]] %*% t(X)
  })
  h1 <- lapply(1:k, function(j){
    af(diag1[[j]])
  })
  diag2 <- lapply(1:k, function(j){
    w2[[j]] %*% h1[[j]]
  })
  h2 <- lapply(1:k, function(j){
    af(diag2[[j]])
  })
  diag3 <- lapply(1:k, function(j){
    w3[[j]] %*% h2[[j]]
  })
  g <- lapply(1:k, function(j){
    af(diag3[[j]]) + t(b[[j]]) %*% t(X)
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
  
  diag3 <- lapply(1:k, function(j){
    daf(diag3[[j]])
  })
  return(list(diag1 = diag1, diag2 = diag2, diag3 = diag3,
              h1 = h1, h2 = h2, g = g, dJg = dJg))
}