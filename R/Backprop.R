# Backprop
# gradients
backprop <- function(X, n, p, k, t, w2,
                     diag1, diag2, ht, g, dJg, batchsize){
  id <- sample(1:n, batchsize) # mini-batch
  dJw2 <- lapply(1:k, function(j){
    sapply(id, function(i){ # mini-batch 1:n
      dJg[j,i] %*% diag2[[j]][,i] %*% t(matrix(ht[[j]][,i]))
    })
  })
  dJw2 <- lapply(1:k, function(j){
    matrix(apply(dJw2[[j]], 1, sum), 1)
  })
  
  dJw1 <- lapply(1:k, function(j){
    sapply(id, function(i){ # mini-batch
      diag(diag1[[j]][,i]) %*% t(w2[[j]]) %*% dJg[j,i] %*% matrix(diag2[[j]][,i]) %*% t(matrix(X[i,]))
    })
  })
  dJw1 <- lapply(1:k, function(j){
    matrix(apply(dJw1[[j]], 1, sum), t)
  })
  
  dJb <- lapply(1:k, function(j){
    sapply(id, function(i){ # mini-batch
      dJg[j,i] * matrix(X[i,])
    })
  })
  dJb <- lapply(1:k, function(j){
    matrix(apply(dJb[[j]], 1, sum), p)
  })
  return(list(dJw1 = dJw1, dJw2 = dJw2, dJb = dJb))
}