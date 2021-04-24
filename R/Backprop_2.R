# Backprop
# gradients
backprop <- function(X, n, p, k, t, w2, w3,
                     diag1, diag2, diag3, h1, h2, g, dJg, batchsize){
  id <- sample(1:n, batchsize) # mini-batch
  dJw3 <- lapply(1:k, function(j){
    sapply(id, function(i){ # mini-batch
      dJg[j,i] %*% diag3[[j]][,i] %*% t(matrix(h2[[j]][,i]))
    })
  })
  dJw3 <- lapply(1:k, function(j){
    matrix(apply(dJw3[[j]], 1, sum), 1)
  })
  
  dJw2 <- lapply(1:k, function(j){
    sapply(id, function(i){ # mini-batch
      diag(diag2[[j]][,i]) %*% t(w3[[j]]) %*% dJg[j,i] %*% diag3[[j]][,i] %*% t(matrix(h1[[j]][,i]))
    })
  })
  dJw2 <- lapply(1:k, function(j){
    matrix(apply(dJw2[[j]], 1, sum), t)
  })
  
  dJw1 <- lapply(1:k, function(j){
    sapply(id, function(i){ # mini-batch
      diag(diag1[[j]][,i]) %*% t(w2[[j]]) %*% diag(diag2[[j]][,i]) %*% t(w3[[j]]) %*% dJg[j,i] %*% diag3[[j]][,i] %*% t(matrix(X[i,]))
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
  return(list(dJw1 = dJw1, dJw2 = dJw2, dJw3 = dJw3, dJb = dJb))
}