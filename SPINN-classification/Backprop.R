# Backprop
# gradients
backprop <- function(X, n, p, k, t, w2,
                     diag1, diag2, ht, g, dJg, batchsize, lambda0){
  id <- sample(1:n, batchsize) # mini-batch
  
  dJw2 <- sapply(id, function(i){ # mini-batch 
    diag(dJg[,i]) %*% as.matrix(rep(1,k)) %*% t(ht[,i]) 
  })
  dJw2 <- matrix(rowSums(dJw2), k) + lambda0 * w2
  
  dJw1 <- sapply(id, function(i){ # mini-batch
    diag(diag1[,i]) %*% t(w2) %*% as.matrix(dJg[,i]) %*% t(as.matrix(X[i,]))   })
  dJw1 <- matrix(rowSums(dJw1), t)
  
  return(list(dJw1 = dJw1, dJw2 = dJw2))
}