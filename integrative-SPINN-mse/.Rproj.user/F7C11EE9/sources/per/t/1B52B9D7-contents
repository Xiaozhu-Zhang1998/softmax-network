# Backprop
# gradients
backprop <- function(X, n, p, t, q, w2, w3,
                     diag1, diag2, diag3, ht, hq, dLf, batchsize, lambda0){
  id <- sample(1:n, batchsize) # mini-batch
  
  dLw3 <- sapply(id, function(i){ # mini-batch 
    dLf[i] %*% diag3[i] %*% t(hq[,i]) 
  })
  dLw3 <- matrix(rowSums(dLw3), 1) + lambda0 * w3
  
  dLw2 <- sapply(id, function(i){ # mini-batch
    diag(diag2[,i]) %*% t(w3) %*% dLf[i] %*% diag3[i] %*% t(ht[,i])
  })
  dLw2 <- matrix(rowSums(dLw2), q) + lambda0 * w2
  
  dLw1 <- sapply(id, function(i){ # mini-batch
    diag(diag1[,i]) %*% t(w2) %*% diag(diag2[,i]) %*% t(w3) %*% dLf[i] %*% diag3[i] %*% t(as.matrix(X[i,]))   })
  dLw1 <- matrix(rowSums(dLw1), t)
  
  return(list(dLw1 = dLw1, dLw2 = dLw2, dLw3 = dLw3))
}