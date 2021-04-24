## A 3-layer NN
Network <- function(X, label, lambda, M, 
                    t, alpha0, batchsize,
                    beta1 = 0.9, beta2 = 0.999, nepoch = 50, decay = 1e-3)
{
  # Structure
  struc <- NetStructure(X, label) # foo
  n <- struc$n; p <- struc$p; k <- struc$k
  
  # Init
  init.val <- init(p, k, t) # foo
  w1 <- init.val$w1; w2 <- init.val$w2; b <- init.val$b
  first_moment_1 <- init.val$first_moment_1
  first_moment_2 <- init.val$first_moment_2
  first_moment_b <- init.val$first_moment_b
  second_moment_1 <- init.val$second_moment_1
  second_moment_2 <- init.val$second_moment_2
  second_moment_b <- init.val$second_moment_b
  
  alpha <- alpha0
  for(epoch in 1:nepoch){
    # Forward
    current.val <- forward(n, k, X, label, w1, w2, b) # foo
    
    # Backward
    grad <- backprop(X, n, p, k, t, w2, current.val$diag1, current.val$diag2,
                     current.val$ht, current.val$g, current.val$dJg, batchsize) # foo
    # Adam
    update <- adam(k, alpha, beta1, beta2, first_moment_1, first_moment_2,
                   first_moment_b, second_moment_1, second_moment_2, second_moment_b,
                   w1, w2, b, grad$dJw1, grad$dJw2, grad$dJb, epoch) # foo
    w1 <- update$w1; w2 <- update$w2; b <- update$b
    first_moment_1 <- update$first_moment_1
    first_moment_2 <- update$first_moment_2
    first_moment_b <- update$first_moment_b
    second_moment_1 <- update$second_moment_1
    second_moment_2 <- update$second_moment_2
    second_moment_b <- update$second_moment_b
    
    # Hier-Prox
    for(j in 1:k){
      rs <- Hier_Prox(p, t, b[[j]], w1[[j]], lambda, M)
      b[[j]] <- rs$b
      w1[[j]] <- rs$w
    }
    
    # 1/t decay
    alpha <- alpha0 / (1 + decay * epoch)
  }
  
  return(list(b = b, w1 = w1, w2 = w2, g = current.val$g))
}