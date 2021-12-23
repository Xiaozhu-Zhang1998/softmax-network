## A 3-layer NN
Network <- function(X, label, lambda0, lambda, a,
                    t, alpha0, batchsize, drop.p, nepoch,
                    beta1 = 0.9, beta2 = 0.999, decay = 1e-3)
{
  # Structure
  struc <- NetStructure(X, label) # foo
  n <- struc$n; p <- struc$p; k <- struc$k
  
  # Init
  init.val <- init(p, k, t) # foo
  w1 <- init.val$w1; w2 <- init.val$w2
  first_moment_1 <- init.val$first_moment_1
  first_moment_2 <- init.val$first_moment_2
  second_moment_1 <- init.val$second_moment_1
  second_moment_2 <- init.val$second_moment_2

  alpha <- alpha0
  for(epoch in 1:nepoch){
    # Forward
    current.val <- forward(n, k, X, label, w1, w2, drop.p) # foo
    
    # Backward
    grad <- backprop(X, n, p, k, t, w2, current.val$diag1, current.val$diag2,
                     current.val$ht, current.val$g, current.val$dJg, batchsize, lambda0) # foo
    # Adam
    update <- adam(k, alpha, beta1, beta2, first_moment_1, first_moment_2,
                   second_moment_1, second_moment_2, 
                   w1, w2, grad$dJw1, grad$dJw2, epoch) # foo
    w1 <- update$w1; w2 <- update$w2
    first_moment_1 <- update$first_moment_1
    first_moment_2 <- update$first_moment_2
    second_moment_1 <- update$second_moment_1
    second_moment_2 <- update$second_moment_2
    
    # dropout
    # w1 <- lapply(1:k, function(j){
    #   w.dim <- dim(w1[[j]])
    #   U <- matrix(runif(w.dim[1] * w.dim[2]), w.dim[1]) < drop.p
    #   return(w1[[j]] * U)
    # })
    
    # Hier-Prox
    w1 <- SPINN(p, t, w1, alpha, lambda, a)
   
    # 1/t decay
    alpha <- alpha0 / (1 + decay * epoch)
  }
  
  return(list(w1 = w1, w2 = w2, g = current.val$g))
}
