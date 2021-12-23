## A 3-layer NN
Network <- function(X, y, lambda0, lambda, a,
                    t, q, alpha0, batchsize, drop.p, nepoch,
                    beta1 = 0.9, beta2 = 0.999, decay = 1e-3)
{
  # Structure
  struc <- NetStructure(X, label) # foo
  n <- struc$n; p <- struc$p
  
  # Init
  init.val <- init(p, t, q) # foo
  w1 <- init.val$w1; w2 <- init.val$w2; w3 <- init.val$w3
  first_moment_1 <- init.val$first_moment_1
  first_moment_2 <- init.val$first_moment_2
  first_moment_3 <- init.val$first_moment_3
  second_moment_1 <- init.val$second_moment_1
  second_moment_2 <- init.val$second_moment_2
  second_moment_3 <- init.val$second_moment_3

  alpha <- alpha0
  for(epoch in 1:nepoch){
    # Forward
    current.val <- forward(n, X, y, w1, w2, w3, drop.p) # foo
    
    # Backward
    grad <- backprop(X, n, p, t, q, w2, w3, current.val$diag1, current.val$diag2, current.val$diag3,
                     current.val$ht, current.val$hq, current.val$dLf, batchsize, lambda0) # foo
    # Adam
    update <- adam(alpha, beta1, beta2, first_moment_1, first_moment_2, first_moment_3,
                   second_moment_1, second_moment_2, second_moment_3,
                   w1, w2, w3, grad$dLw1, grad$dLw2, grad$dLw3, epoch) # foo
    w1 <- update$w1; w2 <- update$w2; w3 <- update$w3
    first_moment_1 <- update$first_moment_1
    first_moment_2 <- update$first_moment_2
    first_moment_3 <- update$first_moment_3
    second_moment_1 <- update$second_moment_1
    second_moment_2 <- update$second_moment_2
    second_moment_3 <- update$second_moment_3
    
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
  
  return(list(w1 = w1, w2 = w2, w3 = w3, f = current.val$f))
}
