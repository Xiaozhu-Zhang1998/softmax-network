# Adam

adam <- function(k, alpha, beta1, beta2,
                 first_moment_1, first_moment_2, first_moment_b,
                 second_moment_1, second_moment_2, second_moment_b,
                 w1, w2, b, dJw1, dJw2, dJb, loop){
  first_moment_1 <- lapply(1:(k-1), function(j){
    beta1 * first_moment_1[[j]] + (1 - beta1) * dJw1[[j]]
  })
  first_moment_2 <- lapply(1:(k-1), function(j){
    beta1 * first_moment_2[[j]] + (1 - beta1) * dJw2[[j]]
  })
  first_moment_b <- lapply(1:(k-1), function(j){
    beta1 * first_moment_b[[j]] + (1 - beta1) * dJb[[j]]
  })
  
  second_moment_1 <- lapply(1:(k-1), function(j){
    beta2 * second_moment_1[[j]] + (1 - beta2) * dJw1[[j]] * dJw1[[j]]
  })
  second_moment_2 <- lapply(1:(k-1), function(j){
    beta2 * second_moment_2[[j]] + (1 - beta2) * dJw2[[j]] * dJw2[[j]]
  })
  second_moment_b <- lapply(1:(k-1), function(j){
    beta2 * second_moment_b[[j]] + (1 - beta2) * dJb[[j]] * dJb[[j]]
  })
  
  first_unbias_1 <- lapply(1:(k-1), function(j){
    first_moment_1[[j]] / (1 - beta1 ** loop)
  })
  first_unbias_2 <- lapply(1:(k-1), function(j){
    first_moment_2[[j]] / (1 - beta1 ** loop)
  })
  first_unbias_b <- lapply(1:(k-1), function(j){
    first_moment_b[[j]] / (1 - beta1 ** loop)
  })
  
  second_unbias_1 <- lapply(1:(k-1), function(j){
    second_moment_1[[j]] / (1 - beta2 ** loop)
  })
  second_unbias_2 <- lapply(1:(k-1), function(j){
    second_moment_2[[j]] / (1 - beta2 ** loop)
  })
  second_unbias_b <- lapply(1:(k-1), function(j){
    second_moment_b[[j]] / (1 - beta2 ** loop)
  })
  
  w1 <- lapply(1:(k-1), function(j){
    w1[[j]] - alpha * first_unbias_1[[j]] / (sqrt(second_unbias_1[[j]]) + 1e-7)
  })
  w2 <- lapply(1:(k-1), function(j){
    w2[[j]] - alpha * first_unbias_2[[j]] / (sqrt(second_unbias_2[[j]]) + 1e-7)
  })
  b <- lapply(1:(k-1), function(j){
    b[[j]] - alpha * first_unbias_b[[j]] / (sqrt(second_unbias_b[[j]]) + 1e-7)
  })
  return(list(w1 = w1, w2 = w2, b = b,
              first_moment_1 = first_moment_1,
              first_moment_2 = first_moment_2,
              first_moment_b = first_moment_b,
              second_moment_1 = second_moment_1,
              second_moment_2 = second_moment_2,
              second_moment_b = second_moment_b))
}

