# Initialization
init <- function(p, k, t){
  w1 <- lapply(1:(k-1), function(j){
    matrix(rnorm(t * p), t, p) / p
  })
  
  w2 <- lapply(1:(k-1), function(j){
    matrix(rnorm(1 * t), 1, t) / t
  })
  
  b <- lapply(1:(k-1), function(j){
    matrix(rnorm(p))
  })
  
  first_moment_1 <- lapply(1:(k-1), function(j){0})
  first_moment_2 <- lapply(1:(k-1), function(j){0})
  first_moment_b <- lapply(1:(k-1), function(j){0})
  second_moment_1 <- lapply(1:(k-1), function(j){0})
  second_moment_2 <- lapply(1:(k-1), function(j){0})
  second_moment_b <- lapply(1:(k-1), function(j){0})
  
  return(list(w1 = w1, w2 = w2, b = b,
              first_moment_1 = first_moment_1, 
              first_moment_2 = first_moment_2,
              first_moment_b = first_moment_b,
              second_moment_1 = second_moment_1, 
              second_moment_2 = second_moment_2, 
              second_moment_b = second_moment_b))
}

