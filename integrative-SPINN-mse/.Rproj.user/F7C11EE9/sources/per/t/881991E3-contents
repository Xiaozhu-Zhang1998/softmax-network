# Initialization
init <- function(p, t, q){
  w1 <- matrix(rnorm(t * p), t, p) / p 
  w2 <- matrix(rnorm(q * t), q, t) / t 
  w3 <- matrix(rnorm(1 * q), 1, q) / q
  
  first_moment_1 <- 0
  first_moment_2 <- 0
  first_moment_3 <- 0
  second_moment_1 <- 0
  second_moment_2 <- 0
  second_moment_3 <- 0

  return(list(w1 = w1, w2 = w2, w3 = w3, 
              first_moment_1 = first_moment_1, 
              first_moment_2 = first_moment_2,
              first_moment_3 = first_moment_3,
              second_moment_1 = second_moment_1, 
              second_moment_2 = second_moment_2,
              second_moment_3 = second_moment_3))
}

