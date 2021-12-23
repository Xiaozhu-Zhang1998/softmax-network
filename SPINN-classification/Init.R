# Initialization
init <- function(p, k, t){
  w1 <- matrix(rnorm(t * p), t, p) / p 
  w2 <- matrix(rnorm(k * t), k, t) / t 
  
  first_moment_1 <- 0
  first_moment_2 <- 0
  second_moment_1 <- 0
  second_moment_2 <- 0

  return(list(w1 = w1, w2 = w2, 
              first_moment_1 = first_moment_1, 
              first_moment_2 = first_moment_2,
              second_moment_1 = second_moment_1, 
              second_moment_2 = second_moment_2))
}

