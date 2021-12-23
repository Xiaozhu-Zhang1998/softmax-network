# Data
library(MASS)
# The first dataset ----
Data.cm1 <- function(p = 25, n = 10000){
  mu <- rep(0, p)
  rho <- 0.2
  Sigma <- diag(rep(1,p))
  for(i in 1:p){
    for(j in 1:p){
      if(i!=j) Sigma[i,j] = rho ** abs(i-j)
    }
  }
  X <- mvrnorm(n, mu = mu, Sigma = Sigma)
  y <- 3 * (X[,1] + X[,2]) * X[,3] ** 2  + 5 * cos(X[,4] + exp(X[,5])) + rnorm(n, sd = 0.01)
  return(list(X = as.matrix(X), y = y))
}


# The second dataset ----
Data.cm2 <- function(p = 25, n = 10000){
  mu <- rep(0, p)
  rho <- 0.2
  Sigma <- diag(rep(1,p))
  for(i in 1:p){
    for(j in 1:p){
      if(i!=j) Sigma[i,j] = rho ** abs(i-j)
    }
  }
  X <- mvrnorm(n, mu = mu, Sigma = Sigma)
  y <- -4 * (X[,3] + X[,4]) * X[,5] ** 3 - 7 * sin(X[,6] ** 2 + X[,7] ** 2) + rnorm(n, sd = 0.01)
  return(list(X = as.matrix(X), y = y))
}


# The third dataset ----
Data.cm3 <- function(p = 25, n = 10000){
  mu <- rep(0, p)
  rho <- 0.2
  Sigma <- diag(rep(1,p))
  for(i in 1:p){
    for(j in 1:p){
      if(i!=j) Sigma[i,j] = rho ** abs(i-j)
    }
  }
  X <- mvrnorm(n, mu = mu, Sigma = Sigma)
  y <- 5 * (X[,7] ** 2 * X[,9] + X[,8] ** 2 * cos(X[,9])) + rnorm(n, sd = 0.01)
  return(list(X = as.matrix(X), y = y))
}


# The whole dataset ----
Data.cm <- function(p = 25, n = 10000){
  d1 <- Data.cm1(p, n)
  d2 <- Data.cm2(p, n)
  d3 <- Data.cm3(p, n)
  X <- matrix(0, nrow = n * 3, ncol = p * 3)
  X[1:n, (1:p)] <- d1$X
  X[(n + 1):(2 * n), (p + 1):(2 * p)] <- d2$X
  X[(2 * n + 1):(3 * n), (2 * p + 1):(3 * p)] <- d3$X
  y <- c(d1$y, d2$y, d3$y)
  return(list(X = as.matrix(X), y = y))
}