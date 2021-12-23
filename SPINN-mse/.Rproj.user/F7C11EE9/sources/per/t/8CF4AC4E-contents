# Data
library(MASS)
load("param.Rdata")

# The first dataset ----
Data.hd1 <- function(p = 1000, n = 500){
  mu <- rep(0, p)
  rho <- 0.2
  Sigma <- diag(rep(1,p))
  for(i in 1:p){
    for(j in 1:p){
      if(i!=j) Sigma[i,j] = rho ** abs(i-j)
    }
  }
  X <- mvrnorm(n, mu = mu, Sigma = Sigma)
  y <- rowSums(sapply(1:100, function(j){
    coef1[j] * X[,j] ** power1[j]
  })) + rnorm(n, sd = 0.01)
  return(list(X = as.matrix(X), y = y))
}


# The second dataset ----
Data.hd2 <- function(p = 1000, n = 500){
  mu <- rep(0, p)
  rho <- 0.2
  Sigma <- diag(rep(1,p))
  for(i in 1:p){
    for(j in 1:p){
      if(i!=j) Sigma[i,j] = rho ** abs(i-j)
    }
  }
  X <- mvrnorm(n, mu = mu, Sigma = Sigma)
  y <- rowSums(sapply(1:33, function(j){
    coef2[j] * X[, 47 + 3 * j] ** power2[3 * j-2] * X[, 48 + 3 * j] ** power2[3 * j-1] * X[, 49 + 3 * j] ** power2[3 * j]
  })) + rnorm(n, sd = 0.01)
  return(list(X = as.matrix(X), y = y))
}


# The third dataset ----
Data.hd3 <- function(p = 1000, n = 500){
  mu <- rep(0, p)
  rho <- 0.2
  Sigma <- diag(rep(1,p))
  for(i in 1:p){
    for(j in 1:p){
      if(i!=j) Sigma[i,j] = rho ** abs(i-j)
    }
  }
  X <- mvrnorm(n, mu = mu, Sigma = Sigma)
  y <- rowSums(sapply(1:33, function(j){
    coef3[2*j - 1] * X[, 198 + 3 * j] ** power3[3 * j-2] + coef3[2*j] * X[, 199 + 3 * j] ** power3[3 * j-1] * X[, 200 + 3 * j] ** power3[3 * j]
  })) + rnorm(n, sd = 0.01)
  return(list(X = as.matrix(X), y = y))
}


# The whole dataset ----
Data.hd <- function(p = 1000, n = 500){
  d1 <- Data.hd1(p, n)
  d2 <- Data.hd2(p, n)
  d3 <- Data.hd3(p, n)
  X <- matrix(0, nrow = n * 3, ncol = p * 3)
  X[1:n, (1:p)] <- d1$X
  X[(n + 1):(2 * n), (p + 1):(2 * p)] <- d2$X
  X[(2 * n + 1):(3 * n), (2 * p + 1):(3 * p)] <- d3$X
  y <- c(d1$y, d2$y, d3$y)
  return(list(X = as.matrix(X), y = y))
}