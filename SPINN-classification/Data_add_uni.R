# Data
library(MASS)
# The first dataset ----
Data.au1 <- function(p = 25, k = 2, n = 10000, nk = 60){
  mu <- rep(0, p)
  rho <- 0.2
  Sigma <- diag(rep(1,p))
  for(i in 1:p){
    for(j in 1:p){
      if(i!=j) Sigma[i,j] = rho ** abs(i-j)
    }
  }
  X <- mvrnorm(n, mu = mu, Sigma = Sigma)
  eg1 <- exp(3 * X[,1]  + 6 * X[,2]**2 - X[,3]**3 + 2 * sin(X[,4]) - 4 * cos(X[,5]) + rnorm(n, sd = 0.01))
  eg2 <- rep(1, n)
  eg <- cbind(eg1, eg2)
  prob <- eg / rowSums(eg)
  label <- as.factor(as.vector(sapply(1:n, function(i){
    which(prob[i,] == max(prob[i,]))
  })))
  
  # select
  id1 <- label == 1;  id2 <- label == 2
  X1 <- X[id1,];  X2 <- X[id2,]
  sample.id1 <- sample(sum(id1), nk)
  sample.id2 <- sample(sum(id2), nk)
  X1 <- X1[sample.id1,]; X2 <- X2[sample.id2,]
  dat <- data.frame(rbind(X1, X2), 
                    label = rep(c(1,2), each = nk))
  dat <- dat[sample(nk * k),]
  return(list(X = as.matrix(dat[,1:p]),
              label = as.factor(dat$label)))
}


# The second dataset ----
Data.au2 <- function(p = 25, k = 2, n = 10000, nk = 60){
  mu <- rep(0, p)
  rho <- 0.2
  Sigma <- diag(rep(1,p))
  for(i in 1:p){
    for(j in 1:p){
      if(i!=j) Sigma[i,j] = rho ** abs(i-j)
    }
  }
  X <- mvrnorm(n, mu = mu, Sigma = Sigma)
  eg1 <- exp(4 * X[,3]  + 5 * X[,4]**2 - 2 * X[,5]**3 + 3 * cos(X[,6]) - 6 * log(abs(X[,7])) + rnorm(n, sd = 0.01))
  eg2 <- rep(1, n)
  eg <- cbind(eg1, eg2)
  prob <- eg / rowSums(eg)
  label <- as.factor(as.vector(sapply(1:n, function(i){
    which(prob[i,] == max(prob[i,]))
  })))
  
  # select
  id1 <- label == 1;  id2 <- label == 2
  X1 <- X[id1,];  X2 <- X[id2,]
  sample.id1 <- sample(sum(id1), nk)
  sample.id2 <- sample(sum(id2), nk)
  X1 <- X1[sample.id1,]; X2 <- X2[sample.id2,]
  dat <- data.frame(rbind(X1, X2), 
                    label = rep(c(1,2), each = nk))
  dat <- dat[sample(nk * k),]
  return(list(X = as.matrix(dat[,1:p]),
              label = as.factor(dat$label)))
}


# The third dataset ----
Data.au3 <- function(p = 25, k = 2, n = 10000, nk = 60){
  mu <- rep(0, p)
  rho <- 0.2
  Sigma <- diag(rep(1,p))
  for(i in 1:p){
    for(j in 1:p){
      if(i!=j) Sigma[i,j] = rho ** abs(i-j)
    }
  }
  X <- mvrnorm(n, mu = mu, Sigma = Sigma)
  eg1 <- exp(-5 * X[,6] + X[,7]**2 + 3 * X[,8]**3 + rnorm(n, sd = 0.01))
  eg2 <- rep(1, n)
  eg <- cbind(eg1, eg2)
  prob <- eg / rowSums(eg)
  label <- as.factor(as.vector(sapply(1:n, function(i){
    which(prob[i,] == max(prob[i,]))
  })))
  
  # select
  id1 <- label == 1;  id2 <- label == 2
  X1 <- X[id1,];  X2 <- X[id2,]
  sample.id1 <- sample(sum(id1), nk)
  sample.id2 <- sample(sum(id2), nk)
  X1 <- X1[sample.id1,]; X2 <- X2[sample.id2,]
  dat <- data.frame(rbind(X1, X2), 
                    label = rep(c(1,2), each = nk))
  dat <- dat[sample(nk * k),]
  return(list(X = as.matrix(dat[,1:p]),
              label = as.factor(dat$label)))
}


# The whole dataset ----
Data.au <- function(p = 25, k = 2, n = 10000, nk = 60){
  d1 <- Data.au1(p, k, n, nk)
  d2 <- Data.au2(p, k, n, nk)
  d3 <- Data.au3(p, k, n, nk)
  X <- matrix(0, nrow = nk * k * 3, ncol = p * 3)
  X[1:(nk * k), (1:p)] <- d1$X
  X[(nk * k + 1):(2 * nk * k), (p + 1):(2 * p)] <- d2$X
  X[(2 * nk * k + 1):(3 * nk * k), (2 * p + 1):(3 * p)] <- d3$X
  label <- c(d1$label, d2$label, d3$label)
  return(list(X = as.matrix(X), label = as.factor(label)))
}