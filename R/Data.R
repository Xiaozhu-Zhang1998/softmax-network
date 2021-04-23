# Data
library(MASS)
Data.generate <- function(){
  mu <- c(0,0,0,0)
  rho <- 0.2
  Sigma <- diag(c(1,1,1,1))
  for(i in 1:4){
    for(j in 1:4){
      if(i!=j) Sigma[i,j] = rho ** abs(i-j)
    }
  }
  X <- mvrnorm(n = 1000, mu = mu, Sigma = Sigma)
  eg1 <- exp(4 * X[,1] + 2 * X[,2] ** 2 + X[,3] ** 2)
  eg2 <- exp(-2 * X[,3] * X[,4])
  eg3 <- exp(rep(0, 1000))
  eg <- cbind(eg1, eg2, eg3)
  prob <- eg / apply(eg, 1, sum)
  label <- as.factor(as.vector(sapply(1:1000, function(i){
    which(prob[i,] == max(prob[i,]))
  })))
  return(list(X = X, label = label))
}

