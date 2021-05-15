# Data
library(MASS)
Data.generate <- function(){
  mu <- c(0,0,0,0,0,0)
  rho <- 0.2
  Sigma <- diag(c(1,1,1,1,1,1))
  for(i in 1:6){
    for(j in 1:6){
      if(i!=j) Sigma[i,j] = rho ** abs(i-j)
    }
  }
  X <- mvrnorm(n = 10000, mu = mu, Sigma = Sigma)
  eg1 <- exp(-1.2 * X[,1] ** 2 - X[,2] ** 2)
  eg2 <- exp(2 * X[,3] * X[,4])
  eg3 <- exp(0.5 * X[,5] - 2 * X[,6])
  eg <- cbind(eg1, eg2, eg3)
  prob <- eg / apply(eg, 1, sum)
  label <- as.factor(as.vector(sapply(1:10000, function(i){
    which(prob[i,] == max(prob[i,]))
  })))
  
  # select
  id1 <- label == 1;  id2 <- label == 2; id3 <- label == 3
  X1 <- X[id1,];  X2 <- X[id2,];  X3 <- X[id3,]
  sample.id1 <- sample(sum(id1), 300)
  sample.id2 <- sample(sum(id2), 300)
  sample.id3 <- sample(sum(id3), 300)
  X1 <- X1[sample.id1,]; X2 <- X2[sample.id2,]; X3 <- X3[sample.id3,]
  dat <- data.frame(rbind(X1, X2, X3), 
                    label = rep(c(1,2,3), each = 300))
  dat <- dat[sample(900),]
  return(list(X = as.matrix(dat[,1:6]), label = as.factor(dat$label)))
}

