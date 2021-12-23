# Data
library(MASS)
Data.hd.generate <- function(p = 20, k = 3, n = 10000, nk = 100){
  mu <- rep(0, p)
  rho <- 0.2
  Sigma <- diag(rep(1,p))
  for(i in 1:p){
    for(j in 1:p){
      if(i!=j) Sigma[i,j] = rho ** abs(i-j)
    }
  }
  X <- mvrnorm(n, mu = mu, Sigma = Sigma)
  eg1 <- exp(-10 * X[,2] ** 2 - X[,6] ** 2 + 8 * X[,10] ** 2)
  eg2 <- exp(9 * X[,4] * X[,8] * X[,12])
  eg3 <- exp(-5 * X[,14] - 14 * X[,16] * sin(X[,18]))
  eg <- cbind(eg1, eg2, eg3)
  prob <- eg / apply(eg, 1, sum)
  label <- as.factor(as.vector(sapply(1:n, function(i){
    which(prob[i,] == max(prob[i,]))
  })))
  
  # select
  id1 <- label == 1;  id2 <- label == 2; id3 <- label == 3
  X1 <- X[id1,];  X2 <- X[id2,];  X3 <- X[id3,]
  sample.id1 <- sample(sum(id1), nk)
  sample.id2 <- sample(sum(id2), nk)
  sample.id3 <- sample(sum(id3), nk)
  X1 <- X1[sample.id1,]; X2 <- X2[sample.id2,]; X3 <- X3[sample.id3,]
  dat <- data.frame(rbind(X1, X2, X3), 
                    label = rep(c(1,2,3), each = nk))
  dat <- dat[sample(nk * k),]
  return(list(X = as.matrix(dat[,1:p]),
              label = as.factor(dat$label)))
}

