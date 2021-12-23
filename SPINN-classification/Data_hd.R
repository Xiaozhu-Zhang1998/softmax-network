# Data
library(MASS)
Data.hd.generate <- function(p = 200, k = 3, n = 10000, nk = 80){
  mu <- rep(0, p)
  rho <- 0.2
  Sigma <- diag(rep(1,p))
  for(i in 1:p){
    for(j in 1:p){
      if(i!=j) Sigma[i,j] = rho ** abs(i-j)
    }
  }
  X <- mvrnorm(n, mu = mu, Sigma = Sigma)
  coef <- runif(120, -15, 15)
  var.id <- sample(1:200, 120)
  var.id1 <- var.id[1:40]; var.id2 <- var.id[41:80]; var.id3 <- var.id[81:120]
  sd = 1
  eg1 <- exp(X[, var.id1] ** 2 %*% as.matrix(coef[1:40]) + rnorm(n, sd))
  eg2 <- exp(X[, var.id2] ** 2 %*% as.matrix(coef[41:80]) + rnorm(n, sd))
  eg3 <- exp(X[, var.id3] ** 2 %*% as.matrix(coef[81:120]) + rnorm(n, sd))
  eg <- cbind(eg1, eg2, eg3)
  prob <- eg / rowSums(eg)
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
                    label = rep(1:k, each = nk))
  dat <- dat[sample(nk * k),]
  return(list(X = as.matrix(dat[,1:p]),
              label = as.factor(dat$label)))
}

