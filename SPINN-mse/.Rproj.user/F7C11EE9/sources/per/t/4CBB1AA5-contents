library(beepr)
source("NetStructure.R")
source("Activation.R")

source("Init.R")
source("Forward.R")
source("Backprop.R")
source("Adam.R")
source("Network.R")

source("SPINN.R")
source("Predict.R")
source("Validation.R")

# Data ----
source("Data_com_mul.R")
source("Data_add_uni.R")
source("Data_high_dim.R")
set.seed(1234)
dat <- Data.hd(p = 1000, n = 4000)
X.dat <- dat$X
y.dat <- dat$y

# Split / 3-fold ----
set.seed(1234)
n.row <- dim(X.dat)[1]
n.f <- n.row / 3
shuffle.id <- sample(n.row)
fold <- lapply(1:3, function(i){
  shuffle.id[(n.f * i - (n.f - 1)):(n.f * i)]
})



# Cross-validate ----
# `X` must be a matrix!
# `label` must be a factor!
t1 <- proc.time()
lambda0 <- c(0.001)
lambda <- c(0.5, 1, 1.5)
a <- c(0.5)
best <- 0; lambda0.b <- NULL; lambda.b <- NULL
for(i in 1:length(lambda)){
  for(j in 1:length(a)){
    set.seed(2021-4-24)
    RATE <- c()
    for(loop in 1:30){ # Repeat 30 times
      # foo
      rate <- validate(X.dat, y.dat, fold, lambda0, lambda[i], a[j],
                     t = 100, q = 50, alpha0 = 0.05, batchsize = 64, 
                     drop.p = 1, nepoch = 100)
      RATE <- c(RATE, rate)
    }
    rate <- mean(RATE) / var(y.dat)
    sd <- sd(RATE / var(y.dat))
    print(paste(rate, sd, lambda[i], a[j]))
    # if(rate > best){
    #   best <- rate
    #   lambda0.b <- lambda0
    #   lambda.b <- lambda[i]
    # }
  }
}
t2 <- proc.time()
t <- t2 - t1
print(t[3][[1]] / 60)
beep(8)