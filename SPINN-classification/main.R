source("NetStructure.R")
source("Loss.R")
source("Activation.R")

source("Init.R")
source("Forward.R")
source("Backprop.R")
source("Adam.R")
source("Network.R")

source("Hier_Prox_gMCP.R")
source("SPINN.R")
source("Penalty functions.R")
source("Predict.R")
source("Accuracy.R")
source("Validation.R")

# Data ----
source("Data_add_uni.R")
source("Data_hd.R")
set.seed(1234)
dat <- Data.au(p = 25, k = 2, n = 10000, nk = 500)
X.dat <- dat$X
label.dat <- dat$label

# Split / 5-fold ----
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
#lambda <- c(1e-3, 5e-3, 1e-2, 5e-2, 1e-1, 5e-1)
lambda0 <- c(0.001)
lambda <- c(0.1, 0.5, 1, 1.5, 2, 5)
a <- c(0.5)
best <- 0; lambda0.b <- NULL; lambda.b <- NULL
for(i in 1:length(lambda)){
  for(j in 1:length(a)){
    set.seed(2021-4-24)
    RATE <- c()
    for(loop in 1:30){ # Repeat 30 times
      # foo
      rate <- validate(X.dat, label.dat, fold, lambda0, lambda[i], a[j],
                     t = 10, alpha0 = 0.05, batchsize = 64, 
                     drop.p = 1, nepoch = 30)
      RATE <- c(RATE, rate)
    }
    rate <- mean(RATE)
    print(paste(rate, lambda[i], a[j]))
    if(rate > best){
      best <- rate
      lambda0.b <- lambda0
      lambda.b <- lambda[i]
    }
  }
}
t2 <- proc.time()
t <- t2 - t1
print(t[3][[1]] / 60)
