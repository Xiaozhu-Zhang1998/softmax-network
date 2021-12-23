source("NetStructure.R")
source("Activation.R")

source("Init.R")
source("Forward.R")
source("Backprop.R")
source("Adam.R")
source("Network.R")

source("integ-SPINN.R")
source("Predict.R")
source("Validation.R")

# Data ----
source("Data_com_mul.R")
source("Data_add_uni.R")
set.seed(1234)
dat <- Data.cm(p = 25, n = 500)
X.dat <- dat$X
y.dat <- dat$y

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
lambda0 <- c(0.001)
lambda <- c(0, 1.4, 1.5, 1.6, 1,7, 1.8, 1.9, 2)
gamma <- c(5)
for(j in 1:length(gamma)){
  for(i in 1:length(lambda)){
    set.seed(2021-4-24)
    RATE <- c()
    for(loop in 1:30){ # Repeat 30 times
      # foo
      rate <- validate(X.dat, y.dat, fold, lambda0, lambda[i], a = 0.5,
                     t = 100, q = 50, alpha0 = 0.05, batchsize = 64, 
                     nepoch = 100, gamma[j])
      RATE <- c(RATE, rate)
    }
    rate <- mean(RATE) / var(y.dat)
    sd <- sd(RATE / var(y.dat))
    print(paste(rate, sd, lambda[i], gamma[j]))
  }
}
t2 <- proc.time()
t <- t2 - t1
print(t[3][[1]] / 60)
beep(8)