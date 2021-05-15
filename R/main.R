source("NetStructure.R")
source("Loss.R")
source("Activation.R")

source("Init.R")
source("Forward.R")
source("Backprop.R")
source("Adam.R")
source("Network.R")

# source("Init_2.R")
# source("Forward_2.R")
# source("Backprop_2.R")
# source("Adam_2.R")
# source("Network_2.R")

# source("Hier_Prox.R")
source("Hier_Prox_gMCP.R")
source("Penalty functions.R")
source("Predict.R")
source("Accuracy.R")
source("Validation.R")

# Data ----
source("Data.R")
source("Data_hd.R")
set.seed(1234)
dat <- Data.hd.generate()
X.dat <- dat$X
label.dat <- dat$label

# Split / 5-fold ----
set.seed(1234)
n.row <- dim(X.dat)[1]
n.f <- n.row / 5
shuffle.id <- sample(n.row)
fold <- lapply(1:5, function(i){
  shuffle.id[(n.f * i - (n.f - 1)):(n.f * i)]
})

# # iris ----
# X.iris <- as.matrix(iris[,1:4])
# X.iris <- scale(X.iris)
# label.iris <- iris$Species
# 
# # Split / 5-fold ----
# set.seed(1234)
# shuffle.id <- sample(150)
# fold <- lapply(1:5, function(i){
#   shuffle.id[(30 * i - 29):(30 * i)]
# })



# Cross-validate ----
# `X` must be a matrix!
# `label` must be a factor!
t1 <- proc.time()
#lambda <- c(1e-3, 5e-3, 1e-2, 5e-2, 1e-1, 5e-1)
lambda <- c(1)
M <- c(3)
best <- 0; lambda0 <- NULL; M0 <- NULL
for(j in 1:length(M)){
  for(i in 1:length(lambda)){
    set.seed(2021-4-24)
    # foo
    val <- validate(X.dat, label.dat, fold, lambda[i], M[j],
                     t = 50, alpha0 = 0.05, batchsize = 64, 
                     drop.p = 1, nepoch = 400)
    rate <- val$rate; fea <- val$fea
    print(paste(rate, fea, lambda[i], M[j]))
    if(rate > best){
      best <- rate
      lambda0 <- lambda[i]
      M0 <- M[j]
    }
  }
}
t2 <- proc.time()
t <- t2 - t1
print(t[3][[1]] / 60)
