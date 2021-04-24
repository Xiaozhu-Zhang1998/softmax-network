source("NetStructure.R")
source("Loss.R")
source("Activation.R")

#source("Init.R")
source("Init_2.R")

#source("Forward.R")
source("Forward_2.R")

#source("Backprop.R")
source("Backprop_2.R")

#source("Adam.R")
source("Adam.R_2")

source("Hier_Prox.R")

#source("Network.R")
source("Network_2.R")

source("Accuracy.R")
source("Validation.R")

# Data ----
# source("Data.R")
# set.seed(2021-4-22)
# dat <- Data.generate()
# X <- dat$X
# label <- dat$label

# iris ----
X.iris <- as.matrix(iris[,1:4])
X.iris <- scale(X.iris)
label.iris <- iris$Species

# Split / 5-fold ----
shuffle.id <- sample(150)
fold <- lapply(1:5, function(i){
  shuffle.id[(30 * i - 29):(30 * i)]
})



# Cross-validate ----
# `X` must be a matrix!
# `label` must be a factor!
set.seed(2021-4-24)
lambda <- c(1e-3, 5e-3, 1e-2, 5e-2, 1e-1, 5e-1, 1, 2)
M <- seq(5, 10, by = 1)
best <- 0; lambda0 <- NULL; M0 <- NULL
for(i in 1:length(lambda)){
  for(j in 1:length(M)){
    # foo
    rate <- validate(X.iris, label.iris, fold, lambda[i], M[j],
                     t = 30, alpha0 = 0.1, batchsize = 64)
    print(paste(rate, lambda[i], M[j]))
    if(rate > best){
      best <- rate
      lambda0 <- lambda[i]
      M0 <- M[j]
    }
  }
}