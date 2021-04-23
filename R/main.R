source("NetStructure.R")
source("Loss.R")
source("Activation.R")
source("Init.R")
source("Forward.R")
source("Backprop.R")
source("Adam.R")
source("Hier_Prox.R")
source("Network.R")
source("Accuracy.R")
source("Validation.R")

# Data ----
source("Data.R")
dat <- Data.generate()
X <- dat$X
label <- dat$label

# Split / 5-fold ----
shuffle.id <- sample(1000)
fold <- lapply(1:5, function(i){
  shuffle.id[(200 * i - 199):(200 * i)]
})



# Cross-validate ----
set.seed(2021-4-22)
lambda <- c(1e-3, 5e-3, 1e-2, 5e-2, 1e-1, 2.5e-1, 5e-1, 7.5e-1, 1)
M <- seq(5, 20, by = 0.5)
best <- 0; lambda0 <- NULL; M0 <- NULL
for(i in 1:length(lambda)){
  for(j in 1:length(M)){
    # foo
    rate <- validate(X, label, fold, lambda[i], M[j])
    print(paste(rate, lambda[i], M[j]))
    if(rate > best){
      best <- rate
      lambda0 <- lambda[i]
      M0 <- M[j]
    }
  }
}
