# Loss function
J <- function(label, g, n){
  class <- sapply(1:n, function(i){
    which(label[i] == levels(label))
    })
  rs1 <- sum(sapply(1:n, function(i){
    g[class[i], i]
  }))
  rs2 <- sum(log(apply(exp(g), 2, sum)))
  return(rs2 - rs1)
}

# Loss Gradient
dJ <- function(label, g, n){
  class <- sapply(1:n, function(i){
    which(label[i] == levels(label))
  })
  eg <- exp(g)
  rs <- sapply(1:(nrow(g)), function(j){
    -(class == j) * 1 + (eg[j, ] / apply(eg, 2, sum))
  })
  rs[is.na(rs)] <- 1
  return(t(rs))
}

# Soft-thresholding
Slambda <- function(lambda, x){
  max(x - lambda, 0)
}