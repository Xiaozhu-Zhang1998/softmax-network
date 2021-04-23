# Loss function
J <- function(label, g, n){
  g.aug <- rbind(g, rep(0, n))
  class <- sapply(1:n, function(i){
    which(label[i] == levels(label))
    })
  rs1 <- sum(sapply(1:n, function(i){
    g.aug[class[i], i]
  }))
  rs2 <- sum(log(apply(exp(g.aug), 2, sum)))
  return(rs2 - rs1)
}

# Loss Gradient
dJ <- function(label, g, n){
  g.aug <- rbind(g, rep(0, n))
  class <- sapply(1:n, function(i){
    which(label[i] == levels(label))
  })
  eg <- exp(g.aug)
  rs <- sapply(1:(nrow(g)), function(i){
    -(class == i) * 1 + (eg[i, ] / apply(eg, 2, sum))
  })
  return(t(rs))
}

# Soft-thresholding
Slambda <- function(lambda, x){
  max(x - lambda, 0)
}