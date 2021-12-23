SPINN <- function(p, t, w1, alpha, lambda, a){
  w1 <- sign(w1) * max(0, abs(w1) - alpha * lambda * (1 - a))
  w1 <- sapply(1:p, function(j){
    norm2 <- sqrt(sum(w1[,j] ** 2))
    coeff <- max(0, (1 - alpha * lambda * a / norm2))
    return(coeff * w1[,j])
  })
}