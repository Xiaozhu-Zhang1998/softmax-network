SPINN <- function(p, t, w1, alpha, lambda, a){
  temp <- sapply(1:length(w1), function(i){
    max(0, abs(w1[i]) - alpha * lambda * (1 - a))
  })
  w1 <- sign(w1) * matrix(temp, nrow(w1), ncol(w1))
  w1 <- sapply(1:p, function(j){
    norm2 <- sqrt(sum(w1[,j] ** 2))
    coeff <- max(0, (1 - alpha * lambda * a / norm2))
    return(coeff * w1[,j])
  })
}