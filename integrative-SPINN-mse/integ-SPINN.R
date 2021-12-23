# integrative SPINN

integrative_SPINN <- function(p, t, w1, alpha, lambda, a, gamma){
  p <- p / 3
  for(j in 1:p){
    for(m in 1:3){
      Omega <- sapply(1:3, function(m){
        (1 - a) * sum(abs(w1[, j + p * (m-1)])) + a * sqrt(sum(w1[, j + p * (m-1)] ** 2))
      })
      C <- lambda * max(0, 1 - sum(Omega) / (lambda * gamma))
      # group lasso
      ind <- j + p *(m - 1)
      w1[,ind] <- max(0, 1 - C * alpha * a / sqrt(sum(w1[,ind] ** 2))) * w1[,ind]
      # lasso
      w1[,ind] <- sapply(1:t, function(i){
        sign(w1[i, ind]) * max(0, abs(w1[i, ind]) - C * (1-a) * alpha)
      })
    }
  }
  return(w1)
}