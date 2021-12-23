# Hier-Prox
Hier_Prox <- function(p, t, b, w, lambda, M){
  w <- t(w)
  for(j in 1:p){
    wj.sort <- sort(abs(w[j,]), decreasing = T)
    wj.sort <- c(wj.sort, 0)
    for(m in 0:t){
      summation <- ifelse(m == 0, 0, sum(wj.sort[1:m])) 
      wm <- M / (1 + m * M**2) * Slambda(lambda, abs(b[j]) + M * summation)
      if(m == 0){if(wm >= wj.sort[m+1]) break}
      if(m != 0){if(wm >= wj.sort[m+1] & wm <= wj.sort[m]) break}
    }
    b[j] <- 1/M * sign(b[j]) * wm
    w[j,] <- sign(w[j,]) * sapply(w[j,], function(x){min(wm, x)})
  }
  return(list(b = b, w = t(w)))
}
