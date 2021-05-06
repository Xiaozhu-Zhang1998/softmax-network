# Group MCP

gMCP_l1 <- function(p, t, b, w, lambda, M, gamma = 3.7){
  beta <- b
  v <- b
  u <- w
  while(TRUE){ 
    s <- matrix(rep(0, p * t), t, p)
    
    # update w
    for(m in 1:t){
      for(j in 1:p){
        w[m,j] <- sign(u[m,j]) * min(M * abs(beta[j]), abs(u[m,j]))
        if(abs(w[m,j]) == M * abs(beta[j])){
          s[m,j] <- sign(w[m,j]) * (u[m,j] - w[m,j]) 
        }
      }
    }
    
    # find sum(s)
    s <- sort(s, decreasing = T)
    s.sum <- cumsum(s)
    bound <- dMCP(sum(abs(beta)), lambda, gamma) / M
    s.sum[s.sum >= bound] <- 0
    s.sum <- max(s.sum)
    
    # update b
    for(j in 1:p){ # j indicates each feature
      a <- (dMCP(sum(abs(beta)), lambda, gamma) - M * sj.sum) 
      b[j] <- sign(v[j]) * max(0, abs(v[j]) - a)
    }
    
    # finish this epoch
    if(abs(beta - b) <= 1e-3) break
    beta <- b
  }
  return(list(b = b, w = w))
}

gMCP_MCP <- function(p, t, b, w, lambda, M, gamma = 3.7){
  beta <- b
  v <- b
  u <- w
  while(TRUE){ 
    s <- matrix(rep(0, p * t), t, p)
    
    # update w
    for(m in 1:t){
      for(j in 1:p){
        w[m,j] <- sign(u[m,j]) * min(M * abs(beta[j]), abs(u[m,j]))
        if(abs(w[m,j]) == M * abs(beta[j])){
          s[m,j] <- sign(w[m,j]) * (u[m,j] - w[m,j]) 
        }
      }
    }
    
    # prep sum(s)
    s <- sort(s, decreasing = T)
    s <- cumsum(s)
    
    # update b
    for(j in 1:p){ # j indicates each feature
      # find sum(s)
      bound <- dcMCP(beta, j, lambda, gamma)  * dMCP(abs(beta[j]), lambda, gamma) / M
      s.sum <- s
      s.sum[s.sum >= bound] <- 0
      s.sum <- max(s.sum)
      # update
      a <- (dcMCP(beta, j, lambda, gamma)  * dMCP(abs(beta[j]), lambda, gamma) - M * sj.sum) 
      b[j] <- sign(v[j]) * max(0, abs(v[j]) - a)
    }
    
    # finish this epoch
    if(abs(beta - b) <= 1e-3) break
    beta <- b
  }
  return(list(b = b, w = w))
}
