# Group MCP

gMCP_l1 <- function(p, t, b, w, lambda, M, gamma = 3.7){
  v <- b
  u <- w
  beta <- b
  count <- 0
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
    s <- sort(s)
    s.sum <- cumsum(s)
    bound <- dMCP(sum(abs(beta)), lambda, gamma) / M
    s.sum[s.sum >= bound] <- 0
    s.sum <- max(s.sum)
    
    # update b
    a <- dMCP(sum(abs(beta)), lambda, gamma) - M * s.sum
    print(a)
    b <- sapply(1:p, function(j){ # j indicates each feature
      sign(v[j]) * max(0, abs(v[j]) - a)
    })
    
    # finish this epoch
    count <- count + 1
    if(sum(abs(beta - b)) <= p * 1e-3) break
    if(count == 50) break
    beta <- b
  }
  return(list(b = as.matrix(b), w = w))
}

# ======================================

# b <- b.store[[1]]
# w <- w1.store[[1]]

gMCP_MCP <- function(p, t, b, w, lambda, M, gamma = 6){
  v <- b
  u <- w
  beta <- v
  for(loop in 1:5){ 
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

    # update b
    for(j in 1:p){ # j indicates each feature
      # find sum(s)
      bound <- dcMCP(beta, j, lambda, gamma) / M
      sj <- s
      sj[sj > bound] <- 0
      sj <- sort(sj, decreasing = T)
      s.sum <- cumsum(sj)
      s.sum[s.sum >= bound] <- 0
      s.sum <- max(s.sum)
      # update
      a <- dcMCP(beta, j, lambda, gamma) - M * s.sum 
      #print(a)
      b[j] <- sign(v[j]) * max(0, abs(v[j]) - a)
    }
    
    # finish this epoch
    #print(sum(abs(beta - b)))
    beta <- b
  }
  return(list(b = b, w = w))
}
