# Group MCP

gMCP_MCP <- function(p, t, w, lambda, gamma = 4){
  u <- w
  for(loop in 1:3){ 
    for(m in 1:t){
      for(j in 1:p){
        a <- dcMCP(w[,j], t, lambda, gamma) 
        w[m,j] <- sign(u[m,j]) * max(0, abs(u[m,j]) - a)
        }
      }
  }
  return(w)
}
