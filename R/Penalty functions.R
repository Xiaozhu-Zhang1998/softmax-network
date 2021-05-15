# Penalty functions
# MCP function
MCP <- function(x, lambda, gamma){
  if(x <= gamma * lambda){
    return(lambda * x - x ** 2 / (2 * gamma))
  }
  else{
    return(0.5 * gamma * lambda ** 2)
  }
}

# Composite MCP function
cMCP <- function(x, lambda, gamma){
  rs <- sapply(1:length(x), function(i){
    MCP(x[i], lambda, gamma)
  })
  rs <- sum(rs)
  rs <- MCP(rs, lambda, gamma)
  return(rs)
}

# Derivative of MCP function
dMCP <- function(x, lambda, gamma){
  rs <- lambda * max(1e-7, 1 - x / (lambda * gamma))
  return(rs)
}

# Derivative of composite MCP function
dcMCP <- function(x, ind, lambda, gamma){
  A <- sapply(1:length(x), function(i){
    MCP(abs(x[i]), lambda, gamma)
  })
  A <- sum(A)
  A <- dMCP(A, lambda, gamma)
  B <- dMCP(abs(x[ind]), lambda, gamma)
  return(A * B)
}