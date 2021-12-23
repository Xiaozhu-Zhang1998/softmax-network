# Cross-validation

# validation
nnTree <- function(X, label, n.tree, n.var, lambda0, lambda, clust,
                     t, alpha0, batchsize, drop.p, nepoch){ 
  # Variable fold
  var.fold <- lapply(1:n.tree, function(i){
    sample(1:ncol(X), n.var)
  })
  # Forest
  prob <- matrix(rep(0, length(levels(label)) * nrow(X)), length(levels(label)))
  for(i in 1:n.tree){
    # Training network
    rs <- Network(X[,var.fold[[i]]], label, lambda0, lambda, clust,
                t, alpha0, batchsize, drop.p, nepoch)
    # Prob
    g.test <- nn.predict(X.test[,var.fold[[i]]], rs$w1, rs$w2, drop.p)
    prob <- prob + sapply(1:ncol(g.test), function(i){
      exp(g.test[,i]) / sum(exp(g.test[,i]))
    })
  }
  # Vote
  prob <- prob / n.tree
  ind <- sapply(1:ncol(g.test), function(i){
    which(max(prob[,i]) == prob[,i])[1]
  })
  tab <- table(ind, label.test)
  
  return(sum(diag(tab)) / sum(tab))
}