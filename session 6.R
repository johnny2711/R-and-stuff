n <- 4
p <- 1.0
A <- matrix(0, nrow=n, ncol=n)

for(i in 2:n){
  for(j in 1:(i-1)){
      if(runif(1) <= p){
        A[i, j] <- 1
      }
    }
    
  }
A <- A + t(A)
A
plot(graph_from_adjacency_matrix(A))

