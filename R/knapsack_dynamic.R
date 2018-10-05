knapsack_dynamic <- function(x, W){
  x = knapsack_objects[1:12,]
  W = 2000

  x$item <- rownames(x)
  x <- x[,c("item", "w", "v")]
  N = NROW(x)
  weights <- as.vector(x[, c("w")])
  value <- as.vector(x[, c("v")])
  items <- as.vector(x[, c("item")])
  knap_dynamic_cal()

  knap_dynamic_cal <- function(n,k){
  if (N == 0 || W == 0){
    return(0)
  } else if (weights[1] > W) {
    output <- knapsack_dynamic(x[-1,],W)
  } else {
    output <- max(value[1]+ knapsack_dynamic(x[-1,], k-w[1]), knapsack_dynamic(x[-1,], k))
  }
  return(output)
    }
  }
