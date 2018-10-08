#' Title The Brute Force Knapsnack Solver
#'
#' @param x The input to function containing the number of items
#' @param W The weights of the items
#'
#' @return NULL
#' @export
#' @examples brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
brute_force_knapsack <- function(x, W){
    stopifnot(is.data.frame(x),is.numeric(W))
    i=2
    optimum_value = 0
    selected_items = c()
    weights<-c()
    values<-c()
    while(i<=nrow(x))
    {
      w<-as.data.frame(combn(x[,1], i))
      v<-as.data.frame(combn(x[,2], i))
      sumw<-colSums(w)
      sumv<-colSums(v)
      weights<-which(sumw<=W)
      if(length(weights) != 0){
        values<-sumv[weights]
        optimum_value<-max(values)
        temp<-which((values)==optimum_value)
        maxValWghtIdx<-weights[temp]
        maxValWght<-w[, maxValWghtIdx]
        j<-1
        while (j<=i){
          selected_items[j]<-which(x[,1]==maxValWght[j])
          j=j+1
        }
      }
      i=i+1

    }

    return(list(value=round(optimum_value),elements=selected_items))
}
