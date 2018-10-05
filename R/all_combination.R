#' Title Function to return all combinations of objects
#'
#' @param N Number of rows in the input dataset
#' @param x The input dataset
#' @param weights The vector containing the weights of the items
#' @param items The vector containing the list of the items
#' @param value The vector containing the value of the items
#'
#' @return NULL
#' @export
#' @importFrom gtools combinations
#'
all_combination <- function(N,x,weights,items,value){

x$item <- rownames(x)
x <- x[,c("item", "w", "v")]
N = NROW(x)
weights <- as.vector(x[, c("w")])
value <- as.vector(x[, c("v")])
items <- as.vector(x[, c("item")])
appended_output <- data.frame()

for(i in 1:N){
    temp_weights <- gtools::combinations(n=N, r=i, v=weights, repeats.allowed = FALSE)
    temp_items <- gtools::combinations(n=N, r=i, v=items, repeats.allowed = FALSE)
    temp_value <- gtools::combinations(n=N, r=i, v=value, repeats.allowed = FALSE)

    output <- cbind(total_value = rowSums(temp_value),
                    total_items = apply(temp_items, 1, paste, collapse=" "),
                    total_weight = rowSums(temp_weights))

    appended_output <- rbind(appended_output, output)
  }
  return(appended_output)
}

