#' Title The Brute Force Knapsnack Solver
#'
#' @param x The input to function containing the number of items
#' @param W The weights of the items
#'
#' @return NULL
#' @export
#' @examples brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
brute_force_knapsack <- function(x, W){

# x = knapsack_objects[1:12,]
# W = 3861

x$item <- rownames(x)
x <- x[,c("item", "w", "v")]
N = NROW(x)
weights <- as.vector(x[, c("w")])
value <- as.vector(x[, c("v")])
items <- as.vector(x[, c("item")])

appended_output <- all_combination(N,x,weights,items,value)
appended_output$weight_match <- ifelse(appended_output$total_weight == W, "Match", "No-match")
df_weight_match <- if(NROW(appended_output) == 1){appended_output[appended_output$weight_match == "Match",]}else{return("No match")}

value = df_weight_match$total_value
elements = as.vector(df_weight_match$total_item)

return(list(value, elements))
}

