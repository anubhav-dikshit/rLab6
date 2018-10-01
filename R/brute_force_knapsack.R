#' Title The Brute Force Knapsnack Solver
#'
#' @param x The input to function containing the number of items
#' @param W The weights of the items
#'
#' @return
#' @export
#'
#' @examples brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
brute_force_knapsack <- function(x, W){

}


library(gtools)
knapsack_objects$item <- rownames(knapsack_objects)
knapsack_objects <- knapsack_objects[,c("item", "w", "v")]
x = knapsack_objects[1:8,]
w = 3500
rows_in_x = NROW(x)
weights <- as.vector(knapsack_objects[1:8,c("w")])
value <- as.vector(knapsack_objects[1:8,c("v")])
items <- as.vector(knapsack_objects[1:8,c("item")])

for(i in 1:nrow(x)){
temp_weights <- combinations(n=rows_in_x, r=i, v=weights, repeats.allowed = FALSE)
temp_items <- combinations(n=rows_in_x, r=i, v=items, repeats.allowed = FALSE)
temp_value <- combinations(n=rows_in_x, r=i, v=value, repeats.allowed = FALSE)

temp_weights <- cbind(temp_weights, total_weight = rowSums(temp_weights))
temp_items <- cbind(temp_items, total_items = rowSums(temp_items))
temp_value <- cbind(temp_value, total_value = rowSums(temp_value))


temp_weight$match <- ifelse(temp_weight$total == w, "Match", "Nomatch")

}
