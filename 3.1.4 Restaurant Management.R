nrow(combinations(6,2))
15*6*2
15*6*3
nrow(combinations(6,3))
20*6*3

meal_combi <- function(n){
  3 * 15 * n
}

entrees <- (1:12)

sapply(entrees, meal_combi)

meal_combi_2 <- function(n){
  3 * 6 * nrow(combinations(n, 2))
}

sides <- (2:12)

sapply(sides, meal_combi_2)
