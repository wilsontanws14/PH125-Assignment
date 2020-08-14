library(gtools)
permutations(8,3)
combinations(8,3)
3/8 * 2/7 * 1/6

runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")

set.seed(1)

B <- 10000

races <- replicate(B, {
  winners <- sample(runners, 3)
  all(winners[1] == "Jamaica", winners[2] == "Jamaica", winners[3]=="Jamaica")
})


mean(races)
