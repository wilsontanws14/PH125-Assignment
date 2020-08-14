S <- sample(c(-0.25, 1), 44, c(0.8, 0.2), replace = TRUE)
mean(S)

avg <- -0.25*0.8+1*0.2

sd <- sqrt(44)*abs(1-(-0.25))*sqrt(0.2*0.8)

1 - pnorm(8, 0, sd)

set.seed(21, sample.kind = "Rounding")
B <- 10000
n <- 44

S <- replicate(B, {
  X <- sample(c(-0.25, 1), n, replace = TRUE, prob = c(0.8, 0.2))
  sum(X)
})

mean(S>=8)

mu <- 44 * (1/4 * 1 + 0* 3/4)
er <- sqrt(44) * abs(1) * sqrt(1/4*3/4)
1-pnorm(30, mu, er)

p <- seq(0.25, 0.95, 0.05)

fu <- function(p){
  # calculate the expected value at given p
  expected_value <- 44 * (1*p + 0*(1-p))
  # calculate the standard error at given p
  standard_error <- sqrt(44) * abs(1 - 0) * sqrt(p*(1 - p))
  # calculate likelihood of score of 35 or greater
  1-pnorm(35, expected_value, standard_error)
}

f_x <- sapply(p, FUN=fu)
plot(p, f_x)

mu <- 500 * (5/38*6 + 33/38*-1)
er <- sqrt(500) * abs(6-(-1))*sqrt(5/38*33/38)

1-pnorm(0, mu, er)
