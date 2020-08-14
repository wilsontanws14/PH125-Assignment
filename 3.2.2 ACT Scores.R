set.seed(16, sample.kind="Rounding")
act_scores <-rnorm(10000, 20.9,5.7)
mean(act_scores)
sd(act_scores)
sum(act_scores >= 36)
mean(act_scores > 30)
mean(act_scores <= 10)
x <- (1:36)
f_x <- dnorm(x, 20.9, 5.7)
plot(x, f_x)

20.9 + 2*5.7
1 - pnorm(32.3, 20.9, 5.7)
qnorm(pnorm(32.3, 20.9, 5.7),20.9, 5.7)
qnorm(0.9772499, 20.9, 5.7)
qnorm(0.975, 20.9, 5.7)

qnorm(0.95, 20.9, 5.7)

p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(x, p)
sample_quantiles[26]
sample_quantiles[80]

theoretical_quantiles <- qnorm(p, 20.9, 5.7)
qqplot(sample_quantiles, theoretical_quantiles)
