library(tidyverse)
library(dslabs)
data(death_prob)
head(death_prob)

p <- death_prob%>% filter(sex=="Female" & age=="50")%>%.$prob

a <- -150000
b <- 1150

mu <- a*p + b*(1-p)

se <- abs(b-a)*sqrt(p*(1-p))

n <- 1000
mu_s <- n*mu
se_s <- sqrt(n)*se

pnorm(0, mu_s, se_s)

p <- death_prob%>% filter(sex=="Male" & age=="50")%>%.$prob

b <- ((700000/1000) - -150000*p)/(1-p)
se <- abs(b-a)*sqrt(p*(1-p))

mu_s <- 700000
se_s <- sqrt(n)*se

pnorm(0, mu_s, se_s)

p <- 0.015
a <- -150000
b <- 1150

mu <- a*p + b*(1-p)

se <- abs(b-a)*sqrt(p*(1-p))

n <- 1000
mu_s <- n*mu
se_s <- sqrt(n)*se
pnorm(0, mu_s, se_s)
pnorm(-1000000, mu_s, se_s)

p <- seq(0.01, 0.03, 0.001)
f_x <- function(p){
  a <- -150000
  b <- 1150
  n <- 1000
  mu_s <- n*(a*p + b*(1-p))
  se_s <- sqrt(n)*(abs(b-a)*sqrt(p*(1-p)))
  pnorm(0, mu_s, se_s)
}
sapply(p, FUN=f_x)

p <- seq(0.01, 0.03, 0.0025)
f_x <- function(p){
  a <- -150000
  b <- 1150
  n <- 1000
  mu_s <- n*(a*p + b*(1-p))
  se_s <- sqrt(n)*(abs(b-a)*sqrt(p*(1-p)))
  pnorm(-1000000, mu_s, se_s)
}
sapply(p, FUN=f_x)

set.seed(25, sample.kind="Rounding")
p_loss <- 0.015
n <- 1000
X <- sample(c(-150000, 1150), n, replace=TRUE, prob=c(p_loss, 1-p_loss))
sum(X)

set.seed(27)
B<-10000
S <- replicate(B, {
  X <- sample(c(-150000, 1150), n, replace=TRUE, prob=c(p_loss, 1-p_loss))
  sum(X)
})
sum(S<=-1000000)/10000

p <- .015
n <- 1000
l <- -150000
z <- qnorm(.05)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))

l*p + x*(1-p)

set.seed(28)
B<-10000
S <- replicate(B, {
  X <- sample(c(l, x), n, replace=TRUE, prob=c(p, 1-p))
  sum(X)
})
mean(S<0)

set.seed(29)
n <- 1000
B <- 10000
l <- -150000
p <- 0.015
x <- 3268
X <- replicate(B, {
  new_p <- p + sample(seq(-0.01, 0.01, length=100), 1)
  Y <- sample(c(x, l), n, replace=TRUE, prob=c(1-new_p, new_p))
  sum(Y)
})

mean(X)
sum(X<0)/B
mean(X< -1000000)
