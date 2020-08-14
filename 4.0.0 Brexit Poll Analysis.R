library(tidyverse)
options(digits = 3)
library(dslabs)
data(brexit_polls)

p <- 0.481
d <- 2*p-1

N <- 1500

p*N

se <- sqrt(p*(1-p)/N)

se*N

2*se

head(brexit_polls)

brexit_polls <- brexit_polls %>% 
  mutate(x_hat = (spread + 1)/2)

mean(brexit_polls$spread)

sd(brexit_polls$spread)

mean(brexit_polls$x_hat)

sd(brexit_polls$x_hat)

yougov_poll <- brexit_polls[1,]
yougov_poll
N <- yougov_poll$samplesize
X_hat <- yougov_poll$x_hat
se_hat <- sqrt(X_hat*(1-X_hat)/N)
se_hat
qnorm(0.975)
ci <- c(X_hat - qnorm(0.975)*se_hat, X_hat + qnorm(0.975)*se_hat)
ci

june_polls <- brexit_polls %>%
    filter(enddate >= "2016-06-01") %>%
    mutate(se_x_hat = sqrt(x_hat*(1-x_hat)/samplesize),
           se_hat = 2*se_x_hat,
           lower = spread - 1.96*se_hat, 
           upper = spread + 1.96*se_hat, 
           hit = lower <= -0.038 & upper >= -0.038) 

nrow(june_polls)

june_polls %>% summarize(mean(hit))

june_polls %>% filter(lower > 0)

june_polls %>% 
    mutate(hit2 = lower <= -0.038 & upper >= -0.038) %>% 
    summarize(mean(hit2))

june_table <- june_polls %>%
  group_by(pollster) %>% 
  summarize(n = sum(samplesize), hit = mean(hit)) %>% 
  arrange(desc(hit))

june_polls %>% ggplot(aes(poll_type, spread)) +
    geom_boxplot()

combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2)

combined_by_type <- combined_by_type %>%
    mutate(se_hat = 2 * sqrt(p_hat*(1-p_hat)/N),
           lower = spread - qnorm(0.975)*se_hat,
           upper = spread + qnorm(0.975)*se_hat)



library(tidyverse)

# load brexit_polls object and add x_hat column
library(dslabs)
data(brexit_polls)
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1)/2)

# final proportion voting "Remain"
p <- 0.481

brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)

table <- brexit_hit %>%
  group_by(poll_type, hit) %>% 
  summarize(hit_rate = n()) %>%
  spread(poll_type, hit_rate)


chisq_test <- table %>%
  select(-hit) %>%
  chisq.test()

odds_online <- (table[[2,2]] / sum(table[[2]])) / 
  (table[[1,2]] / sum(table[[2]]))

odds_telephone <- (table[[2,3]] / sum(table[[3]])) / 
  (table[[1,3]] / sum(table[[3]]))

odds_online/odds_telephone

brexit_polls %>% ggplot(aes(enddate, spread, color=poll_type)) +
  geom_smooth(method = "loess", span = 0.4) +
  geom_point() +
  geom_hline(yintercept=-0.038, color="black")

brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))

brexit_long %>% ggplot(aes(enddate, proportion, color = vote)) + 
  geom_smooth(method = "loess", span = 0.3) 
