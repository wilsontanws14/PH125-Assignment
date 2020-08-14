options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

titanic %>% ggplot(aes(Age, color = Sex)) + geom_density()

female <- titanic %>% filter(Sex == "female" & !is.na(Age)) 
nrow(female)

male <- titanic %>% filter(Sex == "male" & !is.na(Age))
nrow(male)

max(male$Age)
max(female$Age)

titanic %>% ggplot(aes(Age, color = Sex)) + geom_histogram(binwidth = 1) + facet_grid(Sex~.)

params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age)) 

titanic %>% filter(!is.na(Age)) %>% ggplot(aes(sample = Age)) + geom_qq(dparams=params) + geom_abline()
  
titanic %>% ggplot(aes(Survived, fill= Sex)) + geom_bar(position=position_dodge())

titanic %>% 
  filter(!is.na(Age)) %>% 
  mutate(weight = 1/nrow(titanic)) %>%
  ggplot(aes(Age, y=..count.., fill = Survived, weight = weight)) + 
    geom_density(alpha = 0.2, position = "stack")

titanic %>% 
  filter(Fare>0) %>% 
  ggplot(aes(Survived, Fare)) + 
    geom_boxplot() +
    geom_jitter(width = 0.1, alpha = 0.2) + 
    scale_y_continuous(trans ="log2")

titanic %>%
  ggplot(aes(Pclass, fill = Survived)) + 
    geom_bar(position = position_fill())

titanic %>% 
  filter(!is.na(Age)) %>% 
  mutate(weight = 1/nrow(titanic)) %>%
  ggplot(aes(Age, y=..count.., fill = Survived)) + 
  geom_density(alpha = 0.2, position = "stack") + 
  facet_grid(Sex~Pclass)
