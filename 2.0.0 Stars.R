library(tidyverse)
library(dslabs)
data(stars)
options(digits = 3) 

stars %>% 
  ggplot(aes(magnitude)) +
  geom_density()

stars %>% 
  ggplot(aes(temp)) +
  geom_density()

stars %>% 
  ggplot(aes(temp, magnitude)) + 
  geom_point()

stars %>% 
  ggplot(aes(log10(temp), magnitude)) + 
  geom_point() + 
  scale_y_reverse() + 
  scale_x_reverse()

stars %>% 
  ggplot(aes(temp, magnitude, label=star)) + 
  geom_point() + 
  geom_text(nudge_y = -0.5) +
  scale_y_reverse() + 
  scale_x_reverse()

stars %>% 
  filter(star == "Sun")

stars %>% 
  filter(type == "G") %>%
  ggplot(aes(temp, magnitude, color=type)) +
  geom_point() + 
  scale_y_reverse() + 
  scale_x_reverse()
