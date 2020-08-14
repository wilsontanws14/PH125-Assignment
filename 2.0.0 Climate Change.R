library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  select(year) %>%
  max()

temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  select(year) %>%
  min()

temp_carbon %>% 
  filter(year == 2014)
 
temp_carbon %>% 
  filter(year == 1979)

p <-
  temp_carbon %>%
    filter(!is.na(temp_anomaly)) %>%
      ggplot(aes(year, temp_anomaly)) +
      geom_line()

p <- p + geom_hline(aes(yintercept=0), col = "blue")

p + ylab("Temperature anomaly (degrees C)") +
  title("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean", col = "blue"))

p + ylim("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")

p + ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean", col = "blue"))

p + ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")

p + ylab("Temperature anomaly (degrees C)") +
  title("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")

temp_carbon %>%
  filter(temp_anomaly>0.05) %>%
  .$year %>%
  min()

temp_carbon %>%
  filter(temp_anomaly<0.05) %>%
  .$year %>%
  max()


temp_carbon %>%
  filter(temp_anomaly>0.5) %>%
  .$year %>%
  min()

greenhouse_gases %>%
  ggplot(aes(year, concentration)) +
  geom_line() +
  facet_grid(gas~., scales = "free") +
  geom_vline(xintercept = 1850) +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")

temp_carbon %>% 
  filter(!is.na(carbon_emissions)) %>%
  ggplot(aes(year, carbon_emissions)) + 
  geom_line()

co2_time <-
  historic_co2 %>% 
    ggplot(aes(year, co2, color = source)) + 
    geom_line()


historic_co2 %>% 
  filter(year < 2018 & year > -3000) %>%
  ggplot(aes(year, co2, color = source)) + 
  geom_line()
