## explore_oi.R

setwd("C:/Users/trode/Downloads")

library(tidyverse)

data <- read.csv("social_capital_zip.csv")

data$zip <- as.character(data$zip)

## keep if zip code starts with 606 and are in Cook county
chicago_data <- data %>%
  filter(substr(data$zip, 1, 3) == "606") %>%
  filter(county==17031) %>%
  mutate(year=2018)

