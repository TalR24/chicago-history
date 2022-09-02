## explore_oi.R
## Play around with Opportuntiy Insights social capital data by zip code
## Last edfited 9/2/22 by Tal Roded
##########################################################################
library(tidyverse)
library(writexl)

setwd("C:/Users/trode/OneDrive/Desktop/Muth RA/chicago-history/data")


data <- read.csv("social_capital_zip.csv")

data$zip <- as.character(data$zip)

## keep if zip code starts with 606 and are in Cook county - our area of interest
chicago_data <- data %>%
  filter(substr(zip, 1, 3) == "606") %>%
  filter(county==17031) %>%
  mutate(year=2018)


## now bring in IPUMS NHGIS population and demographics data
demographic_data <- read.csv("nhgis_pop_demographics_1990_2020.csv", header = T)

demographic_data_clean <- demographic_data %>%
  rename_with(tolower) %>%
  filter(substr(zctaa, 1, 3) == "606") %>%
  rename(zipcode=zctaa, white=cm1aa, black=cm1ab, native=cm1ac, asian=cm1ad, 
         pacific_islander=cm1ae, other_race=cm1af, two_plus_races=cm1ag) %>%
  select(datayear, zipcode, white, black, native, asian, pacific_islander, other_race, two_plus_races) %>% 
  slice(-1) %>%
  mutate_if(is.character, as.numeric) %>%
  rowwise() %>%
  mutate(total_pop=sum(c_across(white:two_plus_races), na.rm=T)) %>%
  mutate(across(white:two_plus_races, ~ 100*.x/total_pop, .names="prop_{.col}")) %>%
  mutate(across(prop_white:prop_two_plus_races, round))

write_xlsx(demographic_data_clean, 'chicago_demographic_pops_1990_2020.xlsx')
         