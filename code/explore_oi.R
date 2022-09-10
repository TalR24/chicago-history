## explore_oi.R
## Import and clean both Opportuntiy Insights social capital data 
##  and IPUMS NHGIS racial demographic data by zip code
## Last edfited 9/2/22 by Tal Roded
##########################################################################
library(tidyverse)
library(writexl)
library(ggthemes)

setwd("C:/Users/trode/OneDrive/Desktop/Muth RA/chicago-history/data")

##########################################
## Opportunity Insights Data
##########################################
opp_ins_data <- read.csv("social_capital_zip.csv")

opp_ins_data$zip <- as.character(opp_ins_data$zip)

## keep if zip code starts with 606 and are in Cook county - our area of interest
chicago_data <- opp_ins_data %>%
  filter(substr(zip, 1, 3) == "606") %>%
  filter(county==17031) %>%
  mutate(year=2018)

##########################################
## IPUMS NHGIS Demographic 1990-2020 Data
##########################################
## now bring in IPUMS NHGIS population and demographics data
demographic_data <- read.csv("nhgis_pop_demographics_1990_2020.csv", header = T)

demographic_data_clean <- demographic_data %>%
  rename_with(tolower) %>%
  filter(substr(zctaa, 1, 3) == "606") %>%
  rename(zip=zctaa, white=cm1aa, black=cm1ab, native=cm1ac, asian=cm1ad, 
         pacific_islander=cm1ae, other_race=cm1af, two_plus_races=cm1ag) %>%
  select(datayear, zip, white, black, native, asian, pacific_islander, other_race, two_plus_races) %>% 
  slice(-1) %>%
  mutate_if(is.character, as.numeric) %>%
  rowwise() %>%
  mutate(total_pop=sum(c_across(white:two_plus_races), na.rm=T)) %>%
  mutate(across(white:two_plus_races, ~ 100*.x/total_pop, .names="prop_{.col}")) %>%
  mutate(across(prop_white:prop_two_plus_races, round))

demographic_data_clean$zip <- as.character(demographic_data_clean$zip)

# export to excel to share with RA team
#write_xlsx(demographic_data_clean, 'chicago_demographic_pops_1990_2020.xlsx')

dem_trends <- demographic_data_clean %>%
  group_by(zip) %>%
  sort(datayear) %>%
  mutate(across(white:two_plus_races, ~ 100*(.x-.x[1])/.x[1], .names="growth_rate_{.col}"))

demographic_data_clean$datayear <- as.character(demographic_data_clean$datayear)
demographic_data_clean %>%
  filter(datayear=="2020") %>%
  ggplot() + 
  geom_bar(aes(x=reorder(zip, prop_black), y=prop_black), 
           stat='identity', position='dodge') + 
  theme_fivet

##########################################
## Merge datasets and look at spatial correlations
##########################################
chicago_combined <- chicago_data %>%
  inner_join(demographic_data_clean, by="zip")
