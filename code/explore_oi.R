<<<<<<< HEAD
## explore_oi.R
## Play around with Opportuntiy Insights social capital data by zip code
## Last edfited 8/24/22 by Tal Roded
##########################################################################
library(tidyverse)

setwd("C:/Users/trode/OneDrive/Desktop/Muth RA/chicago-history/data")


data <- read.csv("social_capital_zip.csv")

data$zip <- as.character(data$zip)

## keep if zip code starts with 606 and are in Cook county - our area of interest
chicago_data <- data %>%
  filter(substr(zip, 1, 3) == "606") %>%
  filter(county==17031) %>%
  mutate(year=2018)


## now bring in IPUMS NHGIS population and demographics data
demographic_data <- read.csv("nhgis_pop_demographics.csv")

demographic_data$ZCTAA <- as.character(demographic_data$ZCTAA)

demographic_data <- demographic_data %>%
  rename_with(tolower) %>%
  filter(substr(zctaa, 1, 3) == "606") %>%
  filter(datayear==2000 | datayear==2010) %>%
  rename(zipcode=zctaa, total_pop=cl8aa, white=cn3aa, black=cn3ab, native=cn3ac, asian=cn3ad, 
         pacific_islander=cn3ae, other_race=cn3af, two_plus_races=cn3ag) %>%
  select(datayear, zipcode, total_pop, white, black, native, asian, pacific_islander, other_race, two_plus_races)
=======
## explore_oi.R
## Play around with Opportuntiy Insights social capital data by zip code
## Last edfited 8/24/22 by Tal Roded
##########################################################################
library(tidyverse)

setwd("C:/Users/trode/OneDrive/Desktop/Muth RA/chicago-history/data")


data <- read.csv("social_capital_zip.csv")

data$zip <- as.character(data$zip)

## keep if zip code starts with 606 and are in Cook county - our area of interest
chicago_data <- data %>%
  filter(substr(zip, 1, 3) == "606") %>%
  filter(county==17031) %>%
  mutate(year=2018)


## now bring in IPUMS NHGIS population and demographics data
demographic_data <- read.csv("nhgis_pop_demographics.csv")

demographic_data$ZCTAA <- as.character(demographic_data$ZCTAA)

demographic_data <- demographic_data %>%
  rename_with(tolower) %>%
  filter(substr(zctaa, 1, 3) == "606") %>%
  filter(datayear==2000 | datayear==2010) %>%
  rename(zipcode=zctaa, total_pop=cl8aa, white=cn3aa, black=cn3ab, native=cn3ac, asian=cn3ad, 
         pacific_islander=cn3ae, other_race=cn3af, two_plus_races=cn3ag) %>%
  select(datayear, zipcode, total_pop, white, black, native, asian, pacific_islander, other_race, two_plus_races)
>>>>>>> 3ef8dc21ff63ab20757d6c367693f13418e4c7e0
