## explore_oi.R
## Import and clean both Opportuntiy Insights social capital data 
##  and IPUMS NHGIS racial demographic data by zip code
## Last edfited 9/20/22 by Tal Roded
##########################################################################
library(tidyverse)
library(writexl)
library(ggthemes)
library(zipcodeR)

setwd("C:/Users/trode/OneDrive/Desktop/Muth RA/chicago-history")

##########################################
## Opportunity Insights Data
##########################################
opp_ins_data <- read.csv("data/social_capital_zip.csv")

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
demographic_data <- read.csv("data/nhgis_pop_demographics_1990_2020.csv", header = T)

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
  arrange(datayear) %>%
  mutate(across(white:two_plus_races, ~ 100*(.x-.x[1])/.x[1], .names="growth_rate_{.col}"))

demographic_data_clean$datayear <- as.character(demographic_data_clean$datayear)




demographic_data_clean %>%
  filter(datayear=="2020") %>%
  ggplot() + 
  geom_bar(aes(x=reorder(zip, prop_black), y=prop_black), 
           stat='identity', position='dodge') + 
  theme_fivethirtyeight() + 
  labs(title="2020 Black Population Proportions, Chicago Zip Codes") + 
  theme(axis.text.x = element_blank(), 
        panel.grid.major.x = element_blank()) + 
  theme(plot.title = element_text(size=16, hjust=0.8)) + 
  scale_y_continuous(labels=paste0(seq(0,100, 20), "%"), 
                     breaks=seq(0,100, 20))
  

##########################################
## Merge datasets and look at spatial correlations
##########################################
# inner join Opportunity Insights data and IPUMS demographic data
chicago_combined <- chicago_data %>%
  inner_join(demographic_data_clean, by="zip")

# what is the correlation between minority proportion and economic mobility in 2020
mobility_minority_2020 <- chicago_combined %>%
  filter(datayear=="2020") %>%
  # convert number below 50th percentile variable to a proportion
  mutate(prop_below_p50=100*num_below_p50/pop2018) %>%
  # distribution of black population follows exponential shape 
  #take log of this measure
  mutate(prop_black=log(prop_black))

p <- ggplot(mobility_minority_2020, aes(x=prop_black, y=prop_below_p50)) + 
  geom_point() + 
  geom_smooth(method=lm, se=FALSE) + 
  theme_fivethirtyeight() + 
  theme(axis.title = element_text(size=12, face="bold"),
        plot.title = element_text(hjust=0.5),
        axis.text.x = element_blank()) + 
  labs(title="Mobility-Black Proportion by Zip Code", 
       x="Log(% Black)", y="% Below P50") + 
  theme(panel.border = element_rect(color="black", fill=NA, size=1, linetype="solid"))
p
ggsave("charts/income-percentile_black_scatter.png", plot = p, 
       width = 20, height = 16, units = "cm", dpi=600)

p <- ggplot(mobility_minority_2020, aes(x=prop_black, y=ec_zip)) + 
  geom_point() + 
  geom_smooth(method=lm, se=FALSE) + 
  theme_fivethirtyeight() + 
  theme(axis.title = element_text(size=12, face="bold"),
        plot.title = element_text(hjust=0.5),
        axis.text.x = element_blank()) + 
  labs(title="Mobility-Black Proportion by Zip Code", 
       x="Log(% Black)", y="Economic Connectedness") + 
  theme(panel.border = element_rect(color="black", fill=NA, size=1, linetype="solid"))
p
ggsave("charts/econ-connect_black_scatter.png", plot = p, 
       width = 20, height = 16, units = "cm", dpi=600)


p <- ggplot(mobility_minority_2020, aes(x=prop_black, y=volunteering_rate_zip)) + 
  geom_point() + 
  geom_smooth(method=lm, se=FALSE) + 
  theme_fivethirtyeight() + 
  theme(axis.title = element_text(size=12, face="bold"),
        plot.title = element_text(hjust=0.5),
        axis.text.x = element_blank()) + 
  labs(title="Mobility-Black Proportion by Zip Code", 
       x="Log(% Black)", y="Volunteering Rate") + 
  theme(panel.border = element_rect(color="black", fill=NA, size=1, linetype="solid"))
p
ggsave("charts/volunteer-rate_black_scatter.png", plot = p, 
       width = 20, height = 16, units = "cm", dpi=600)
