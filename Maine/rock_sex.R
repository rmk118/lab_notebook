#Rock crab seasonal sex ratio differences
#Ruby Krasnow
#Last modified: Aug 8, 2023

# Load packages
library(tidyverse)
library(lubridate) #working with dates
library(sf) #for spatial data analysis/visualization

# Time series analysis
library(tseries)
library(forecast)

#Data visualization
library(patchwork)
library(ggpubr)
library(ggpmisc)

#Stats/modeling

library(car)
library(rstatix)
library(nlme)
library(broom.mixed)
library(RLRsim)
library(MASS)

# Import data
df_r_len<- read.csv("data/Maine_inshore_trawl/MErockLength.csv") #jonah crab length and sex data
df_tows<-read.csv("data/Maine_inshore_trawl/MEtows.csv") #tow data
surveyGrid <-st_read("~/Downloads/lab_notebook/Maine/MaineDMR_-_Inshore_Trawl_Survey_Grid") #CRS: WGS 84/EPSG 4326

surveyGrid <- surveyGrid %>% 
  mutate(Region = region_id,
         Stratum = depth_stra,
         GridID = grid_id, .keep="unused", .before=last_surve)

regionsGrid_orig <- surveyGrid %>% group_by(Stratum) %>% summarise(num = n_distinct(GridID))

df_r_len <- df_r_len %>%
  arrange(Season, Year, Stratum, Region, Tow_Number)

df_r_sex <- df_r_len %>% 
  filter(Year > 2003) %>% 
  dplyr::select(c("Season", "Year", "Tow_Number", "Region", "Stratum", "Frequency", "Sex")) %>% 
  arrange(Season, Year, Stratum, Region, Tow_Number)

data_r_complete <- df_r_sex %>% 
  group_by(Season, Year, Region, Stratum) %>% 
  complete(Sex, Tow_Number) #makes implicit NAs explicit by filling in all combinations of tow number and sex

data_r_complete <- data_r_complete %>% 
  group_by(Season, Year, Region, Stratum, Tow_Number, Sex) %>% 
  mutate(Num_Sex = cumsum(Frequency)) %>% #add up the number of crabs caught per tow of each sex
  slice_tail()  %>% #retrieve the total
  replace(is.na(.),0)

data_r_complete <- data_r_complete %>% filter(Sex != "Unknown") %>% 
  group_by(Season, Year, Region, Stratum, Tow_Number) %>% 
  mutate(Total = cumsum(Num_Sex)) %>% #add both sexes to find the total number of crabs caught per tow
  slice_tail() %>% 
  mutate(perc_f = ifelse(Sex=="Female", Num_Sex/Total, (1-(Num_Sex/Total))), #calculate percent female
         perc_m = ifelse(Sex == "Male", Num_Sex/Total, (1-(Num_Sex/Total)))) #calculate percent male

#find the mean proportion of female crabs for each stratum by season and year
data_r_strat <- data_r_complete %>%
  group_by(Season, Year, Stratum) %>% 
  summarise(perc_f = mean(perc_f, na.rm=TRUE)) 

data_r_complete_geom <- left_join(regionsGrid_orig, data_r_strat) #join to sf object for visualization

sex_r_diff <- pivot_wider(data_r_strat, names_from="Season", values_from = "perc_f") %>% 
  group_by(Year, Stratum) %>% 
  summarise(Fall = mean(Fall, na.rm=TRUE),
            Spring = mean(Spring, na.rm=TRUE)) %>% 
  mutate(Diff = Fall-Spring, .keep="unused") %>% #calculate the seasonal difference in perc. female from Fall to Spring
  na.omit() %>% 
  filter(Year > 2004) %>% ungroup() %>% 
  mutate(Stratum = as.numeric(Stratum))

sex_r_diff_geom <- left_join(regionsGrid_orig, sex_r_diff) #join to sf for visualization

sex_r_diff %>% 
  group_by(Stratum) %>% 
  identify_outliers(Diff) #check for extreme outliers, none found

ggplot(data=sex_r_diff, aes(x=Stratum, y=Diff, group=Stratum))+geom_violin()+theme_bw()
ggplot(data=sex_r_diff)+geom_line(aes(x=Year, y=Diff, group=Stratum, color=Stratum))

#across all years
#find the mean proportion of female crabs for each stratum by season
data_r_agg <- data_r_complete %>%
  group_by(Season, Stratum) %>% 
  summarise(perc_f = mean(perc_f, na.rm=TRUE)) 

sex_r_diff_agg <- pivot_wider(data_r_agg, names_from="Season", values_from = "perc_f") %>% 
  group_by(Stratum) %>% 
  summarise(Fall = mean(Fall, na.rm=TRUE),
            Spring = mean(Spring, na.rm=TRUE)) %>% 
  mutate(Diff = Fall-Spring, .keep="unused") %>% #calculate the seasonal difference in perc. female from Fall to Spring
  na.omit() %>% 
  ungroup() %>% 
  mutate(Stratum = as.numeric(Stratum))

# Horizontal line with R^2 < 0.004
r_squared_r <- ggplot(data=sex_r_diff_agg, aes(x=Stratum, y=Diff))+
  geom_point()+
  theme_bw()+
  stat_smooth(method="lm", se=FALSE)+
  stat_regline_equation(label.y = 0, aes(label = after_stat(eq.label))) +
  stat_regline_equation(label.y = 0.05, aes(label = after_stat(rr.label)))+
  ylim(-0.1, 0.3)

sex_r_diff_geom_agg <- left_join(regionsGrid_orig, sex_r_diff_agg) #join to sf for visualization

p1_r <- ggplot(data=sex_r_diff_geom_agg)+geom_sf(aes(fill=Diff))
p2_r <- ggplot(data=sex_r_diff_geom %>% group_by(Stratum) %>% summarise(diff = mean(Diff)))+geom_sf(aes(fill=diff))
p1_r+p2_r

r_squared_r+r_squared_j

# Regions -----------------------------------------------------------------

#find the mean proportion of female crabs for each stratum and region by season and year
data_r_area <- data_r_complete %>%
  group_by(Season, Year, Stratum, Region) %>% 
  summarise(perc_f = mean(perc_f, na.rm=TRUE)) 

sex_diff_r_reg <- pivot_wider(data_r_area, names_from="Season", values_from = "perc_f") %>% 
  group_by(Year, Region, Stratum) %>% 
  summarise(Fall = mean(Fall, na.rm=TRUE),
            Spring = mean(Spring, na.rm=TRUE)) %>% 
  mutate(Diff = Fall-Spring, .keep="unused") %>% #calculate the seasonal diff in perc. female, Fall-Spring
  na.omit() %>% 
  filter(Year > 2004) %>% ungroup() %>% 
  mutate(Stratum = as.numeric(Stratum))

sex_diff_r_geom_reg <- left_join(regionsGrid_reg, sex_diff_r_reg) #join to sf for visualization

ggplot(data=sex_diff_r_geom_reg)+geom_sf(aes(fill=Diff))

#find the mean proportion of female crabs for each stratum and region by season and year
data_r_area <- data_r_complete %>%
  group_by(Season, Year, Stratum, Region) %>% 
  summarise(perc_f = mean(perc_f, na.rm=TRUE)) 

sex_diff_r_reg <- pivot_wider(data_r_area, names_from="Season", values_from = "perc_f") %>% 
  group_by(Year, Region, Stratum) %>% 
  summarise(Fall = mean(Fall, na.rm=TRUE),
            Spring = mean(Spring, na.rm=TRUE)) %>% 
  mutate(Diff = Fall-Spring, .keep="unused") %>% #calculate the seasonal diff in perc. female, Fall-Spring
  na.omit() %>% 
  filter(Year > 2004) %>% ungroup() %>% 
  mutate(Stratum = as.numeric(Stratum))

sex_diff_r_geom_reg <- left_join(regionsGrid_reg, sex_diff_r_reg) #join to sf for visualization

ggplot(data=sex_diff_r_geom_reg)+geom_sf(aes(fill=Diff))


#find the mean proportion of female crabs for each stratum and region - all years
data_r_area_all <- data_r_complete %>%
  group_by(Season, Stratum, Region) %>% 
  summarise(perc_f = mean(perc_f, na.rm=TRUE)) 

sex_diff_r_reg_all <- pivot_wider(data_r_area_all, names_from="Season", values_from = "perc_f") %>% 
  group_by(Region, Stratum) %>% 
  summarise(Fall = mean(Fall, na.rm=TRUE),
            Spring = mean(Spring, na.rm=TRUE)) %>% 
  mutate(Diff = Fall-Spring, .keep="unused") %>% #calculate the seasonal diff in perc. female, Fall-Spring
  na.omit() %>% 
  ungroup() %>% 
  mutate(Stratum = as.numeric(Stratum))

sex_diff_r_geom_reg_all <- left_join(regionsGrid_reg, sex_diff_r_reg_all) #join to sf for visualization

ggplot(data=sex_diff_r_geom_reg_all)+geom_sf(aes(fill=Diff))
ggplot(data=sex_diff_r_geom_reg_all %>% filter(Stratum==1))+geom_sf(aes(fill=Diff))
