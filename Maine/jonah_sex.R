#Jonah crab seasonal sex ratio differences
#Ruby Krasnow
#Last modified: July 31, 2023

# Load packages
library(tidyverse)
library(rstatix)
library(ggpubr)
library(car)
library(sf)
library(sandwich)
library(lme4)
library(lmerTest)
library(lmtest)
library(tseries)
library(forecast)

# Import data
df_j_len<- read.csv("data/Maine_inshore_trawl/MEjonahLength.csv") #jonah crab length and sex data
df_tows<-read.csv("data/Maine_inshore_trawl/MEtows.csv") #tow data
surveyGrid <-st_read("~/Downloads/lab_notebook/Maine/MaineDMR_-_Inshore_Trawl_Survey_Grid") #CRS: WGS 84/EPSG 4326

surveyGrid <- surveyGrid %>% 
  mutate(Region = region_id,
         Stratum = depth_stra,
         GridID = grid_id, .keep="unused", .before=last_surve)

regionsGrid_orig <- surveyGrid %>% group_by(Stratum) %>% summarise(num = n_distinct(GridID))

df_j_len <- df_j_len %>%
  arrange(Season, Year, Stratum, Region, Tow_Number)

df_sex <- df_j_len %>% filter(Year > 2003) %>% 
  select(Season, Year, Tow_Number, Region, Stratum, Frequency, Sex) %>% 
  arrange(Season, Year, Stratum, Region, Tow_Number)

data_complete <- df_sex %>% 
  group_by(Season, Year, Region, Stratum) %>% 
  complete(Sex, Tow_Number) #makes implicit NAs explicit by filling in all combinations of tow number and sex

data_complete <- data_complete %>% 
  group_by(Season, Year, Region, Stratum, Tow_Number, Sex) %>% 
  mutate(Num_Sex = cumsum(Frequency)) %>% #add up the number of crabs caught per tow of each sex
  slice_tail()  %>% #retrieve the total
  replace(is.na(.),0)

data_complete <- data_complete %>% filter(Sex != "Unknown") %>% 
  group_by(Season, Year, Region, Stratum, Tow_Number) %>% 
  mutate(Total = cumsum(Num_Sex)) %>% #add both sexes to find the total number of crabs caught per tow
  slice_tail() %>% 
  mutate(perc_f = ifelse(Sex=="Female", Num_Sex/Total, (1-(Num_Sex/Total))), #calculate percent female
         perc_m = ifelse(Sex == "Male", Num_Sex/Total, (1-(Num_Sex/Total)))) #calculate percent male

#find the mean proportion of female crabs for each stratum by season and year
data_strat <- data_complete %>%
  group_by(Season, Year, Stratum) %>% 
  summarise(perc_f = mean(perc_f, na.rm=TRUE)) 

data_complete_geom <- left_join(regionsGrid_orig, data_strat) #join to sf object for visualization


sex_diff <- pivot_wider(data_strat, names_from="Season", values_from = "perc_f") %>% 
  group_by(Year, Stratum) %>% 
  summarise(Fall = mean(Fall, na.rm=TRUE),
            Spring = mean(Spring, na.rm=TRUE)) %>% 
  mutate(Diff = Fall-Spring, .keep="unused") %>% #calculate the seasonal difference in perc. female from Fall to Spring
  na.omit() %>% 
  filter(Year > 2004) %>% ungroup() %>% 
  mutate(Stratum = as.numeric(Stratum))

sex_diff_geom <- left_join(regionsGrid_orig, sex_diff) #join to sf for visualization

sex_diff %>% 
  group_by(Stratum) %>% 
  identify_outliers(Diff) #check for extreme outliers, none found

sex_diff %>%
  group_by(Stratum) %>%
  shapiro_test(Diff) #no significant deviations from normality

tibble(sex_diff) #preview data

# Ordinary least squares linear regression
lm <- lm(Diff ~ Stratum, data=sex_diff)
summary(lm)
par(mfrow = c(2, 2))
plot(lm)
shapiro.test(lm$residuals) #good
durbinWatsonTest(lm, max.lag=15) #good

bptest(lm) #p<0.05, will use robust standard errors
lm_results <- coeftest(lm, vcov = vcovHC(lm, type = 'HC0'))
lm_results

# Year is not significant when added as an explanatory variable
lm_year <- lm(Diff ~ Stratum + Year, data=sex_diff)
coeftest(lm_year, vcov = vcovHC(lm_year, type = 'HC0'))


# Figures -----------------------------------------------------------------

ggplot(data=sex_diff)+geom_line(aes(x=Year, y=Diff, group=Stratum, color=Stratum))

#across all years
allYears <- sex_diff_geom %>% group_by(Stratum) %>% summarize(Diff = mean(Diff))
ggplot(data=allYears)+geom_sf(aes(fill=Diff))

ggplot(data=sex_diff, aes(x=Stratum, y=Diff, group=Stratum))+geom_violin()+theme_bw()

ggplot(data=sex_diff %>% group_by(Stratum) %>% summarise(Diff=mean(Diff)), aes(x=Stratum, y=Diff))+geom_point()+theme_bw()+ stat_smooth(method="lm", se=FALSE)+
  stat_regline_equation(label.y = 0, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = -0.05, aes(label = ..rr.label..))





