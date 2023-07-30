#Maine inshore trawl survey - length data
#Ruby Krasnow
#Last modified: June 30, 2023

library(tidyverse)
library(patchwork)
library(lubridate)

#spatial packages
library(sf)
library(sfheaders)

#df_s_len<- read.csv("data/Maine_inshore_trawl/MEscallopLength.csv") #scallop length
#df_r_len<- read.csv("data/Maine_inshore_trawl/MErockLength.csv") #rock crab length
df_j_len<- read.csv("data/Maine_inshore_trawl/MEjonahLength.csv") #jonah crab length

df_tows<-read.csv("data/Maine_inshore_trawl/MEtows.csv") #tow data

numTows <- df_tows %>% group_by(Survey) %>% summarise(tows = n_distinct(Tow_Number))

#df_r_len <- df_r_len %>% mutate(Common_Name = "Rock crab") #note shell height is in cm
df_j_len <- df_j_len %>% mutate(Common_Name = "Jonah crab") #note shell height is in cm

# df_s_len <- df_s_len %>% 
#   filter(Common_Name == "Scallop Sea") %>% 
#   mutate(Length = replace(Length, Unit_of_Length == "CM", Length*10)) %>% 
#   mutate(Unit_of_Length = recode(Unit_of_Length, "CM"="MM"))
#shell height was measured in cm until 2005, then switched to MM
# 
# df_j_len <- df_j_len %>% 
#   arrange(Season, Year, Stratum, Region, Tow_Number)
# 
# #Mean length per tow by region and stratum - multiply by frequency!!
# j_len <- df_j_len %>% 
#   group_by(Season, Year, Region, Stratum) %>% 
#   summarise(Length = mean(Length)) %>% mutate(area = as.numeric(paste0(Region, Stratum)))
# 
# j_len <- left_join(regionsGrid_orig, j_len)

df_sex <- df_j_len %>% filter(Year > 2003) %>% select(Season, Year, Tow_Number, Region, Stratum, Frequency, Sex) %>% mutate(Area = as.numeric(paste0(Region, Stratum))) %>% 
     arrange(Season, Year, Stratum, Region, Tow_Number)

testRes <- df_sex %>% group_by(Season, Year, Area, Tow_Number, Sex) %>% mutate(Num_Sex = cumsum(Frequency)) %>% slice_tail()

testRes2 <- df_sex %>% 
  group_by(Season, Year, Region, Stratum) %>% 
  complete(Sex, Tow_Number) 

testRes2 <- testRes2 %>% 
  group_by(Season, Year, Region, Stratum, Tow_Number, Sex) %>% 
  mutate(Num_Sex = cumsum(Frequency)) %>% 
  slice_tail()  %>% 
  replace(is.na(.),0)

testRes2 <- testRes2 %>% filter(Sex != "Unknown") %>% 
  group_by(Season, Year, Region, Stratum, Tow_Number) %>% 
  mutate(Total = cumsum(Num_Sex)) %>% 
  slice_tail() %>% 
  mutate(perc_f = ifelse(Sex=="Female", Num_Sex/Total, (1-(Num_Sex/Total))),
         perc_m = ifelse(Sex == "Male", Num_Sex/Total, (1-(Num_Sex/Total))))

testRes2 <- testRes2 %>% group_by(Season, Year, Region, Stratum) %>% summarise(perc_m = mean(perc_m)) %>% 
  mutate(Area = as.numeric(paste0(Region, Stratum)))

testRes2_geom <- left_join(regionsGrid_orig %>% mutate(Area = area, .keep="unused"), testRes2)

ggplot(data=testRes2_geom %>% filter(Season=="Fall"))+geom_sf(aes(fill=perc_m))+facet_wrap(~Year)

sex_diff <- pivot_wider(testRes2, names_from="Season", values_from = "perc_m") %>% mutate(Diff = Fall-Spring) %>% na.omit() %>% group_by(Year, Stratum) %>% summarise(Diff = mean(Diff)) %>% filter(Year > 2004)

sex_diff_geom <- left_join(regionsGrid_orig %>% mutate(Stratum = as.numeric(substr(area, 2,2))), sex_diff)

ggplot(data=sex_diff_geom)+geom_sf(aes(fill=Diff))+facet_wrap(~Year)

#sex_diff %>% filter(Stratum==1)
#shapiro.test(sex_diff %>% filter(Stratum==1) %>% pull(Diff))
#t.test(sex_diff %>% filter(Stratum==1) %>% pull(Diff))

#sex_diff %>% filter(Stratum==1)
#shapiro.test(sex_diff %>% filter(Stratum==1) %>% pull(Diff))
#t.test(sex_diff %>% filter(Stratum==1) %>% pull(Diff))

summary(aov(Diff ~ Stratum, data=sex_diff))

library(rstatix)
library(ggpubr)
#Build the linear model
model  <- lm(Diff ~ Stratum, data = sex_diff)
model
par(mfrow = c(2, 2))
plot(model)
plot(model,5)
summary(model)

durbinWatsonTest(model)
ncvTest(model)
# Create a QQ plot of residuals
ggqqplot(residuals(model))

sex_diff %>% group_by(Stratum) %>% identify_outliers(Diff)

sex_diff <- sex_diff %>% filter(!(Year==2019 & Stratum==3)) %>% 
  mutate(Stratum = as.factor(Stratum)) %>% arrange(Stratum)#remove extreme outlier

shapiro.test(residuals(model))


sex_diff %>%
  group_by(Stratum) %>%
  shapiro_test(Diff) 

library(car)
ggqqplot(sex_diff, "Diff", facet.by = "Stratum")
plot(model, 3)

sex_diff %>% ungroup() %>% levene_test(Diff ~ Stratum)
leveneTest(Diff ~ Stratum, data=sex_diff)

welch_anova_test(data=sex_diff %>% ungroup(), Diff ~ Stratum)
games_howell_test(data=sex_diff %>% ungroup(), Diff ~ Stratum)

par(mfrow = c(2, 2))
plot(lm(Diff ~ Year + Stratum, data=sex_diff))

summary(lm(Diff ~ Year + Stratum, data=sex_diff))

library(lme4)
#library(nlme)

lmer(Diff ~ (1|Year) + Stratum, data=sex_diff)
summary(lmer(Diff ~ (1|Year) + Stratum, data=sex_diff))
ggplot(data=sex_diff)
  
  
 summary(anova(lmer(Diff ~ (1|Year) + Stratum, data=sex_diff))) 
  
  plot(lmer(Diff ~ (1|Year) + Stratum, data=sex_diff))
  