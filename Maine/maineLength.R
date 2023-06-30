#Maine inshore trawl survey - length data
#Ruby Krasnow
#Last modified: June 30, 2023

library(tidyverse)
library(patchwork)
library(lubridate)

#spatial packages
library(sf)
library(sfheaders)

df_s_len<- read.csv("data/Maine_inshore_trawl/MEscallopLength.csv") #scallop length
df_r_len<- read.csv("data/Maine_inshore_trawl/MErockLength.csv") #rock crab length
df_j_len<- read.csv("data/Maine_inshore_trawl/MEjonahLength.csv") #jonah crab length

df_tows<-read.csv("data/Maine_inshore_trawl/MEtows.csv") #tow data

numTows <- df_tows %>% group_by(Survey) %>% summarise(tows = n_distinct(Tow_Number))

df_r_len <- df_r_len %>% mutate(Common_Name = "Rock crab") #note shell height is in cm
df_j_len <- df_j_len %>% mutate(Common_Name = "Jonah crab") #note shell height is in cm

df_s_len <- df_s_len %>% 
  filter(Common_Name == "Scallop Sea") %>% 
  mutate(Length = replace(Length, Unit_of_Length == "CM", Length*10)) 
#shell height was measured in cm until 2005, then switched to MM

df_j_len <- df_j_len %>% 
  arrange(Season, Year, Stratum, Region, Tow_Number)

#Mean length per tow by region and stratum
j_len <- df_j_len %>% 
  group_by(Season, Region, Stratum, Tow_Number) %>% 
  summarise(numPerTow = n_distinct(Length))
