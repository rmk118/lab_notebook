#ME-NH Inshore Trawl Survey
#Ruby Krasnow
#Last modified: June 29, 2023

library(tidyverse)
library(patchwork)
library(lubridate)

#spatial packages
library(sf)
library(sfheaders)

#Not very many observations
# testingStars<- read.csv("data/Maine_inshore_trawl/starCatch.csv")
# testingStars %>%  distinct(Common_Name)
# testingStars <- testingStars %>% filter(Common_Name == "Northern Sea Star" | Common_Name == "Star Common")

df_s_len<- read.csv("data/Maine_inshore_trawl/MEscallopLength.csv") #scallop length
df_s_cat<- read.csv("data/Maine_inshore_trawl/MEscallopCatch.csv") #scallop catch
df_r_len<- read.csv("data/Maine_inshore_trawl/MErockLength.csv") #rock crab length
df_r_cat<- read.csv("data/Maine_inshore_trawl/MErockCatch.csv") #rock crab catch
df_j_len<- read.csv("data/Maine_inshore_trawl/MEjonahLength.csv") #jonah crab length
df_j_cat<- read.csv("data/Maine_inshore_trawl/MEjonahCatch.csv") #jonah crab catch
df_tows<-read.csv("data/Maine_inshore_trawl/MEtows.csv") #tow data


numTows <- df_tows %>% group_by(Survey) %>% summarise(tows = n_distinct(Tow_Number))

cleanCatch <- function(x) {
    full_join(x, df_tows) %>%
    arrange(Survey, Tow_Number) %>% 
    select(-c("Stratum", "Subsample_Weight_kg_2", "Date", "Surface_WaterTemp_DegC", "Surface_Salinity", "Air_Temp")) %>%
    mutate(Number_Caught = replace_na(Number_Caught,0)) %>% 
    mutate(Stratum = Depth_Stratum, .keep="unused")
}

r_cat <- cleanCatch(df_r_cat) %>% 
  mutate(Common_Name = "Rock crab")
j_cat <- cleanCatch(df_j_cat) %>% 
  mutate(Common_Name = "Jonah crab")
s_cat <- cleanCatch(df_s_cat) %>% 
  mutate(Common_Name = "Scallop")

rm(df_j_cat, df_r_cat, df_s_cat)

summarise_all(df_r_len,n_distinct)

df_r_len <- df_r_len %>% mutate(Common_Name = "Rock crab") #note shell height is in cm
df_j_len <- df_j_len %>% mutate(Common_Name = "Jonah crab") #note shell height is in cm


df_s_len < df_s_len %>% 
  filter(Common_Name == "Scallop Sea")

  
df_s_len$Length <- replace(df_s_len$Length, df_s_len$Unit_of_Length == "CM", 10)
