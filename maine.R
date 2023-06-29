#ME-NH Inshore Trawl Survey
#Ruby Krasnow
#Last modified: June 29, 2023

library(tidyverse)
library(patchwork)
library(lubridate)

#spatial packages
library(sf)
library(sfheaders)

df_s_len<- read.csv("data/Maine_inshore_trawl/MEscallopLength.csv")
df_s_cat<- read.csv("data/Maine_inshore_trawl/MEscallopCatch.csv")
df_r_len<- read.csv("data/Maine_inshore_trawl/MErockLength.csv")
df_r_cat<- read.csv("data/Maine_inshore_trawl/MErockCatch.csv")
df_j_len<- read.csv("data/Maine_inshore_trawl/MEjonahLength.csv")
df_j_cat<- read.csv("data/Maine_inshore_trawl/MEjonahCatch.csv")
df_tows<-read.csv("data/Maine_inshore_trawl/MEtows.csv")


numTows <- df_tows %>% group_by(Survey) %>% summarise(tows = n_distinct(Tow_Number))

cleanCatch <- function(x) {
    full_join(x, df_tows) %>%
    arrange(Survey, Tow_Number) %>% 
    select(-c("Stratum", "Subsample_Weight_kg_2", "Date")) %>%
    mutate(Number_Caught = replace_na(Number_Caught,0))
}

r_cat <- cleanCatch(df_r_cat) %>% 
  mutate(Common_Name = "Rock crab")
j_cat <- cleanCatch(df_j_cat) %>% 
  mutate(Common_Name = "Jonah crab")
s_cat <- cleanCatch(df_s_cat) %>% 
  mutate(Common_Name = "Scallop")
