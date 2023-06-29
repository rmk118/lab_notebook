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
