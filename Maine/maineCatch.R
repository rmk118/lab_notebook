#ME-NH Inshore Trawl Survey - Catch/Weight Analysis
#Ruby Krasnow
#Last modified: July 13, 2023

library(tidyverse)
library(patchwork)
library(lubridate)
library(tseries)

#spatial packages
library(sf)
library(sfheaders)

library(rEDM)

source('~/Downloads/lab_notebook/Maine/helpFunctionsMaine.R')
df_s_cat<- read.csv("data/Maine_inshore_trawl/MEscallopCatch.csv") #scallop catch
df_r_cat<- read.csv("data/Maine_inshore_trawl/MErockCatch.csv") #rock crab catch
df_j_cat<- read.csv("data/Maine_inshore_trawl/MEjonahCatch.csv") #jonah crab catch
df_tows<-read.csv("data/Maine_inshore_trawl/MEtows.csv") #tow data

numTows <- df_tows %>% group_by(Survey) %>% summarise(tows = n_distinct(Tow_Number))

s_cat_Spatial <- cleanCatch(df_s_cat) %>% 
  mutate(Common_Name = "Scallop")  %>% 
  filter(Season == "Fall")

r_cat_Spatial <- cleanCatch(df_r_cat) %>% 
  mutate(Common_Name = "Rock") %>%
  filter(Season == "Fall")

j_cat_Spatial <- cleanCatch(df_j_cat) %>% 
  mutate(Common_Name = "Jonah") %>%
  filter(Season == "Fall")

rm(df_j_cat, df_r_cat, df_s_cat)

colOrder<-c("area", "Survey", "Tow_Number", "Region", "Stratum", "logCatch", "logWt", "Expanded_Catch", 
  "Expanded_Weight_kg", "Date", "Common_Name", "Number_Caught", "Weight_kg",
  "Start_Latitude", "Start_Longitude","Season",
  "Year","Grid", "Start_Depth_fathoms", "End_Depth_fathoms",
  "Bottom_WaterTemp_DegC", "Bottom_Salinity")

j_cat_Spatial <- j_cat_Spatial %>% select(all_of(colOrder))
r_cat_Spatial <- r_cat_Spatial %>% select(all_of(colOrder))
s_cat_Spatial <- s_cat_Spatial %>% select(all_of(colOrder))

j_cat <- summaryCatch(j_cat_Spatial)
r_cat <- summaryCatch(r_cat_Spatial)
s_cat <- summaryCatch(s_cat_Spatial)

catch <- s_cat %>% left_join(j_cat, by=c("area", "Season", "Region", "Stratum", "Year"), suffix = c("_s", "_j"))
catch <- catch %>% left_join(r_cat, by=c("area", "Season", "Region", "Stratum", "Year")) %>% 
  mutate(avgCatch_r = avgCatch,avgWt_r = avgWt,avgLogCatch_r = avgLogCatch,avgLogWt_r = avgLogWt, .keep="unused")

# ggplot(catch, aes(x=logS_catch,y=logJ_catch, color=Year))+geom_point()+theme_classic()+geom_smooth(method = "lm")+labs(x="scallops", y="jonah crabs")+facet_grid(Region~Stratum) #+ scale_color_gradient(colours = rainbow(30))
# ggplot(catch, aes(x=logS_catch,y=logR_catch, color=Year))+geom_point()+theme_classic()+labs(x="scallops", y="rock crabs")+geom_smooth(method = "lm")+facet_grid(Region~Stratum)
# ggplot(catch, aes(x=logJ_catch,y=logR_catch, color=Year))+geom_point()+theme_classic()+labs(x="jonah crabs", y="rock crabs")
# ggplot(catch, aes(x=logS_catch,y=logJ_catch, color=Year))+geom_point()+theme_classic()+labs(x="scallops", y="jonah crabs")+facet_wrap(~Stratum)

catchTidy <- pivot_longer(catch, 
        cols = 6:ncol(catch)) %>% 
  mutate(Type = case_when(
    startsWith(name, "avgCatch_") ~"catch",
    startsWith(name,"avgWt_") ~"wt",
    startsWith(name,"avgLogWt") ~"logWt",
    startsWith(name,"avgLogCatch") ~"logCatch")) %>% 
  mutate(Species = case_when(
    endsWith(name, "s") ~"scallop",
    endsWith(name, "r") ~"rock",
    endsWith(name, "j") ~"jonah"))

catchTidy <- catchTidy %>% mutate(Species = as.factor(Species),Season = as.factor(Season),Region = as.factor(Region), Stratum = as.factor(Stratum)) %>% 
          select(-name)

# catchFall <- catchTidy %>% filter(Type == "catch") %>% filter(Season == "Fall")
# wtFall <- catchTidy %>% filter(Type == "wt") %>% filter(Season == "Fall")
logCatchFall <- catchTidy %>% filter(Type == "logCatch") %>% filter(Season == "Fall")
logWtFall <- catchTidy %>% filter(Type == "logWt") %>% filter(Season == "Fall")

# catchSpring <- catchTidy %>% filter(Type == "avgCatch") %>% filter(Season == "Spring")
# wtSpring <- catchTidy %>% filter(Type == "avgWt") %>% filter(Season == "Spring")
# logCatchSpring <- catchTidy %>% filter(Type == "avgLogCatch") %>% filter(Season == "Spring")
# logWtSpring <- catchTidy %>% filter(Type == "avgLogWt") %>% filter(Season == "Spring")

# ggplot(logCatchSpring, aes(x=Year,y=value, color=Species))+geom_line()+theme_classic()+labs(x="Time", y="Log catch")+facet_grid(Region~Stratum)
# ggplot(logWtSpring, aes(x=Year,y=value, color=Species))+geom_line()+theme_classic()+labs(x="Time", y="Log weight")+facet_grid(Region~Stratum)

ggplot(logCatchFall, aes(x=Year,y=value, color=Species))+geom_line()+theme_classic()+labs(x="Time", y="Log catch")+facet_grid(Region~Stratum)

ggplot(logWtFall, aes(x=Year,y=value, color=Species))+geom_line()+theme_classic()+labs(x="Time", y="Log weight")+facet_grid(Region~Stratum)



# log weight --------------------------------------------------------------
# ggplot(scalLogWtFall, aes(x=Year, y=logWeight, group = Region, color=Region))+geom_line()+facet_grid(~Stratum)
# ggplot(scalLogWtSpring, aes(x=Year, y=logWeight, group = Region, color=Region))+geom_line()+facet_grid(~Stratum)
# ggplot(scalLogCatchSpring, aes(x=Year, y=logCatch, group = Region, color=Region))+geom_line()+facet_grid(~Stratum)
# ggplot(scalLogCatchFall, aes(x=Year, y=logCatch, group = Region, color=Region))+geom_line()+facet_grid(~Stratum)

# kpss.test(logWtFall %>% filter(Species=="rock", Region==1, Stratum==4) %>% pull(value), null='Level')
# summary(logWtFall %>% filter(Species=="rock", Region==1, Stratum==4) %>% pull(value))
# 
# 
# suppressWarnings(findSpeciesKPSS(logWtFall %>% filter(Species=="scallop"), season="Fall", type="logWt"))
# suppressWarnings(findSpeciesKPSS(logCatchFall %>% filter(Species=="scallop"), season="Fall", type="logCatch"))
# 
# findSpeciesKPSS(logWtFall %>% filter(Species=="rock"), season="Fall", type="logWt")
# 
# findSpeciesKPSS(logWtFall %>% filter(Species=="jonah"), season="Fall", type="logWt")
