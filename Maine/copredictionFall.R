#Coprediction Analysis
#Ruby Krasnow
#Last modified: July 15, 2023

#Load packages
library(tidyverse)
library(lubridate) #date formatting
library(patchwork) #combining plots
# library(tseries) #for KPSS test for stationarity
library(rEDM) #EDM

#spatial packages
# library(sf)
# library(sfheaders)
# library(spdep)

#import help functions and data
source('~/Downloads/lab_notebook/Maine/helpFunctionsMaine.R')
df_tows<-read.csv("data/Maine_inshore_trawl/MEtows.csv") #tow data
df_s_cat<- read.csv("data/Maine_inshore_trawl/MEscallopCatch.csv") #scallop catch
df_r_cat<- read.csv("data/Maine_inshore_trawl/MErockCatch.csv") #rock crab catch
df_j_cat<- read.csv("data/Maine_inshore_trawl/MEjonahCatch.csv") #jonah crab catch

s_cat_clean <- cleanCatch(df_s_cat) %>% 
  mutate(Common_Name = "Scallop") %>%
  filter(Season == "Fall")

r_cat_clean <- cleanCatch(df_r_cat) %>% 
  mutate(Common_Name = "Rock") %>% 
  filter(Season == "Fall")

j_cat_clean <- cleanCatch(df_j_cat) %>% 
  mutate(Common_Name = "Jonah") %>% 
  filter(Season == "Fall")

#Reorder columns
colOrder<-c("area", "Survey", "Tow_Number", "Region", "Stratum", "logCatch", "logWt", "Expanded_Catch", 
            "Expanded_Weight_kg", "Date", "Common_Name", "Number_Caught", "Weight_kg",
            "Start_Latitude", "Start_Longitude","Season",
            "Year","Grid", "Start_Depth_fathoms", "End_Depth_fathoms",
            "Bottom_WaterTemp_DegC", "Bottom_Salinity")

j_cat_clean <- j_cat_clean %>% select(all_of(colOrder))
r_cat_clean <- r_cat_clean %>% select(all_of(colOrder))
s_cat_clean <- s_cat_clean %>% select(all_of(colOrder))

#computes averages for each study area (area = region-stratum combination)
j_cat_sum <- summaryCatch(j_cat_clean)
r_cat_sum <- summaryCatch(r_cat_clean)
s_cat_sum <- summaryCatch(s_cat_clean)

catch <- s_cat_sum %>% left_join(j_cat_sum, by=c("area", "Season", "Region", "Stratum", "Year"), suffix = c("_s", "_j"))
catch <- catch %>% left_join(r_cat_sum, by=c("area", "Season", "Region", "Stratum", "Year")) %>% 
  mutate(avgCatch_r = avgCatch,avgWt_r = avgWt,avgLogCatch_r = avgLogCatch,avgLogWt_r = avgLogWt, .keep="unused")

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

catchTidy <- catchTidy %>% mutate(area = as.factor(area), Species = as.factor(Species),Season = as.factor(Season),Region = as.factor(Region), Stratum = as.factor(Stratum)) %>% 
  select(-name)

logCatchFall <- catchTidy %>% filter(Type == "logCatch") %>% filter(Season == "Fall")
logWtFall <- catchTidy %>% filter(Type == "logWt") %>% filter(Season == "Fall")


# Areas -------------------------------------------------------------------

#Scallops

#Catch
findSpeciesE((catchTidy %>% filter(Species=="scallop")), season="Fall", type="logCatch")
findSpeciesErho((catchTidy %>% filter(Species=="scallop")), season="Fall", type="logCatch")
findSpeciesTheta((catchTidy %>% filter(Species=="scallop")), season="Fall", type="logCatch",
                 df_Es = findSpeciesE((catchTidy %>% filter(Species=="scallop")), season="Fall", type="logCatch"))
findSpeciesTheta_rho((catchTidy %>% filter(Species=="scallop")), season="Fall", type="logCatch",
                 df_Es = findSpeciesE((catchTidy %>% filter(Species=="scallop")), season="Fall", type="logCatch"))
findSpeciesKPSS(catchTidy %>% filter(Species=="scallop"), season="Fall", type="logCatch")

testxmap2 <- make_xmap_block(df=(logCatchFall %>% filter(Species=="scallop")), predictor=value, 
                              target=value, ID_col=area, E_max=6, cause_lag=1)

make_xmap_block(df=(logCatchFall %>% filter(Species=="scallop") %>% ungroup() %>% select(area, Year, value)), predictor=value,
                target=value, ID_col=area, E_max=6, cause_lag=1)

do_xmap_once(df=(logCatchFallInt %>% filter(Species=="scallop") %>% ungroup() %>% select(areaInt, Year, value)), predictor="value", target="value", ID_col="areaInt", E_max=6, tp=1)

#Weight
findSpeciesE((catchTidy %>% filter(Species=="scallop")), season="Fall", type="logWt")
findSpeciesErho((catchTidy %>% filter(Species=="scallop")), season="Fall", type="logWt")
findSpeciesTheta((catchTidy %>% filter(Species=="scallop")), season="Fall", type="logWt",
                 df_Es = findSpeciesE((catchTidy %>% filter(Species=="scallop")), season="Fall", type="logWt"))

findSpeciesTheta_rho((catchTidy %>% filter(Species=="scallop")), season="Fall", type="logWt",
                     df_Es = findSpeciesE((catchTidy %>% filter(Species=="scallop")), season="Fall", type="logWt"))
findSpeciesKPSS(catchTidy %>% filter(Species=="scallop"), season="Fall", type="logWt")

logCatchFallInt <- logCatchFall %>% mutate(areaInt = as.integer(paste0(Region, Stratum)))
logWtFallInt <- logWtFall %>% mutate(areaInt = as.integer(paste0(Region, Stratum)))
do_xmap_once(df=(logWtFallInt %>% filter(Species=="scallop") %>% ungroup() %>% select(areaInt, Year, value)), predictor="value", target="value", ID_col="areaInt", E_max=6, tp=1)



