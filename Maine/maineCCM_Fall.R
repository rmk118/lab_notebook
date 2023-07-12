#Maine spatial CCM - Fall
#Ruby Krasnow
#Last modified: July 10, 2023

library(tidyverse)
library(lubridate)
library(plotly)
library(patchwork)
library(car)
library(rEDM)

#spatial packages
library(sf)
library(sfheaders)
library(spdep)

#import help functions and data
source('~/Downloads/lab_notebook/Maine/helpFunctionsMaine.R')

#start with most basic and add levels of spatial complexity
#First, average across all tows, regions, and strata
coarselogWtFall<- logWtFall %>% ungroup() %>% select(-c(Season, Type)) %>% group_by(Species, Year) %>% summarise(avgLogWt = mean(value))
s <- coarselogWtFall %>% filter(Species=="scallop") %>% select(Year, avgLogWt, Species)
r <- coarselogWtFall %>% filter(Species=="rock") %>% select(Year, avgLogWt, Species)
j <- coarselogWtFall %>% filter(Species=="jonah") %>% select(Year, avgLogWt, Species)
findE_v(s$avgLogWt) #7
findErho_v(s$avgLogWt) #0.57
findE_v(r$avgLogWt) #4
findErho_v(r$avgLogWt) #0.34
findE_v(j$avgLogWt) #1
findErho_v(j$avgLogWt) #0.29
s_xmap_r <- cbind(s, r) %>% select(-c(Species...3, Year...4, Species...6)) %>% mutate(scallopWt = avgLogWt...2, rockWt = avgLogWt...5, id = 1,.keep = "unused")
s_xmap_j <- cbind(s, j) %>% select(-c(Species...3, Year...4, Species...6)) %>% mutate(scallopWt = avgLogWt...2, jonahWt = avgLogWt...5, id = 1,.keep = "unused")

# cmap <- CCM(dataFrame = s_xmap_r, E = 4, Tp = 0, columns = "scallopWt",
#             target = "rockWt", libSizes = "4 20 1", sample=100, showPlot = TRUE)

ComputeError(s$avgLogWt,r$avgLogWt)
compute_stats(s$avgLogWt,r$avgLogWt)

#sig linear, non-sig EDM with E=5
do_xmap_once(s_xmap_r, predictor="scallopWt", ID_col = "id", target="rockWt", E_max=7, tp=1)
#sig linear, non-sig EDM with E=3
do_xmap_once(s_xmap_r, predictor="scallopWt", ID_col = "id", target="rockWt", E_max=7, tp=0)
#sig linear, non-sig EDM with E=1
do_xmap_once(s_xmap_r, predictor="rockWt", ID_col = "id", target="scallopWt", E_max=7, tp=0)

#EDM significant with E=2, linear non-sig
do_xmap_once(s_xmap_j, predictor="scallopWt", ID_col = "id", target="jonahWt", E_max=7, tp=1) 
#EDM significant with E=3, linear non-sig
do_xmap_once(s_xmap_j, predictor="scallopWt", ID_col = "id", target="jonahWt", E_max=7, tp=0)

#E*=1, non-sig linear and non-sig EDM with tp=1 and tp=0
do_xmap_once(s_xmap_j, predictor="jonahWt", ID_col = "id", target="scallopWt", E_max=7, tp=0) 

########################################################################################
#then do multispatial using the regions as replicates

regionslogWtFall<- catch %>% select(Season, Year, Region, Stratum, avgLogWt_s, avgLogWt_j, avgLogWt_r) %>% filter(Season == "Fall") %>% group_by(Region, Year) %>% summarise(meanLogWt_s = mean(avgLogWt_s), meanLogWt_r = mean(avgLogWt_r), meanLogWt_j = mean(avgLogWt_j))

# #both EDM and linear highly sig, E=7 (tp = 0 --> E = 6)
# do_xmap_once(regionslogWtFall, predictor="meanLogWt_s", ID_col = "Region", target="meanLogWt_r", E_max=7, tp=1)
# #sig linear but nonsig EDM with E=6 (tp = 0 --> E = 1)
# do_xmap_once(regionslogWtFall, predictor="meanLogWt_r", ID_col = "Region", target="meanLogWt_s", E_max=7, tp=1)

# #EDM highly sig with E=7 but linear non-sig.
# #If E-max changed to 15, E* is 13. p-val gets higher for EDM and lower for linear as E increases
# do_xmap_once(regionslogWtFall, predictor="meanLogWt_j", ID_col = "Region", target="meanLogWt_s", E_max=15, tp=1)

# # E* = 14, linear non-sig and EDM sig
# do_xmap_once(regionslogWtFall, predictor="meanLogWt_j", ID_col = "Region", target="meanLogWt_s", E_max=15, tp=0)


# #EDM significant when E=7 (linear non-sig) but allowing higher E-max gives E* = 8, with borderline sig (0.09)
# #If E-max changed to 15, E* is 13. p-val gets higher for EDM and lower for linear as E increases
# do_xmap_once(regionslogWtFall, predictor="meanLogWt_s", ID_col = "Region", target="meanLogWt_j", E_max=7, tp=1)

# #E* = 1, EDM non-sig with borderline sig linear (0.06)
# do_xmap_once(regionslogWtFall, predictor="meanLogWt_s", ID_col = "Region", target="meanLogWt_j", E_max=7, tp=0)

########################################################################################
#then do multispatial using strata as replicates - worse than no replication?

stratalogWtFall<- catch %>% select(Season, Year, Region, Stratum, avgLogWt_s, avgLogWt_j, avgLogWt_r) %>% 
  filter(Season == "Fall") %>% group_by(Stratum, Year) %>% 
  summarise(meanLogWt_s = mean(avgLogWt_s), meanLogWt_r = mean(avgLogWt_r), meanLogWt_j = mean(avgLogWt_j))

#Neither EDM nor linear significant with E*=4 (tp = 0 --> E* = 5, still non-sig)
# do_xmap_once(stratalogWtFall, predictor="meanLogWt_s", ID_col = "Stratum", target="meanLogWt_r", E_max=7, tp=1)
# #Neither EDM nor linear significant with E*=7 (tp = 0 --> E* = 2, still non-sig)
# #increasing E_max to 15 makes E* go to 1?
# do_xmap_once(stratalogWtFall, predictor="meanLogWt_r", ID_col = "Stratum", target="meanLogWt_s", E_max=12, tp=1)
# 
# #Neither EDM nor linear significant with E*=7 (tp = 0 --> E* = 5, still non-sig)
# # E-max 8 -> E* = 1, E-max 9 E*=7, E-max 10 E*=10, E-max 11 E*=1, E-max 12 E*=9
# do_xmap_once(stratalogWtFall, predictor="meanLogWt_j", ID_col = "Stratum", target="meanLogWt_s", E_max=7, tp=1)

# # #Neither EDM nor linear significant with E*=2 (tp = 0 --> E* = 3, still non-sig)
# do_xmap_once(stratalogWtFall, predictor="meanLogWt_s", ID_col = "Stratum", target="meanLogWt_j", E_max=7, tp=0)

########################################################################################
#multispatial using the study areas as replicates

arealogWtFall<- catch %>% select(Season, Year, Region, Stratum, avgLogWt_s, avgLogWt_j, avgLogWt_r) %>% 
  filter(Season == "Fall") %>% group_by(Stratum, Region, Year) %>% 
  summarise(meanLogWt_s = mean(avgLogWt_s), meanLogWt_r = mean(avgLogWt_r), meanLogWt_j = mean(avgLogWt_j)) 
arealogWtFall$area <- as.integer(paste0(arealogWtFall$Region, arealogWtFall$Stratum))
arealogWtFall <- arealogWtFall %>% ungroup() %>% select(-c(Region, Stratum)) %>% group_by(area) %>% select(area, Year, meanLogWt_r, meanLogWt_s,meanLogWt_j)

#Neither EDM nor linear significant with E*=6 (tp = 0 --> E* = 1, still non-sig)
do_xmap_once(arealogWtFall, predictor="meanLogWt_s", ID_col = "area", target="meanLogWt_r", E_max=7, tp=1)
#Neither EDM nor linear significant with E*=7 (tp = 0 --> E* = 1, still non-sig)
do_xmap_once(arealogWtFall, predictor="meanLogWt_r", ID_col = "area", target="meanLogWt_s", E_max=7, tp=0)

#Neither EDM nor linear sig with E*=4, but tp = 0 --> E* = 4, both EDM and linear became significant
do_xmap_once(arealogWtFall, predictor="meanLogWt_j", ID_col = "area", target="meanLogWt_s", E_max=7, tp=0)

#EDM sig with E*=5, linear non-sig. (tp = 0 --> E* = 6, both EDM and linear sig)
do_xmap_once(arealogWtFall, predictor="meanLogWt_s", ID_col = "area", target="meanLogWt_j", E_max=7, tp=1)

########################################################################################
#region 1.1 only, no spatial component
region1.1CCM <- catch %>% select(Season, Year, Region, Stratum, avgLogWt_s, avgLogWt_j, avgLogWt_r) %>% 
  filter(Season == "Fall", Stratum==1, Region==1) %>%  ungroup() %>% select(-c(Region, Season))

#Neither EDM nor linear significant with E*=3 (tp=0 --> E* = 6, EDM still non-sig but linear highly sig)
do_xmap_once(region1.1CCM, predictor="avgLogWt_s", ID_col = "Stratum", target="avgLogWt_j", E_max=7, tp=0)

#Neither EDM nor linear significant with E*=7 (tp=0 --> E* = 2, EDM still non-sig but linear highly sig)
#E-max = 8 --> E* still 7 but EDM p-val shoots up, linear becomes sig
do_xmap_once(region1.1CCM, predictor="avgLogWt_j", ID_col = "Stratum", target="avgLogWt_s", E_max=7, tp=0)

#Neither EDM nor linear significant with E*=1 (tp=0 --> E* = 4, both EDM and linear sig)
do_xmap_once(region1.1CCM, predictor="avgLogWt_r", ID_col = "Stratum", target="avgLogWt_s", E_max=7, tp=0)

#EDM borderline (0.0699) and linear sig with E*=1 (tp=0 --> E* = 7, EDM still non-sig but linear highly sig)
do_xmap_once(region1.1CCM, predictor="avgLogWt_s", ID_col = "Stratum", target="avgLogWt_r", E_max=7, tp=0)


########################################################################################
#region 1.1 CCM with bootstrapping
all_catFull <- all_catFull %>%  
  select(-c("End_Latitude","End_Longitude")) %>% 
  mutate(area = paste(Region, Stratum),.before= Survey) %>% 
  filter(Season == "Fall")

all_cat_sf<- st_as_sf(all_catFull, coords = c("Start_Longitude", "Start_Latitude"), crs=4326)
# Map of all points over grid
# ggplot() + geom_sf(data = surveyGrid) + geom_sf(data = all_cat_sf)
# ggplot(data=all_cat_sf)+geom_sf(aes(color = area))

all_region1.1points <- all_cat_sf %>% filter(area=="1 1")

########################################################################################
#all regions with bootstrapping



