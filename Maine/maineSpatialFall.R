#Maine spatial analysis - Fall
#Ruby Krasnow
#Last modified: July 7, 2023

#spatial packages
library(sf)
library(sfheaders)
library(plotly)
library(spdep)
library(tidyverse)
library(lubridate)

#import help functions and tow data
source('~/Downloads/lab_notebook/Maine/helpFunctionsMaine.R')
df_tows<-read.csv("data/Maine_inshore_trawl/MEtows.csv") #tow data

#grid_id starts again at 1 for each region
#st_layers("~/Downloads/lab_notebook/Maine/MaineDMR_-_Inshore_Trawl_Survey_Grid")
surveyGrid <-st_read("~/Downloads/lab_notebook/Maine/MaineDMR_-_Inshore_Trawl_Survey_Grid")
#CRS: WGS 84/EPSG 4326

surveyGrid <- surveyGrid %>% 
  mutate(Region = region_id,
         Stratum = depth_stra,
         GridID = grid_id, .keep="unused", .before=last_surve)

surveyGrid$region_stratum <- paste(surveyGrid$Region, surveyGrid$Stratum)

#plot(surveyGrid["GridID"], main="Grid ID")
#plot(surveyGrid["OBJECTID"], main="Object ID")
#plot(surveyGrid["Region"], main="Region")
#plot(surveyGrid["Stratum"], main="Depth Stratum")
plot(surveyGrid["region_stratum"], main="Study Area")

surveyGrid %>% group_by(Region, Stratum) %>% summarise(num = n_distinct(GridID))

surveyed <- surveyGrid %>% filter(!is.na(surveys))
plot(surveyed["region_stratum"], main="Study Area")
plot(surveyed["Region"], main="Region")


df_s_cat<- read.csv("data/Maine_inshore_trawl/MEscallopCatch.csv") #scallop catch
df_r_cat<- read.csv("data/Maine_inshore_trawl/MErockCatch.csv") #rock crab catch
df_j_cat<- read.csv("data/Maine_inshore_trawl/MEjonahCatch.csv") #jonah crab catch
df_tows<-read.csv("data/Maine_inshore_trawl/MEtows.csv") #tow data

s_cat_Spatial <- cleanCatch(df_s_cat) %>% 
  mutate(Common_Name = "Scallop") %>% 
  select(-c("End_Latitude","End_Longitude")) %>% 
  mutate(area = paste(Region, Stratum),.before= Survey) %>% 
  filter(Season == "Fall")

r_cat_Spatial <- cleanCatch(df_r_cat) %>% 
  mutate(Common_Name = "Rock") %>% 
  select(-c("End_Latitude","End_Longitude")) %>% 
  mutate(area = paste(Region, Stratum),.before= Survey) %>% 
  filter(Season == "Fall")

j_cat_Spatial <- cleanCatch(df_j_cat) %>% 
  mutate(Common_Name = "Jonah") %>% 
  select(-c("End_Latitude","End_Longitude")) %>% 
  mutate(area = paste(Region, Stratum),.before= Survey) %>% 
  filter(Season == "Fall")

# calculate the average number of tows per area over the whole time series
numTows<- (s_cat_Spatial %>% group_by(area) %>% summarise(num = n_distinct(row_number())))

s_cat_sf<- st_as_sf(s_cat_Spatial, coords = c("Start_Longitude", "Start_Latitude"), crs=4326)
r_cat_sf<- st_as_sf(r_cat_Spatial, coords = c("Start_Longitude", "Start_Latitude"), crs=4326)
j_cat_sf<- st_as_sf(j_cat_Spatial, coords = c("Start_Longitude", "Start_Latitude"), crs=4326)

# Map of all points over grid
#ggplot() + geom_sf(data = surveyGrid) + geom_sf(data = s_cat_sf)

# ggplot(data=s_cat_sf)+geom_sf(aes(color = area))


# Region 1, Stratum 1 -----------------------------------------------------

#all grids merged together
ggplot(st_union(surveyGrid, by_feature = FALSE) %>% st_sf()) + geom_sf()

#find the grids and points in region 1.1
region1.1grid<- surveyGrid %>% filter(region_stratum=="1 1") %>% st_union(by_feature = FALSE) #merged together
region1.1points <- s_cat_sf %>% filter(area=="1 1")

#find all points not in region 1.1
not1.1points <- s_cat_sf %>% filter(area!="1 1")

# This plot shows all points not in 1.1 over the entire grid (all areas)
ggplot() + geom_sf(data = surveyGrid) + geom_sf(data = not1.1points)

# This plot shows all points marked as being in 1.1 over the region of merged 1.1 grids
ggplot()+ geom_sf(data=region1.1grid)+geom_sf(data = region1.1points)

# Use st_length(surveyGrid) and st_length(s_cat_sf) to check that units are m
# This plot adds a 1000m buffer around each point
#ggplot()+ geom_sf(data=region1.1grid)+geom_sf(data = st_buffer(region1.1points, 1000)) 

# find all of points NOT marked as being in region 1.1 that are located within 2 tows length 
# (2 nautical miles = 3704 meters) of region 1.1 
neighbors_1.1 <- st_intersection(not1.1points, st_buffer(region1.1grid, 3704))

# This plot shows the neighboring points in the context of the entire study region
ggplot()+geom_sf(data=st_union(surveyGrid, by_feature = FALSE))+geom_sf(data = neighbors_1.1)

# This plot shows the neighboring points over region 1.1 (with grid boundaries) and the buffer around 1.1 - best visual
ggplot()+geom_sf(data = st_buffer(region1.1grid, 3704))+ 
  geom_sf(data=surveyGrid %>% filter(region_stratum=="1 1"))+ 
  geom_sf(data = neighbors_1.1)

#total number of neighbors within 2 tows
nrow(neighbors_1.1) #102

#how many neighbors will be used in the bootstrapping
nNeighbors <- round(0.5*(numTows %>% filter(area=="1 1") %>% pull(num)))

# Start with 10 iterations (10 random samples of n neighbors), ideally would do 100 iterations
# where n is 0.5*(tows completed in that area over the time series)

trialStrat <- sample_n(neighbors_1.1, nNeighbors, replace = FALSE)
trialStrat <- bind_rows(region1.1points, trialStrat) %>% mutate(trial = 1, .before=area)
for (i in 2:20) {
  tempStrat <- sample_n(neighbors_1.1, nNeighbors, replace = FALSE)
  tempStrat <- bind_rows(region1.1points, tempStrat) %>% mutate(trial = i, .before=area)
  trialStrat<- bind_rows(trialStrat, tempStrat)
}

# meanTest <- trialStrat %>% group_by(trial, area) %>% 
#   summarise(avgCatch = mean(Expanded_Catch),
#             avgWt = mean(Expanded_Weight_kg))

summaryCatchTrial <- function(df) {
  df %>% group_by(trial,Year) %>%
    summarise(avgCatch = mean(Expanded_Catch),
              avgWt = mean(Expanded_Weight_kg))
}


eTest <- summaryCatchTrial(trialStrat)
eTest$logCatch <- log(eTest$avgCatch+1)
eTest$logWt <- log(eTest$avgWt+1)


colOrderTrials<-c("trial", "Season", "Year", "geometry", "avgCatch", "avgWt", "logCatch", "logWt")
eTest <- eTest %>% select(all_of(colOrderTrials))
            
eTest <- pivot_longer(eTest, cols = 5:ncol(eTest)) %>%
  mutate(Type = case_when(
    name== "avgCatch" ~"avgCatch",
    name=="avgWt" ~"avgWt",
    name=="logWt" ~"logWt",
    name=="logCatch" ~"logCatch")) %>%
  mutate(Species = as.factor("scallop"),
          Season = as.factor(Season),
          trial = as.factor(trial)) %>% 
   select(-name)

findE_v(eTest %>% filter(Type=="logCatch", trial==1) %>% pull(value))

v <- c()
r <- c()

#par(mfrow=c(5,4), mar=c(0.6,1,0.4,0.5))

for (i in 1:20) {
  rho_E<- EmbedDimension(dataFrame = (eTest %>% filter(Type=="logCatch", trial==i) %>% st_drop_geometry() %>% select(Year, value)), lib = "1 23", pred = "1 23", columns = "value",target = "value", maxE = 6)
  v<-append(v,(rho_E[which.max(rho_E$rho),"E"][1]))
  r<-append(r,(rho_E[which.max(rho_E$rho),"rho"][1]))
  }

round(mean(v))
print(r)
summary(r)
summary(v)
sd(v)


# Generalizing to all areas - only Scallops -----------------------------------------------

# numRegions = 5
# numStrata = 4
# 
# for (i in 1:numRegions) {
#   for (j in 1:numStrata) {
#     
#     baseArea <- paste(i, j)
#     
#     areaGrid<- surveyGrid %>% filter(region_stratum==baseArea) %>% st_union(by_feature = FALSE) #merged together
#     areaPoints <- s_cat_sf %>% filter(area==baseArea)
#     
#     #find all points not in the base area
#     notAreaPoints <- s_cat_sf %>% filter(area!=baseArea)
#     
#     #which of those points are close to the base area
#     neighborsArea <- st_intersection(notAreaPoints, st_buffer(areaGrid, 3704))
#     
#     #how many neighboring points are there
#     print(baseArea)
#     print(nrow(neighborsArea))
#     
#     #how many neighbors will be used in the bootstrapping
#     nNeighbors <- round(0.5*(numTows %>% filter(area==baseArea) %>% pull(num)))
#     #print(numTows %>% filter(area==baseArea) %>% pull(num))
#     #print(nNeighbors)
#     
#     # #trial 1 - so we aren't binding rows to an empty dataframe
#     # trialStrat <- sample_n(neighborsArea, nNeighbors, replace = FALSE)
#     # trialStrat <- bind_rows(areaPoints, trialStrat) %>% mutate(trial = 1, .before=area)
#     # #trials 2 through 10
#     # for (k in 2:10) {
#     #   tempStrat <- sample_n(neighborsArea, nNeighbors, replace = FALSE)
#     #   tempStrat <- bind_rows(areaPoints, tempStrat) %>% mutate(trial = k, .before=area)
#     #   trialStrat<- bind_rows(trialStrat, tempStrat)
#     # }
#   }
# }
# 
# 
