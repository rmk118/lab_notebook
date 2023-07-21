#Maine spatial analysis - Fall
#Ruby Krasnow
#Last modified: July 21, 2023

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
df_tows<-read.csv("data/Maine_inshore_trawl/MEtows.csv") #tow data
df_s_cat<- read.csv("data/Maine_inshore_trawl/MEscallopCatch.csv") #scallop catch
df_r_cat<- read.csv("data/Maine_inshore_trawl/MErockCatch.csv") #rock crab catch
df_j_cat<- read.csv("data/Maine_inshore_trawl/MEjonahCatch.csv") #jonah crab catch

surveyGrid <-st_read("~/Downloads/lab_notebook/Maine/MaineDMR_-_Inshore_Trawl_Survey_Grid") #CRS: WGS 84/EPSG 4326

surveyGrid <- surveyGrid %>% 
  mutate(Region = region_id,
         Stratum = depth_stra,
         GridID = grid_id, .keep="unused", .before=last_surve)

surveyGrid$region_stratum <- paste(surveyGrid$Region, surveyGrid$Stratum)

# plot(surveyGrid["GridID"], main="Grid ID")
# plot(surveyGrid["OBJECTID"], main="Object ID")
# plot(surveyGrid["Region"], main="Region")
# plot(surveyGrid["Stratum"], main="Depth Stratum")
# plot(surveyGrid["region_stratum"], main="Study Area")

# surveyGrid %>% group_by(Region, Stratum) %>% summarise(num = n_distinct(GridID)) #number of grids in each study area

# surveyed <- surveyGrid %>% filter(!is.na(surveys)) #only accurate through 2019
# plot(surveyed["region_stratum"], main="Study Area")
# plot(surveyed["Region"], main="Region")

s_cat_clean <- cleanCatch(df_s_cat) %>% 
  mutate(Common_Name = "Scallop") %>%
  filter(Season == "Fall")

r_cat_clean <- cleanCatch(df_r_cat) %>% 
  mutate(Common_Name = "Rock") %>% 
  filter(Season == "Fall")

j_cat_clean <- cleanCatch(df_j_cat) %>% 
  mutate(Common_Name = "Jonah") %>% 
  filter(Season == "Fall")

# calculate the average number of tows per area over the whole time series
# there are a few years where there is one more tow for scallops than for the crab species
numTows<- (s_cat_clean %>% group_by(area) %>% summarise(num = n_distinct(row_number())))
numTows_fromDF<- df_tows %>% filter(Season == "Fall") %>% group_by(Region, Depth_Stratum, Year) %>% summarise(num = n_distinct(Tow_Number))
numTowsCrabs<- (r_cat_clean %>% group_by(area) %>% summarise(num = n_distinct(row_number())))
# calculate the average number of tows per area per year
yearlyTows <- s_cat_clean %>% group_by(area, Year, Season) %>% summarise(num = n_distinct(row_number()))
hist(yearlyTows$num)
max(yearlyTows$num)
ggplot(yearlyTows, aes(x=num)) + geom_histogram(bins=10)+theme_classic()+
  labs(title="Number of tows per area per year (Fall ME-NH trawl survey)",x="Tows per area", y = "Count")

s_cat_sf<- st_as_sf(s_cat_clean, coords = c("Start_Longitude", "Start_Latitude"), crs=4326)
r_cat_sf<- st_as_sf(r_cat_clean, coords = c("Start_Longitude", "Start_Latitude"), crs=4326)
j_cat_sf<- st_as_sf(j_cat_clean, coords = c("Start_Longitude", "Start_Latitude"), crs=4326)

# Map of all points over grid
# ggplot() + geom_sf(data = surveyGrid) + geom_sf(data = s_cat_sf) +facet_wrap(~Year)
# 
# ggplot(data=s_cat_sf)+geom_sf(aes(color = area))
# 
# ggplot()+ geom_sf(data = (surveyGrid  %>% filter(region_stratum == "1 1")))+geom_sf(data=(s_cat_sf %>% filter(area == "1 1")), aes(color = area))
# 
# ggplot()+ geom_sf(data = (surveyGrid  %>% filter(region_stratum == "1 1")))+geom_sf(data=(s_cat_sf %>% filter(area == "1 1")), aes(color = Year))


################# Region 1, Stratum 1 - example -----------------------------------------------------

#all grids merged together
ggplot(st_union(surveyGrid, by_feature = FALSE) %>% st_sf()) + geom_sf()

regionsGrid <- surveyGrid %>% group_by(region_stratum) %>% summarise(num = n_distinct(GridID))
plot(regionsGrid[1])

#find the grids and points in region 1.1
region1.1grid<- surveyGrid %>% filter(region_stratum=="1 1") %>% st_union(by_feature = FALSE) #merged together
region1.1points <- s_cat_sf %>% filter(area=="1 1")

#find all points not in region 1.1
not1.1points <- s_cat_sf %>% filter(area!="1 1")

# This plot shows all points not in 1.1 over the entire grid (all areas)
ggplot() + geom_sf(data = surveyGrid) + geom_sf(data = not1.1points)

# This plot shows all points marked as being in 1.1 over the region of merged 1.1 grids
marked<- ggplot()+ geom_sf(data=region1.1grid)+geom_sf(data = region1.1points)+ggtitle("Labeled as 1.1")

inside<- ggplot()+ geom_sf(data=region1.1grid)+geom_sf(data=st_intersection(region1.1grid, s_cat_sf))+ggtitle("Start coords within 1.1")

marked+inside

# Use st_length(surveyGrid) and st_length(s_cat_sf) to check that units are m

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

# 20 iterations (20 random samples of n neighbors)
# where n is 0.5*(tows completed in that area over the time series)

#trial 1 - so we're not binding to an empty df
trialStrat <- sample_n(neighbors_1.1, nNeighbors, replace = FALSE) #randomly sample n points from the neighbors
#create "Frankenstein stratum" by combining base stratum with the random sample of neighbors
trialStrat <- bind_rows(region1.1points, trialStrat) %>% mutate(trial = 1, .before=area) 

#trials 2-20
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
              avgWt = mean(Expanded_Weight_kg),
              avgLogCatch = mean(logCatch),
              avgLogWt = mean(logWt)) 
}

eTest <- summaryCatchTrial(trialStrat)

colOrderTrials<-c("trial", "Year", "geometry", "avgCatch", "avgWt", "avgLogCatch", "avgLogWt")
eTest <- eTest %>% select(all_of(colOrderTrials))
            
eTest <- pivot_longer(eTest, cols = 4:ncol(eTest)) %>%
  mutate(Type = case_when(
    name== "avgCatch" ~"avgCatch",
    name=="avgWt" ~"avgWt",
    name=="avgLogWt" ~"avgLogWt",
    name=="avgLogCatch" ~"avgLogCatch")) %>%
  mutate(Species = as.factor("scallop"),
          trial = as.factor(trial)) %>% 
   select(-name)

#findE_v(eTest %>% filter(Type=="avgLogCatch", trial==1) %>% pull(value))

v <- c()
r <- c()

par(mfrow=c(5,4), mar=c(0.6,1,0.4,0.5))

for (i in 1:20) {
  rho_E<- EmbedDimension(dataFrame = (eTest %>% filter(Type=="avgLogWt", trial==i) %>% st_drop_geometry() %>% select(Year, value)), lib = "1 23", pred = "1 23", columns = "value",target = "value", maxE = 10)
  v<-append(v,(rho_E[which.max(rho_E$rho),"E"][1]))
  r<-append(r,(rho_E[which.max(rho_E$rho),"rho"][1]))
  }

print(round(mean(v)))
print(r)
summary(r)
summary(v)
sd(v)


# Generalizing to all areas - only Scallops 10 trials-----------------------------------------------

numRegions = 5
numStrata = 4

allAreasE <- data.frame(matrix(nrow = 5, ncol=4))
allAreasRho <-data.frame(matrix(nrow = 5, ncol=4))
LBtest <-data.frame(matrix(nrow = 5, ncol=4))

for (i in 1:numRegions) {
  for (j in 1:numStrata) {

    baseArea <- paste(i, j)

    areaGrid<- surveyGrid %>% filter(region_stratum==baseArea) %>% st_union(by_feature = FALSE) #merged together
    areaPoints <- s_cat_sf %>% filter(area==baseArea)

    #find all points not in the base area
    notAreaPoints <- s_cat_sf %>% filter(area!=baseArea)

    #which of those points are close to the base area
    neighborsArea <- st_intersection(notAreaPoints, st_buffer(areaGrid, 3704))

    #how many neighboring points are there
    print(baseArea)
    print(nrow(neighborsArea))

    #how many neighbors will be used in the bootstrapping
    nNeighbors <- round(0.5*(numTows %>% filter(area==baseArea) %>% pull(num)))
    #print(numTows %>% filter(area==baseArea) %>% pull(num))
    #print(nNeighbors)

    #trial 1 - so we aren't binding rows to an empty dataframe
    trialStrat <- sample_n(neighborsArea, nNeighbors, replace = FALSE)
    trialStrat <- bind_rows(areaPoints, trialStrat) %>% mutate(trial = 1, .before=area)
    #trials 2 through 10
    for (k in 2:10) {
      tempStrat <- sample_n(neighborsArea, nNeighbors, replace = FALSE)
      tempStrat <- bind_rows(areaPoints, tempStrat) %>% mutate(trial = k, .before=area)
      trialStrat<- bind_rows(trialStrat, tempStrat)
    }
    
    trialSum <- summaryCatchTrial(trialStrat) %>% select(all_of(colOrderTrials))
    
    trialSum <- pivot_longer(trialSum, cols = 4:ncol(trialSum)) %>%
      mutate(Type = case_when(
        name== "avgCatch" ~"avgCatch",
        name=="avgWt" ~"avgWt",
        name=="avgLogWt" ~"avgLogWt",
        name=="avgLogCatch" ~"avgLogCatch")) %>%
      mutate(Species = as.factor("scallop")) %>%
      select(-name)
    
    v <- c()
    r <- c()
    
    for (x in 1:10) {
      df <- as.data.frame(trialSum %>% filter(Type=="avgLogWt", trial == x) %>% 
                           st_drop_geometry() %>% 
                          select(Year, value))
      lib_pred_vec <- paste(1,nrow(df))
      rho_E<- EmbedDimension(dataFrame = df, lib = lib_pred_vec, pred = lib_pred_vec, columns = "value",target = "value", maxE = 10)
      v<-append(v,(rho_E[which.max(rho_E$rho),"E"][1]))
      r<-append(r,(rho_E[which.max(rho_E$rho),"rho"][1]))
    }
    
    print(round(mean(v)))
    print(mean(r))
    
    allAreasE[i, j]<- round(mean(v))
    allAreasRho[i,j]<- mean(r)
    LBtest[i,j]<-Box.test(scalLogCatchFall_1.1$value, lag=5, type="Ljung-Box")
    
  }
}

allAreasE
allAreasRho

noBoot<- findSpeciesErho(s_catchTidy, season="Fall", type="avgLogWt")
rhoDifference<- allAreasRho-noBoot
mean(as.matrix(noBoot))
sd(as.matrix(noBoot))
mean(as.matrix(allAreasRho))
sd(as.matrix(allAreasRho))

boxplot(c(as.matrix(noBoot)), c(as.matrix(allAreasRho)))
t.test(c(as.matrix(noBoot)), c(as.matrix(allAreasRho)), paired = TRUE, alternative = "less")

mean(as.matrix(rhoDifference))

shapiro.test(c(as.matrix(rhoDifference)))
boxplot(c(as.matrix(rhoDifference)))
hist(c(as.matrix(rhoDifference)), breaks=7)

noBootVals<- c(as.matrix(noBoot))
allAreasVals <- c(as.matrix(allAreasRho))
wilcox.test(noBootVals, allAreasVals, alternative = "less")


# # 20 trials-----------------------------------------------
# 
# allAreasE_20 <- data.frame(matrix(nrow = 5, ncol=4))
# allAreasRho_20 <-data.frame(matrix(nrow = 5, ncol=4))
# 
# allAreasE_50 <- data.frame(matrix(nrow = 5, ncol=4))
# allAreasRho_50 <-data.frame(matrix(nrow = 5, ncol=4))
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
#     #trial 1 - so we aren't binding rows to an empty dataframe
#     trialStrat <- sample_n(neighborsArea, nNeighbors, replace = FALSE)
#     trialStrat <- bind_rows(areaPoints, trialStrat) %>% mutate(trial = 1, .before=area)
#     #trials 2 through 10
#     for (k in 2:50) {
#       tempStrat <- sample_n(neighborsArea, nNeighbors, replace = FALSE)
#       tempStrat <- bind_rows(areaPoints, tempStrat) %>% mutate(trial = k, .before=area)
#       trialStrat<- bind_rows(trialStrat, tempStrat)
#     }
#     
#     trialSum <- summaryCatchTrial(trialStrat) %>% select(all_of(colOrderTrials))
#     
#     trialSum <- pivot_longer(trialSum, cols = 4:ncol(trialSum)) %>%
#       mutate(Type = case_when(
#         name== "avgCatch" ~"avgCatch",
#         name=="avgWt" ~"avgWt",
#         name=="avgLogWt" ~"avgLogWt",
#         name=="avgLogCatch" ~"avgLogCatch")) %>%
#       mutate(Species = as.factor("scallop")) %>%
#       select(-name)
#     
#     v <- c()
#     r <- c()
#     
#     for (x in 1:50) {
#       df <- as.data.frame(trialSum %>% filter(Type=="avgLogWt", trial == x) %>% 
#                             st_drop_geometry() %>% 
#                             select(Year, value))
#       lib_pred_vec <- paste(1,nrow(df))
#       rho_E<- EmbedDimension(dataFrame = df, lib = lib_pred_vec, pred = lib_pred_vec, columns = "value",target = "value", maxE = 10)
#       v<-append(v,(rho_E[which.max(rho_E$rho),"E"][1]))
#       r<-append(r,(rho_E[which.max(rho_E$rho),"rho"][1]))
#     }
#     
#     print(round(mean(v)))
#     print(mean(r))
#     
#     allAreasE_50[i, j]<- round(mean(v))
#     allAreasRho_50[i,j]<- mean(r)
#     
#   }
# }

allAreasE_20
allAreasRho_20

allAreasE_50
allAreasRho_50

allAreasE
allAreasRho

mean(as.matrix(allAreasE_20-allAreasE))
(as.matrix(allAreasE_20-noBootE))

noBootE<- findSpeciesE(s_catchTidy, season="Fall", type="avgLogWt")
noBoot<- findSpeciesErho(s_catchTidy, season="Fall", type="avgLogWt")
rhoDifference<- allAreasRho-noBoot
mean(as.matrix(noBoot))
sd(as.matrix(noBoot))
mean(as.matrix(allAreasRho))
sd(as.matrix(allAreasRho))

boxplot(c(as.matrix(noBoot)), c(as.matrix(allAreasRho)))
#good figure to show how bootstrapping gives a cleaner picture of the same dynamics
boxplot(c(as.matrix(noBootE)), c(as.matrix(allAreasE_20))) 
t.test(c(as.matrix(noBoot)), c(as.matrix(allAreasRho)), paired = TRUE, alternative = "less")

mean(as.matrix(rhoDifference))

shapiro.test(c(as.matrix(rhoDifference)))
boxplot(c(as.matrix(rhoDifference)))
hist(c(as.matrix(rhoDifference)), breaks=7)

noBootVals<- c(as.matrix(noBoot))
allAreasVals <- c(as.matrix(allAreasRho))
wilcox.test(noBootVals, allAreasVals, alternative = "less")

mean(as.matrix(allAreasRho_20-noBoot))
mean(as.matrix(allAreasRho_50-noBoot))

shapiro.test(c(as.matrix(allAreasRho_20-noBoot)))
t.test(c(as.matrix(noBoot)), c(as.matrix(allAreasRho_20)), paired = TRUE, alternative = "less") #p=0.156


# Rock Crabs! ------------------------------------------------------------------


rockE <- data.frame(matrix(nrow = 5, ncol=4))
rockRho <-data.frame(matrix(nrow = 5, ncol=4))

rockE_20 <- data.frame(matrix(nrow = 5, ncol=4))
rockRho_20 <-data.frame(matrix(nrow = 5, ncol=4))

for (i in 1:numRegions) {
  for (j in 1:numStrata) {
    
    baseArea <- paste(i, j)
    
    areaGrid<- surveyGrid %>% filter(region_stratum==baseArea) %>% st_union(by_feature = FALSE) #merged together
    areaPoints <- r_cat_sf %>% filter(area==baseArea)
    
    #find all points not in the base area
    notAreaPoints <- r_cat_sf %>% filter(area!=baseArea)
    
    #which of those points are close to the base area
    neighborsArea <- st_intersection(notAreaPoints, st_buffer(areaGrid, 3704))
    
    #how many neighboring points are there
   # print("study area")
    print(baseArea)
    #print("number of points within 2 tows")
    print(nrow(neighborsArea))
    
    #how many neighbors will be used in the bootstrapping
    nNeighbors <- round(0.5*(numTowsCrabs %>% filter(area==baseArea) %>% pull(num)))
    
    # print("number of tows in this region")
    # print(numTowsCrabs %>% filter(area==baseArea) %>% pull(num))
    # print("number of neighbors used in bootstrapping")
    # print(nNeighbors)
    
    #trial 1 - so we aren't binding rows to an empty dataframe
    trialStrat <- sample_n(neighborsArea, nNeighbors, replace = FALSE)
    trialStrat <- bind_rows(areaPoints, trialStrat) %>% mutate(trial = 1, .before=area)
    #trials 2 through 10
    for (k in 2:20) {
      tempStrat <- sample_n(neighborsArea, nNeighbors, replace = FALSE)
      tempStrat <- bind_rows(areaPoints, tempStrat) %>% mutate(trial = k, .before=area)
      trialStrat<- bind_rows(trialStrat, tempStrat)
    }
    
    trialSum <- summaryCatchTrial(trialStrat) %>% select(all_of(colOrderTrials))
    
    trialSum <- pivot_longer(trialSum, cols = 4:ncol(trialSum)) %>%
      mutate(Type = case_when(
        name== "avgCatch" ~"avgCatch",
        name=="avgWt" ~"avgWt",
        name=="avgLogWt" ~"avgLogWt",
        name=="avgLogCatch" ~"avgLogCatch")) %>%
      mutate(Species = as.factor("rock")) %>%
      select(-name)
    
    v <- c()
    r <- c()
    
    for (x in 1:20) {
      df <- as.data.frame(trialSum %>% filter(Type=="avgLogWt", trial == x) %>% 
                            st_drop_geometry() %>% 
                            select(Year, value))
      lib_pred_vec <- paste(1,nrow(df))
      rho_E<- EmbedDimension(dataFrame = df, lib = lib_pred_vec, pred = lib_pred_vec, columns = "value",target = "value", maxE = 10)
      v<-append(v,(rho_E[which.max(rho_E$rho),"E"][1]))
      r<-append(r,(rho_E[which.max(rho_E$rho),"rho"][1]))
    }
    
    #print(round(mean(v)))
    #print(mean(r))
    
   # rockE_20[i, j]<- round(mean(v))
    #rockRho_20[i,j]<- mean(r)
    
  }
}

rockE_20
rockRho_20

mean(as.matrix(rockRho_20-rockRho))

rockE
rockRho

mean(as.matrix(rockRho))
mean(as.matrix(allAreasRho-rockRho))

shapiro.test(c(as.matrix(allAreasRho-rockRho))) #p-value = 0.3154
shapiro.test(c(as.matrix(allAreasRho))) #p-value = 0.002958
shapiro.test(c(as.matrix(rockRho))) #p-value = 0.7743

t.test(c(as.matrix(allAreasRho)), c(as.matrix(rockRho))) #p=0.001

wilcox.test(c(as.matrix(allAreasRho)), c(as.matrix(rockRho))) #p=0.0008
wilcox.test(c(as.matrix(allAreasRho)), c(as.matrix(rockRho)), paired=TRUE) #p=0.0005

t.test(c(as.matrix(allAreasRho)), c(as.matrix(rockRho)), paired=TRUE, alternative = "greater") #p-value = 0.0003


# Jonah Crabs! ------------------------------------------------------------------


jonahE <- data.frame(matrix(nrow = 5, ncol=4))
jonahRho <-data.frame(matrix(nrow = 5, ncol=4))

jonahE_20 <- data.frame(matrix(nrow = 5, ncol=4))
jonahRho_20 <-data.frame(matrix(nrow = 5, ncol=4))

for (i in 1:numRegions) {
  for (j in 1:numStrata) {
    
    baseArea <- paste(i, j)
    
    areaGrid<- surveyGrid %>% filter(region_stratum==baseArea) %>% st_union(by_feature = FALSE) #merged together
    areaPoints <- j_cat_sf %>% filter(area==baseArea)
    
    #find all points not in the base area
    notAreaPoints <- j_cat_sf %>% filter(area!=baseArea)
    
    #which of those points are close to the base area
    neighborsArea <- st_intersection(notAreaPoints, st_buffer(areaGrid, 3704))
    
    #how many neighboring points are there
    print("study area")
    print(baseArea)
    print("number of points within 2 tows")
    print(nrow(neighborsArea))
    
    #how many neighbors will be used in the bootstrapping
    nNeighbors <- round(0.5*(numTowsCrabs %>% filter(area==baseArea) %>% pull(num)))
    
    # print("number of tows in this region")
    # print(numTowsCrabs %>% filter(area==baseArea) %>% pull(num))
    # print("number of neighbors used in bootstrapping")
    # print(nNeighbors)
    
    #trial 1 - so we aren't binding rows to an empty dataframe
    trialStrat <- sample_n(neighborsArea, nNeighbors, replace = FALSE)
    trialStrat <- bind_rows(areaPoints, trialStrat) %>% mutate(trial = 1, .before=area)
    #trials 2 through 10
    for (k in 2:20) {
      tempStrat <- sample_n(neighborsArea, nNeighbors, replace = FALSE)
      tempStrat <- bind_rows(areaPoints, tempStrat) %>% mutate(trial = k, .before=area)
      trialStrat<- bind_rows(trialStrat, tempStrat)
    }
    
    trialSum <- summaryCatchTrial(trialStrat) %>% select(all_of(colOrderTrials))
    
    trialSum <- pivot_longer(trialSum, cols = 4:ncol(trialSum)) %>%
      mutate(Type = case_when(
        name== "avgCatch" ~"avgCatch",
        name=="avgWt" ~"avgWt",
        name=="avgLogWt" ~"avgLogWt",
        name=="avgLogCatch" ~"avgLogCatch")) %>%
      mutate(Species = as.factor("jonah")) %>%
      select(-name)
    
    v <- c()
    r <- c()
    
    for (x in 1:10) {
      df <- as.data.frame(trialSum %>% filter(Type=="avgLogWt", trial == x) %>% 
                            st_drop_geometry() %>% 
                            select(Year, value))
      lib_pred_vec <- paste(1,nrow(df))
      rho_E<- EmbedDimension(dataFrame = df, lib = lib_pred_vec, pred = lib_pred_vec, columns = "value",target = "value", maxE = 10)
      v<-append(v,(rho_E[which.max(rho_E$rho),"E"][1]))
      r<-append(r,(rho_E[which.max(rho_E$rho),"rho"][1]))
    }
    
    print(round(mean(v)))
    print(mean(r))
    
    jonahE[i, j]<- round(mean(v))
    jonahRho[i,j]<- mean(r)
    
  }
}

jonahE
jonahRho


jonahE_20
jonahRho_20

mean(as.matrix(rockRho_20-rockRho))
mean(as.matrix(jonahE))
mean(as.matrix(rockE))
mean(as.matrix(jonahRho))
mean(as.matrix(allAreasRho-jonahRho))


shapiro.test(c(as.matrix(allAreasRho-jonahRho))) #p-value = 0.03232

shapiro.test(c(as.matrix(jonahRho))) #p-value = 0.337

shapiro.test(c(as.matrix(rockRho-jonahRho))) #p-value = 0.973

wilcox.test(c(as.matrix(allAreasRho)), c(as.matrix(jonahRho)), paired=TRUE) #p=0.398, p-value = 0.2943 if paired
wilcox.test(c(as.matrix(rockRho)), c(as.matrix(jonahRho)), paired=TRUE) #p=0.01, 0.026 if paired

t.test(c(as.matrix(jonahRho)), c(as.matrix(rockRho))) #p=0.009
t.test(c(as.matrix(jonahRho)), c(as.matrix(rockRho)), paired=TRUE) #p-value = 0.01908

shapiro.test(c(as.matrix(rockE-jonahE))) #p-value = 0.335
t.test(c(as.matrix(jonahE)), c(as.matrix(rockE)), paired=TRUE) #p-value = 0.472

E_all3 <- data.frame(scallop=c(as.matrix(allAreasE)),
                     rock = c(as.matrix(rockE)),
                     jonah = c(as.matrix(jonahE))) %>%  
  pivot_longer(cols = everything(), names_to = "species", values_to = "E") %>% 
  arrange(species)

leveneTest(E_all3$E ~ E_all3$species)

res_aovE <- aov(E ~ species, data = E_all3)
summary(res_aovE)
par(mfrow = c(1, 2)) # combine plots

# histogram
hist(res_aov$residuals) 

# QQ-plot
qqPlot(res_aov$residuals,
       id = FALSE # id = FALSE to remove point identification
)
shapiro.test(res_aovE$residuals) #p-value = 0.4066

Rho_all3 <- data.frame(scallop=c(as.matrix(allAreasRho)),
                     rock = c(as.matrix(rockRho)),
                     jonah = c(as.matrix(jonahRho))) %>%  
  pivot_longer(cols = everything(), names_to = "species", values_to = "rho") %>% 
  arrange(species)

res_aovRho <- aov(rho ~ species, data = Rho_all3)
summary(res_aovRho)
par(mfrow = c(1, 2)) # combine plots

# histogram
hist(res_aovRho$residuals)

# QQ-plot
qqPlot(res_aovRho$residuals,
       id = FALSE # id = FALSE to remove point identification
)
shapiro.test(res_aovRho$residuals) #p-value = 0.118

TukeyHSD(res_aovRho)
