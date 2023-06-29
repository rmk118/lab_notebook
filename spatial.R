
#spatial

library(sf)
library(NEFSCspatial)
library(tidyverse)
library(rEDM)
library(patchwork)

# NEFSC scallop survey data - through 2015 -----------------------------------------------------------------

st_layers("~/Downloads/Fish/Fish.gdb")
fish<- st_read("~/Downloads/Fish/Fish.gdb", layer="ScallopBiomass")

# st_geometry(fish)
# class(fish)
st_crs(fish) # now they are both NAD83

par(mar = c(0,0,1,0))
plot(fish[6])

fish<- fish %>% 
  filter(year_ > 1966) %>% 
  mutate(YEAR = year_, .keep = "unused") %>%
  na.omit()

scallops<-NEFSCspatial::Shellfish_Strata
plot(scallops)

testGIS <-st_read("~/Downloads/lab_notebook/data/Scallop_Rotational_Areas_20230419/Scallop_Rotational_Areas_20230411.shp")
plot(testGIS["AREANAME"], main="Scallop Management Areas")

testGB <- testGIS %>% 
  filter(GARFO_ID != "G000610" & GARFO_ID != "G000611") %>% 
  select(AREANAME, GARFO_ID, CFRPARA, FRCITE, FRDATE, SOURCE, COMMNAME, geometry) %>% 
  mutate(NAME = recode(AREANAME, "Area I Scallop Rotational Area" = "Area_I", 
                       "Area II Scallop Rotational Area" = "Area_II", 
                       "Nantucket Lightship North Scallop Rotational Area" = "NL_N",
                       "Nantucket Lightship West Scallop Rotational Area" = "NL_W"))


plot(testGB["NAME"], main="Georges Bank Scallop Management Areas", key.pos=1, key.width = 0.1, key.length = 0.9)

# Adjust CRS of the map of protected areas to match scallop survey CRS
# st_crs(fish) - NAD83 (EPSG:4269)
# st_crs(testGB) - WGS 84 / Pseudo-Mercator (EPSG: 3857)
testGB2<- st_transform(testGB, crs = "EPSG:4269")
st_crs(testGB2) # now they are both NAD83

testGB2 <- testGB2 %>% arrange(AREANAME)
areaOne <- st_intersects(testGB2, fish)[[1]]

# Map of all points over polygons
ggplot() + geom_sf(data = testGB2) + geom_sf(data = fish)

# Intersection (first polygons, then points)
interCounts <- st_intersects(testGB2, fish)

# Add point count to each polygon
testGB2$count <- lengths(interCounts)

# Map of number of points within polygons
ggplot(testGB2) + geom_sf(aes(fill = count))

# Intersection (first points, then polygons)
interID <- st_intersects(fish, testGB2)

# stations<- fish[unlist(interCounts),] #only the stations within a protected area

interID[sapply(interID, function(x) length(x)==0L)] <- 0
# Adding column area with the name of
# the areas containing the points

unlistInter <- unlist(interID)
area<-  case_when(unlistInter == 0 ~"NA",
                  unlistInter ==1 ~ "Area1",
                  unlistInter ==2 ~ "Area2",
                  unlistInter ==3 ~"NLN",
                  unlistInter ==4 ~ "NLW")
fish$area <- area


# subset by protected area
Area_I <- fish %>% 
  filter(area == "Area1")

Area_II <- fish %>% 
  filter(area == "Area2")

NL_N <- fish %>% 
  filter(area == "NLN")

NL_W <- fish %>% 
  filter(area == "NLW")

stations <- bind_rows(Area_I, Area_II, NL_N, NL_W)

ggplot(testGB2) + geom_sf() + geom_sf(data = stations)

areaI <- Area_I %>% 
  group_by(YEAR) %>% 
  summarise(avgAbund = mean(abundance),
            avgBio = mean(biomass_kg),
            avgMeat = mean(biomassMeatWeight_kg))

areaII <- Area_II %>% 
  group_by(YEAR) %>% 
  summarise(avgAbund = mean(abundance),
            avgBio = mean(biomass_kg),
            avgMeat = mean(biomassMeatWeight_kg))

NLN <- NL_N %>% 
  group_by(YEAR) %>% 
  summarise(avgAbund = mean(abundance),
            avgBio = mean(biomass_kg),
            avgMeat = mean(biomassMeatWeight_kg))

NLW <- NL_W %>% 
  group_by(YEAR) %>% 
  summarise(avgAbund = mean(abundance),
            avgBio = mean(biomass_kg),
            avgMeat = mean(biomassMeatWeight_kg))

# EDM for each area (univariate, averaged across stations - not multispatial)
library(sfheaders)

# AREA 1 ------------------------------------------------------------------

areaI <- sf_to_df(areaI, fill=TRUE)
areaIa <- areaI %>% 
  mutate(avgAbund = as.integer(avgAbund)) %>% 
  select(YEAR, avgAbund) %>% 
  distinct()

areaI_abundanceE <- EmbedDimension(dataFrame = areaIa, lib = "1 35", pred = "1 35", columns = "avgAbund", target = "avgAbund")
areaI_abundanceEval <- areaI_abundanceE[which.max(areaI_abundanceE$rho),"E"] #7

areaI_abundanceTheta <- PredictNonlinear(dataFrame = areaIa, lib = "1 35", pred = "1 35", columns = "avgAbund", target = "avgAbund", E = areaI_abundanceEval, theta=0.1:50)
areaI_abundanceThetaVal <- areaI_abundanceTheta[which.max(areaI_abundanceTheta$rho),"Theta"] #0.1

areaIb <- areaI %>% 
  select(YEAR, avgMeat) %>% 
  distinct()

areaIbE <- EmbedDimension(dataFrame = areaIb, lib = "1 35", pred = "1 35", columns = "avgMeat", target = "avgMeat")
areaIbEval <- areaIbE[which.max(areaIbE$rho),"E"] #3

areaIbTheta <- PredictNonlinear(dataFrame = areaIb, lib = "1 35", pred = "1 35", columns = "avgMeat", target = "avgMeat", E = areaIbEval)
areaIbThetaVal <- areaIbTheta[which.max(areaIbTheta$rho),"Theta"] #0.3

# AREA 2 ------------------------------------------------------------------

areaII <- sf_to_df(areaII, fill=TRUE)
areaIIa <- areaII %>% 
  mutate(avgAbund = as.integer(avgAbund)) %>% 
  select(YEAR, avgAbund) %>% 
  distinct()

areaII_abundanceE <- EmbedDimension(dataFrame = areaIIa, lib = "1 36", pred = "1 36", columns = "avgAbund", target = "avgAbund")
areaII_abundanceEval <- areaII_abundanceE[which.max(areaII_abundanceE$rho),"E"] #1, but 3 is close

areaII_abundanceTheta <- PredictNonlinear(dataFrame = areaIIa, lib = "1 36", pred = "1 36", columns = "avgAbund", target = "avgAbund", E = 3, theta=0.1:20)
areaII_abundanceThetaVal <- areaII_abundanceTheta[which.max(areaII_abundanceTheta$rho),"Theta"] #9.1

areaIIb <- areaII %>% 
  select(YEAR, avgMeat) %>% 
  distinct()

areaIIbE <- EmbedDimension(dataFrame = areaIIb, lib = "1 36", pred = "1 36", columns = "avgMeat", target = "avgMeat")
areaIIbEval <- areaIIbE[which.max(areaIIbE$rho),"E"] #4

areaIIbTheta <- PredictNonlinear(dataFrame = areaIIb, lib = "1 36", pred = "1 36", columns = "avgMeat", target = "avgMeat", E = areaIIbEval)
areaIIbThetaVal <- areaIIbTheta[which.max(areaIIbTheta$rho),"Theta"] #1

# Nantucket Lightship North ------------------------------------------------------------------

NLN <- sf_to_df(NLN, fill=TRUE)
NLNa <- NLN %>% 
  mutate(avgAbund = as.integer(avgAbund)) %>% 
  select(YEAR, avgAbund) %>% 
  distinct()

NLN_abundanceE <- EmbedDimension(dataFrame = NLNa, lib = "1 36", pred = "1 36", columns = "avgAbund", target = "avgAbund")
NLN_abundanceEval <- NLN_abundanceE[which.max(NLN_abundanceE$rho),"E"] #1

NLN_abundanceTheta <- PredictNonlinear(dataFrame = NLNa, lib = "1 36", pred = "1 36", columns = "avgAbund", target = "avgAbund", E = NLN_abundanceEval, theta = 1:20)
NLN_abundanceThetaVal <- NLN_abundanceTheta[which.max(NLN_abundanceTheta$rho),"Theta"] #1

NLNb <- NLN %>% 
  select(YEAR, avgMeat) %>% 
  distinct()

NLNbE <- EmbedDimension(dataFrame = NLNb, lib = "1 36", pred = "1 36", columns = "avgMeat", target = "avgMeat")
NLNbEval <- NLNbE[which.max(NLNbE$rho),"E"] #1, but 8 is also reasonable

NLNbTheta <- PredictNonlinear(dataFrame = NLNb, lib = "1 36", pred = "1 36", columns = "avgMeat", target = "avgMeat", E = NLNbEval)
NLNbThetaVal <- NLNbTheta[which.max(NLNbTheta$rho),"Theta"] #2

# Nantucket Lightship West ------------------------------------------------------------------

NLW <- sf_to_df(NLW, fill=TRUE)
NLWa <- NLW %>% 
  mutate(avgAbund = as.integer(avgAbund)) %>% 
  select(YEAR, avgAbund) %>% 
  distinct()

NLW_abundanceE <- EmbedDimension(dataFrame = NLWa, lib = "1 32", pred = "1 32", columns = "avgAbund", target = "avgAbund")
NLW_abundanceEval <- NLW_abundanceE[which.max(NLW_abundanceE$rho),"E"] #1

NLW_abundanceTheta <- PredictNonlinear(dataFrame = NLWa, lib = "1 32", pred = "1 32", columns = "avgAbund", target = "avgAbund", E = NLW_abundanceEval, theta=1:45)
NLW_abundanceThetaVal <- NLW_abundanceTheta[which.max(NLW_abundanceTheta$rho),"Theta"] #3

NLWb <- NLW %>% 
  select(YEAR, avgMeat) %>% 
  distinct()

NLWbE <- EmbedDimension(dataFrame = NLWb, lib = "1 32", pred = "1 32", columns = "avgMeat", target = "avgMeat")
NLWbEval <- NLWbE[which.max(NLWbE$rho),"E"] #3

NLWbTheta <- PredictNonlinear(dataFrame = NLWb, lib = "1 32", pred = "1 32", columns = "avgMeat", target = "avgMeat", E = NLWbEval)
NLWbThetaVal <- NLWbTheta[which.max(NLWbTheta$rho),"Theta"] #4

# Multispatial EDM for each area (univariate, averaged across stations)
library(multispatialCCM)

# Abundance embedding dimension plots
# Eplot1a <- ggplot(areaI_abundanceE, aes(x=E, y=rho))+geom_line()+theme_classic()+
#   labs(title = "Area 1 abundance")+ theme(text = element_text(size = 13))
# Eplot2a <- ggplot(areaII_abundanceE, aes(x=E, y=rho))+geom_line()+theme_classic()+
#   labs(title = "Area 2 abundance")+ theme(text = element_text(size = 13))
# Eplot3a <- ggplot(NLN_abundanceE, aes(x=E, y=rho))+geom_line()+theme_classic()+
#   labs(title = "NL North abundance")+ theme(text = element_text(size = 13))
# Eplot4a <- ggplot(NLW_abundanceE, aes(x=E, y=rho))+geom_line()+theme_classic()+
#   labs(title = "NL West abundance")+ theme(text = element_text(size = 13))

# Eplot1a + Eplot2a + Eplot3a + Eplot4a

#Biomass embedding dimension plots
# Eplot1b <- ggplot(areaIbE, aes(x=E, y=rho))+geom_line()+theme_classic()+
#   labs(title = "Area 1 biomass")+ theme(text = element_text(size = 13))
# Eplot2b <- ggplot(areaIIbE, aes(x=E, y=rho))+geom_line()+theme_classic()+
#   labs(title = "Area 2 biomass")+ theme(text = element_text(size = 13))
# Eplot3b <- ggplot(NLNbE, aes(x=E, y=rho))+geom_line()+theme_classic()+
#   labs(title = "NL North biomass")+ theme(text = element_text(size = 13))
# Eplot4b <- ggplot(NLWbE, aes(x=E, y=rho))+geom_line()+theme_classic()+
#   labs(title = "NL West biomass")+ theme(text = element_text(size = 13))
# 
# Eplot1b + Eplot2b + Eplot3b + Eplot4b

#Abundance linearity plots
# Tplot1a <- ggplot(areaI_abundanceTheta, aes(x=Theta, y=rho))+geom_line()+theme_classic()+
#   labs(title = "Area 1 abundance")+ theme(text = element_text(size = 13))
# Tplot2a <- ggplot(areaII_abundanceTheta, aes(x=Theta, y=rho))+geom_line()+theme_classic()+
#   labs(title = "Area 2 abundance")+ theme(text = element_text(size = 13))
# Tplot3a <- ggplot(NLN_abundanceTheta, aes(x=Theta, y=rho))+geom_line()+theme_classic()+
#   labs(title = "NL North abundance")+ theme(text = element_text(size = 13))
# Tplot4a <- ggplot(NLW_abundanceTheta, aes(x=Theta, y=rho))+geom_line()+theme_classic()+
#   labs(title = "NL West abundance")+ theme(text = element_text(size = 13))
# 
# Tplot1a + Tplot2a + Tplot3a + Tplot4a

#Biomass linearity plots
# Tplot1b <- ggplot(areaIbTheta, aes(x=Theta, y=rho))+geom_line()+theme_classic()+
#   labs(title = "Area 1 biomass")+ theme(text = element_text(size = 13))
# Tplot2b <- ggplot(areaIIbTheta, aes(x=Theta, y=rho))+geom_line()+theme_classic()+
#   labs(title = "Area 2 biomass")+ theme(text = element_text(size = 13))
# Tplot3b <- ggplot(NLNbTheta, aes(x=Theta, y=rho))+geom_line()+theme_classic()+
#   labs(title = "NL North biomass")+ theme(text = element_text(size = 13))
# Tplot4b <- ggplot(NLWbTheta, aes(x=Theta, y=rho))+geom_line()+theme_classic()+
#   labs(title = "NL West biomass")+ theme(text = element_text(size = 13))
# 
# Tplot1b + Tplot2b + Tplot3b + Tplot4b

# non-spatial EDM for each station individually
# then use Bayesian model that accounts for spatial autocorrelation to compare areas
# using EDM results from each station as model input

# Take IDs from ScallopLenOverlap and combine with aCat, then run the same analysis on asterias as you did for scallops in the spatial.R file

df_stations <- read.csv("data/22564_UNION_FSCS_SVSTA2022.csv")
dat_cat <- read.csv("data/22564_UNION_FSCS_SVCAT2022.csv")

df_stations <- df_stations %>% 
  mutate(STATION = as.numeric(STATION), YEAR = EST_YEAR) %>% 
  select(c("CRUISE6","CRUISE","STRATUM", "TOW","STATION","ID","AREA","SVVESSEL","SVGEAR",
           "BEGIN_EST_TOWDATE","YEAR","SETDEPTH", "AVGDEPTH", "BEGLAT", "BEGLON"))

df_stations <- df_stations %>% 
  mutate(BEGIN_EST_TOWDATE = lubridate::dmy(BEGIN_EST_TOWDATE)) %>% 
  mutate(YEAR = year(BEGIN_EST_TOWDATE)) %>% 
  mutate(TOW = as.integer(TOW))

aCat <- dat_cat %>% 
  filter(SCIENTIFIC_NAME =="Asterias") %>% 
  # | SCIENTIFIC_NAME =="Asterias rubens (boreal asterias)"  ) %>%
  mutate(CRUISE6 = as.factor(CRUISE6),
         CRUISE = as.factor(CRUISE),
         STATION = as.numeric(STATION),
         .keep = "unused")

aCat <- left_join(aCat, df_stations)

# df_stations %>% filter(TOW == 17) %>% filter(STATION == 74)

cruises <- read.csv("data/22564_SVDBS_CRUISES2022.csv")

st_layers("~/Downloads/Fishing_Effects_Sediment/FishingEffectsSediment.shp")
sediment<- st_read("~/Downloads/Fishing_Effects_Sediment/FishingEffectsSediment.shp")
plot(sediment[1])

sediment<- st_transform(sediment, crs = "EPSG:4269")
st_crs(sediment)

gbBounds <- st_as_sfc(st_bbox(testGB2))
fishBounds <- st_as_sfc(st_bbox(fish))
#sedimentGB <- st_contains(sediment, gbBounds)

# sedimentGB <- sediment %>% 
#  filter(withinGB == TRUE)

sedimentGB <- st_intersects(gbBounds, sediment)
sedimentFish <- st_intersects(fishBounds, sediment)

sedimentGB2 <- sediment %>% 
  filter(row_number() %in% sedimentGB[[1]]) 

sedimentFish2 <- sediment %>% 
  filter(row_number() %in% sedimentFish[[1]])

plot(sedimentGB2[1])
plot(sedimentFish2[1])

ggplot(testGB2) + geom_sf() + geom_sf(data = sedimentGB2[1])
ggplot(testGB2) + geom_sf() + geom_sf(data = sedimentFish2[1])

ggplot(sedimentGB2[1]) + geom_sf() + geom_sf(data = testGB2)
ggplot(sedimentFish2[1]) + geom_sf() + geom_sf(data = testGB2)

ggplot(sedimentGB2[1]) + geom_sf() + geom_sf(data = fish)

# Intersection (first polygons, then points)
interCountsSed <- st_intersects(sedimentGB2, fish)

# Add point count to each polygon
sedimentGB2$count <- lengths(interCountsSed)

# Map of number of points within polygons
ggplot(sedimentGB2) + geom_sf(aes(fill = count))

# Intersection (first points, then polygons)
interIDSed <- st_intersects(fish, sedimentGB2)

interIDSed[sapply(interIDSed, function(x) length(x)==0L)] <- 0

fishPts <- fish %>% 
  filter(interIDSed[row_number()] != 0)
ggplot(sedimentGB2[1]) + geom_sf() + geom_sf(data = fishPts)


# Length ------------------------------------------------------------------
dat_len<- read.csv("data/22564_UNION_FSCS_SVLEN2022.csv")
scallopLen <- dat_len %>%
  filter(SCIENTIFIC_NAME == "Placopecten magellanicus (sea scallop)") %>% 
  mutate(NAME = recode(SCIENTIFIC_NAME, "Placopecten magellanicus (sea scallop)" = "scallop")) #shorten for readability

scallopLen <- scallopLen %>% 
  select(-c("CATCHSEX", "STATUS_CODE", "SVSPP", "SCIENTIFIC_NAME"))
rm(dat_len)

scallopLen <- scallopLen %>%
  mutate(CRUISE6 = as.factor(CRUISE6),
         CRUISE = as.factor(CRUISE),
         STATION = as.numeric(STATION),
         .keep = "unused")

scallopLen <- left_join(scallopLen, cruises, by="CRUISE6")

scallopLen <- scallopLen %>%
  mutate(YEAR = as.numeric(YEAR)) %>%
  filter(as.numeric(STRATUM) > 5999) #shellfish strata start with 6

len <- left_join(scallopLen, df_stations)
