
#spatial

library(sf)
library(NEFSCspatial)
library(tidyverse)
library(rEDM)

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
areaI_abundanceE <- areaI_abundanceE[which.max(areaI_abundanceE$rho),"E"] #7

areaI_abundanceTheta <- PredictNonlinear(dataFrame = areaIa, lib = "1 35", pred = "1 35", columns = "avgAbund", target = "avgAbund", E = areaI_abundanceE, theta=0.1:50)
areaI_abundanceTheta <- areaI_abundanceTheta[which.max(areaI_abundanceTheta$rho),"Theta"] #0.1

areaIb <- areaI %>% 
  select(YEAR, avgMeat) %>% 
  distinct()

areaIbE <- EmbedDimension(dataFrame = areaIb, lib = "1 35", pred = "1 35", columns = "avgMeat", target = "avgMeat")
areaIbE <- areaIbE[which.max(areaIbE$rho),"E"] #3

areaIbTheta <- PredictNonlinear(dataFrame = areaIIb, lib = "1 35", pred = "1 35", columns = "avgMeat", target = "avgMeat", E = areaIbE)
areaIbTheta <- areaIbTheta[which.max(areaIbTheta$rho),"Theta"] #0.3

# AREA 2 ------------------------------------------------------------------

areaII <- sf_to_df(areaII, fill=TRUE)
areaIIa <- areaII %>% 
  mutate(avgAbund = as.integer(avgAbund)) %>% 
  select(YEAR, avgAbund) %>% 
  distinct()

areaII_abundanceE <- EmbedDimension(dataFrame = areaIIa, lib = "1 36", pred = "1 36", columns = "avgAbund", target = "avgAbund")
areaII_abundanceE <- areaII_abundanceE[which.max(areaII_abundanceE$rho),"E"] #1, but 3 is close

areaII_abundanceTheta <- PredictNonlinear(dataFrame = areaIIa, lib = "1 36", pred = "1 36", columns = "avgAbund", target = "avgAbund", E = 3, theta=0.1:20)
areaII_abundanceTheta <- areaII_abundanceTheta[which.max(areaII_abundanceTheta$rho),"Theta"] #9.1

areaIIb <- areaII %>% 
  select(YEAR, avgMeat) %>% 
  distinct()

areaIIbE <- EmbedDimension(dataFrame = areaIIb, lib = "1 36", pred = "1 36", columns = "avgMeat", target = "avgMeat")
areaIIbE <- areaIIbE[which.max(areaIIbE$rho),"E"] #4

areaIIbTheta <- PredictNonlinear(dataFrame = areaIIb, lib = "1 36", pred = "1 36", columns = "avgMeat", target = "avgMeat", E = areaIIbE)
areaIIbTheta <- areaIIbTheta[which.max(areaIIbTheta$rho),"Theta"] #1

# Nantucket Lightship North ------------------------------------------------------------------

NLN <- sf_to_df(NLN, fill=TRUE)
NLNa <- NLN %>% 
  mutate(avgAbund = as.integer(avgAbund)) %>% 
  select(YEAR, avgAbund) %>% 
  distinct()

NLN_abundanceE <- EmbedDimension(dataFrame = NLNa, lib = "1 36", pred = "1 36", columns = "avgAbund", target = "avgAbund")
NLN_abundanceE <- NLN_abundanceE[which.max(NLN_abundanceE$rho),"E"] #1

NLN_abundanceTheta <- PredictNonlinear(dataFrame = NLNa, lib = "1 36", pred = "1 36", columns = "avgAbund", target = "avgAbund", E = NLN_abundanceE, theta = 1:20)
NLN_abundanceTheta <- NLN_abundanceTheta[which.max(NLN_abundanceTheta$rho),"Theta"] #1

NLNb <- NLN %>% 
  select(YEAR, avgMeat) %>% 
  distinct()

NLNbE <- EmbedDimension(dataFrame = NLNb, lib = "1 36", pred = "1 36", columns = "avgMeat", target = "avgMeat")
NLNbE <- NLNbE[which.max(NLNbE$rho),"E"] #1, but 8 is also reasonable

NLNbTheta <- PredictNonlinear(dataFrame = NLNb, lib = "1 36", pred = "1 36", columns = "avgMeat", target = "avgMeat", E = NLNbE)
NLNbTheta <- NLNbTheta[which.max(NLNbTheta$rho),"Theta"] #2

# Nantucket Lightship West ------------------------------------------------------------------

NLW <- sf_to_df(NLW, fill=TRUE)
NLWa <- NLW %>% 
  mutate(avgAbund = as.integer(avgAbund)) %>% 
  select(YEAR, avgAbund) %>% 
  distinct()

NLW_abundanceE <- EmbedDimension(dataFrame = NLWa, lib = "1 32", pred = "1 32", columns = "avgAbund", target = "avgAbund")
NLW_abundanceE <- NLW_abundanceE[which.max(NLW_abundanceE$rho),"E"] #1

NLW_abundanceTheta <- PredictNonlinear(dataFrame = NLWa, lib = "1 32", pred = "1 32", columns = "avgAbund", target = "avgAbund", E = NLW_abundanceE, theta=1:45)
NLW_abundanceTheta <- NLW_abundanceTheta[which.max(NLW_abundanceTheta$rho),"Theta"] #3

NLWb <- NLW %>% 
  select(YEAR, avgMeat) %>% 
  distinct()

NLWbE <- EmbedDimension(dataFrame = NLWb, lib = "1 32", pred = "1 32", columns = "avgMeat", target = "avgMeat")
NLWbE <- NLWbE[which.max(NLWbE$rho),"E"] #3

NLWbTheta <- PredictNonlinear(dataFrame = NLWb, lib = "1 32", pred = "1 32", columns = "avgMeat", target = "avgMeat", E = NLWbE)
NLWbTheta <- NLWbTheta[which.max(NLWbTheta$rho),"Theta"] #4

# Multispatial EDM for each area (univariate, averaged across stations)
library(multispatialCCM)

# AREA 1 ------------------------------------------------------------------

# AreaI_multispatial <- sf_to_df(Area_I, fill = TRUE)
# test <- AreaI_multispatial %>% 
#   mutate %>% 
#   group_by(station) %>% 
#   summarise(years=n_distinct(YEAR))









# non-spatial EDM for each station individually
# then use Bayesian model that accounts for spatial autocorrelation to compare areas
# using EDM results from each station as model input

lengths(interID)

fish <- fish %>% 
  mutate(area = c(0))



x <- 1:50
test<-case_when(
  x %% 35 == 0 ~ "fizz buzz",
  x %% 5 == 0 ~ "fizz",
  x %% 7 == 0 ~ "buzz",
  TRUE ~ as.character(x)
)
