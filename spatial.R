
#spatial

library(sf)
library(NEFSCspatial)
library(tidyverse)

# NEFSC scallop survey data - through 2015 -----------------------------------------------------------------

st_layers("~/Downloads/Fish/Fish.gdb")
fish<- st_read("~/Downloads/Fish/Fish.gdb", layer="ScallopBiomass")

# st_geometry(fish)
# class(fish)

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

# Map of points within polygons
ggplot() + geom_sf(data = testGB2) + geom_sf(data = fish)

# Intersection (first polygons, then points)
interCounts <- st_intersects(testGB2, fish)

# Add point count to each polygon
testGB2$count <- lengths(interCounts)

# Map of number of points within polygons
ggplot(testGB2) + geom_sf(aes(fill = count))

# Intersection (first points, then polygons)
interID <- st_intersects(fish, testGB2)

stations<- fish[unlist(interCounts),] #only the stations within a protected area

# Adding column area with the name of
# the areas containing the points
stations$area <- testGB2[unlist(interID), "NAME",
                         drop = TRUE] # drop geometry
stations


# subset by protected area
Area_I <- stations %>% 
  filter(area == "Area_I")

Area_II <- stations %>% 
  filter(area == "Area_II")

NL_W <- stations %>% 
  filter(area == "NL_W")

NL_N <- stations %>% 
  filter(area == "NL_N")

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
