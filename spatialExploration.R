#Spatial data exploration
#Ruby Krasnow
#Last modified: June 24, 2023

# SMAST Scallop data -----------------------------------------------------------------

library(sf)

# SMAST Scallop data - the data I want, but averaged across years
st_layers("data/SMAST_SCALLOP/SMAST_Scallops.gdb")
test <- st_read("data/SMAST_SCALLOP/SMAST_Scallops.gdb")

class(test)
attr(test, "sf_column")
print(test[1:15], n = 3)
test_geom <- st_geometry(test)

par(mar = c(0,0,1,0))
plot(test[6])

# Spatial -----------------------------------------------------------------

# NEFSC scallop survey data - through 2015
st_layers("~/Downloads/Fish/Fish.gdb")
fish <- st_read("~/Downloads/Fish/Fish.gdb", layer="ScallopBiomass")

attr(fish, "sf_column")
st_geometry(fish)
class(fish)

par(mar = c(0,0,1,0))
plot(fish[6])

# NEFSC Spatial -----------------------------------------------------------------
library(sf)
library(raster)
library(NEFSCspatial)
library(tidyverse)

scallops<-NEFSCspatial::Shellfish_Strata
plot(scallops)

testGIS<-st_read("~/Downloads/Scallop_Rotational_Areas_20230419/Scallop_Rotational_Areas_20230411.shp")
plot(testGIS["AREANAME"], main="Scallop Management Areas")
testGB <- testGIS %>% 
  filter(GARFO_ID != "G000610" & GARFO_ID != "G000611") %>% 
  select(AREANAME, GARFO_ID, CFRPARA, FRCITE, FRDATE, SOURCE, COMMNAME, geometry) %>% 
  mutate(NAME = recode(AREANAME, "Area I Scallop Rotational Area" = "Area_I", "Area II Scallop Rotational Area" = "Area_II", "Nantucket Lightship North Scallop Rotational Area" = "NL_N","Nantucket Lightship West Scallop Rotational Area" = "NL_W"))
