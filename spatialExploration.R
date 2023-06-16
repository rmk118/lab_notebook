#Spatial data exploration
#Ruby Krasnow
#Last modified: June 16, 2023

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