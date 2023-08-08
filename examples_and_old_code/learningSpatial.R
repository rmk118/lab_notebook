#Practicing spatial data science
#Ruby Krasnow
#Last modified: July 15, 2023

#Install packages

library(sf)
library(spdep)
library(tidyverse)
library(Hmisc)
library(robustHD)

library(gap)
library(gridExtra)
library(geodaData)
library(spData)
library(mapview)
library(tmap)

mergedGrid <- st_union(surveyGrid, by_feature = FALSE) %>% st_sf()
mergedGridBuffer <- st_buffer(mergedGrid, 9000)
ggplot(mergedGridBuffer)+geom_sf()

s_cat_sf<- st_as_sf(s_cat_clean, coords = c("Start_Longitude", "Start_Latitude"), crs=4326)
voronoiScal <- s_cat_sf %>%
  st_geometry() %>% # to get sfc from sf
  st_union() %>% # to get a sfc of MULTIPOINT type
  st_voronoi() %>% #
  st_collection_extract(type = "POLYGON") %>% # a list of polygons
  st_sf() %>% # from list to sf object
  st_intersection(mergedGridBuffer) %>%
  st_join(s_cat_sf)  # put names back
 #envelope = st_geometry(mergedGrid)

class(voronoiScal)
plot(voronoiScal$geometry)
mapview(voronoiScal, zcol = "logCatch")

# Neighbors
nb <- poly2nb(voronoiScal, queen = TRUE) # queen shares point or border
nbw <- nb2listw(nb, style = "W")

# Global Moran's I
gmoranScal <- moran.test(voronoiScal$logCatch, nbw,alternative = "greater")
gmoranScal

moran.plot(voronoiScal$logCatch, nbw)
# Global Moran's I
# gmoran <- moran.test(map$vble, nbw,
#                      alternative = "greater")

lmoran <- localmoran(voronoiScal$logCatch, nbw, alternative = "greater")
head(lmoran)
# gmoran

ggplot() +
  geom_sf(data = voronoiScal, alpha = .5) +
  # geom_sf(data = state, lwd = .75, fill = NA) + 
  geom_sf(data = s_cat_sf, color = "red", pch = 19, size = 0.5)


tmap_mode("view")

voronoiScal$lmI <- lmoran[, "Ii"] # local Moran's I
voronoiScal$lmZ <- lmoran[, "Z.Ii"] # z-scores
# p-values corresponding to alternative greater
voronoiScal$lmp <- lmoran[, "Pr(z > E(Ii))"]

p1 <- tm_shape(voronoiScal) +
  tm_polygons(col = "logCatch", title = "logCatch", style = "quantile") +
  tm_layout(legend.outside = TRUE)

p2 <- tm_shape(voronoiScal) +
  tm_polygons(col = "lmI", title = "Local Moran's I",
              style = "quantile") +
  tm_layout(legend.outside = TRUE)

p3 <- tm_shape(voronoiScal) +
  tm_polygons(col = "lmZ", title = "Z-score",
              breaks = c(-Inf, 1.65, Inf)) +
  tm_layout(legend.outside = TRUE)

p4 <- tm_shape(voronoiScal) +
  tm_polygons(col = "lmp", title = "p-value",
              breaks = c(-Inf, 0.05, Inf)) +
  tm_layout(legend.outside = TRUE)

tmap_arrange(p1, p2, p3, p4)
mapview(voronoiScal, zcol="logCatch")
mapview(voronoiScal, zcol="lmI")

 #### weight
p1a <- tm_shape(voronoiScal) +
  tm_polygons(col = "logWt", title = "logWeight", style = "quantile") +
  tm_layout(legend.outside = TRUE)

p2a <- tm_shape(voronoiScal) +
  tm_polygons(col = "lmIwt", title = "Local Moran's I",
              style = "quantile") +
  tm_layout(legend.outside = TRUE)

p3a <- tm_shape(voronoiScal) +
  tm_polygons(col = "lmZwt", title = "Z-score",
              breaks = c(-Inf, 1.65, Inf)) +
  tm_layout(legend.outside = TRUE)

p4a <- tm_shape(voronoiScal) +
  tm_polygons(col = "lmpWt", title = "p-value",
              breaks = c(-Inf, 0.05, Inf)) +
  tm_layout(legend.outside = TRUE)

tmap_arrange(p1a, p2a, p3a, p4a)

gmoranMC <- moran.mc(voronoiScal$logCatch, nbw, nsim = 999)
gmoranMC

mapview(voronoiScal, zcol="lmI")
mapview(voronoiScal, zcol="lmpWt")

# moranPlot <- ggplot(voronoiScal)+geom_sf(aes(fill=lmI))+scale_fill_viridis_c(option = "G", name="Local Moran's I\n(catch)")+theme_bw()
# catchPplot<- ggplot(voronoiScal)+geom_sf(aes(fill=(as.logical(lmp<0.05))))+scale_fill_grey(start = 0.75, end = 0, name="Local Moran's I\n(catch) p-val", labels=c("p≥0.05", "p<0.05"))+theme_bw()
# catchPlot + moranPlot+ plot_annotation(tag_levels = 'A') + plot_layout(tag_level = 'new') & 
#   theme(plot.tag = element_text(size = 12, vjust = 6))
# moranPlotWt <- ggplot(voronoiScal)+geom_sf(aes(fill=lmIwt))+scale_fill_viridis_c(option = "G", name="Local Moran's I\n(weight)")+theme_bw()
# wtPplot<- ggplot(voronoiScal)+geom_sf(aes(fill=(as.logical(lmpWt<0.05))))+scale_fill_grey(start = 0.75, end = 0, name="Local Moran's I\n(weight) p-val", labels=c("p≥0.05", "p<0.05"))+theme_bw()

hist(gmoranMC$res)
abline(v = gmoranMC$statistic, col = "red")


# Intersection (first points, then grid squares)
# interGrid <- st_intersects(s_cat_sf, surveyGrid)

# Intersection (first polygons, then points)
interCounts2 <- st_intersects(surveyGrid, s_cat_sf)
#interCounts2[sapply(interCounts2, function(x) length(x)==0L)] <- 0
# Add point count to each polygon
surveyGrid$count <- lengths(interCounts2)

# Map of number of points within polygons
ggplot(surveyGrid) + geom_sf(aes(fill = count))
ggplot(surveyGrid %>% filter(count > 0)) + geom_sf(aes(fill = count))

interGrid[sapply(interGrid, function(x) length(x)==0L)] <- 0

# Adding column area with the name of
# the areas containing the points
unlistInterGrid <- unlist(interGrid)
pointsInGrid <- s_cat_sf %>% mutate(gridRow = unlistInterGrid, .before=Tow_Number)

pointsInGrid %>% group_by(gridRow) %>% summarise(num = n())

ggplot(s_cat_sf) +geom_sf(aes(color=area))

weird <- voronoiScal[997,]

ggplot() +geom_sf(data=surveyGrid)+geom_sf(data=weird, color="red")
