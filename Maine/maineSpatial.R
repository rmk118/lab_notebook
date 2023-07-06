#Maine spatial analysis
#Ruby Krasnow
#Last modified: July 6, 2023

#spatial packages
library(sf)
library(sfheaders)
library(plotly)
library(spdep)
library(tidyverse)
library(lubridate)


cleanCatch <- function(x) {
  full_join(x, df_tows) %>%
    arrange(Survey, Tow_Number) %>% 
    select(-c("Stratum", "Subsample_Weight_kg_2", "Date", "Surface_WaterTemp_DegC", "Surface_Salinity", "Air_Temp", "Tow_Time")) %>%
    mutate(Number_Caught = replace_na(Number_Caught,0),
           Weight_kg = replace_na(Weight_kg,0),
           Expanded_Catch = replace_na(Expanded_Catch,0),
           Expanded_Weight_kg = replace_na(Expanded_Weight_kg,0)) %>% 
    mutate(Stratum = Depth_Stratum, Date = date(ymd_hms(Start_Date)), .keep="unused")
}

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
#plot(surveyGrid["region_stratum"], main="Study Area")

surveyGrid %>% group_by(Region, Stratum) %>% summarise(num = n_distinct(GridID))

surveyed <- surveyGrid %>% filter(!is.na(surveys))
plot(surveyed["region_stratum"], main="Study Area")
plot(surveyed["Region"], main="Region")

sumTemp <- surveyed %>% group_by(Region, Stratum) %>% summarise(num = n_distinct(GridID))
mean(sumTemp$num)

df_s_cat<- read.csv("data/Maine_inshore_trawl/MEscallopCatch.csv") #scallop catch
df_tows<-read.csv("data/Maine_inshore_trawl/MEtows.csv") #tow data

s_cat_Spatial <- cleanCatch(df_s_cat) %>% 
  mutate(Common_Name = "Scallop") %>% 
  select(-c("End_Latitude","End_Longitude")) %>% 
  mutate(area = paste(Region, Stratum),.before= Survey)

s_cat_sf<- st_as_sf(s_cat_Spatial, coords = c("Start_Longitude", "Start_Latitude"), crs=4326)

# Map of all points over grid
#ggplot() + geom_sf(data = surveyGrid) + geom_sf(data = s_cat_sf)

head(s_cat_sf)
#ggplot(data=s_cat_sf)+geom_sf(aes(color = area))

st_queen <- function(a, b = a) st_relate(a, b, pattern = "F***T****")
sf.sgbp.surveyed <- st_queen(surveyed)

as.nb.sgbp <- function(x, ...) {
  attrs <- attributes(x)
  x <- lapply(x, function(i) { if(length(i) == 0L) 0L else i } )
  attributes(x) <- attrs
  class(x) <- "nb"
  x
}
sf.nb.surveyed <- as.nb.sgbp(sf.sgbp.surveyed)
summary(sf.nb.surveyed)

test <- st_centroid(surveyed)
head(test) #CRS: WGS 84/EPSG 4326

longitude <- map_dbl(surveyed$geometry, ~st_centroid(.x)[[1]])
latitude <- map_dbl(surveyed$geometry, ~st_centroid(.x)[[2]])
coords <- cbind(longitude, latitude)
head(coords)
plot(sf.nb.surveyed, coords, lwd=.2, col="blue", cex = .5)

surveyed.card <- card(sf.nb.surveyed)
max(surveyed.card)
ggplot() +
  geom_histogram(aes(x=surveyed.card), breaks = seq(0,9, by = 1)) +
  xlab("number of neighbors")

surveyedNoGeom <- st_drop_geometry(surveyed) %>% select(c("OBJECTID", "region_stratum"))

neighbors_df<-data.frame(sf.sgbp.surveyed)
neighbors_df<- neighbors_df %>% 
  mutate(OBJECTID = surveyedNoGeom[row.id, "OBJECTID"], .keep="unused") %>% 
  mutate(neighborID = surveyedNoGeom[col.id, "OBJECTID"], .keep="unused") 

neighbors_df_test<- left_join(neighbors_df, surveyedNoGeom) %>% rename(objectRegion = region_stratum, objectID = OBJECTID)
neighbors_df_test <- left_join(neighbors_df_test, surveyedNoGeom, by = c("neighborID"="OBJECTID"))



# Region 1, Stratum 1 -----------------------------------------------------
# newMatrix<- data.frame(matrix(nrow=5, ncol=4))
# 
# for (i in 1:5) {
#   for (j in 1:4) {
# tempArea <- neighbors_df_test %>% 
#   group_by(objectID) %>% 
#   filter(neighborRegion == paste(i,j)) %>% 
#   filter(objectRegion != paste(i,j))
# 
# newMatrix[i,j]<-nrow(tempArea)
# print(paste(i,j))
# print(nrow(tempArea))
# }
# }
# 
# tempMat <- neighbors_df_test %>% 
#   group_by(objectID) %>% 
#   filter(neighborRegion == "3 4")


# Combine geometries
ggplot(st_union(surveyGrid, by_feature = FALSE) %>% st_sf()) + geom_sf()

region1.1grid<- surveyGrid %>% filter(region_stratum=="1 1") %>% st_union(by_feature = FALSE)
ggplot()+ geom_sf(data=region1.1)+geom_sf(data = s_cat_sf %>% filter(area=="1 1"))

#find all points not in region 1.1
region1.1points <- s_cat_sf %>% filter(area=="1 1")
not1.1points <- s_cat_sf %>% filter(area!="1 1")
# find all of those points within 2 tows length buffer of region 1.1 
ggplot()+geom_sf(data=st_union(surveyGrid, by_feature = FALSE))+geom_sf(data = st_intersection(not1.1points, st_buffer(region1.1points, 1000)))


