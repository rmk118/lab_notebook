#Maine spatial analysis
#Ruby Krasnow
#Last modified: July 7, 2023

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
plot(surveyGrid["region_stratum"], main="Study Area")

surveyGrid %>% group_by(Region, Stratum) %>% summarise(num = n_distinct(GridID))

surveyed <- surveyGrid %>% filter(!is.na(surveys))
plot(surveyed["region_stratum"], main="Study Area")
plot(surveyed["Region"], main="Region")

# calculate the average number of tows per area over the whole time series
sumTemp <- s_cat_Spatial %>% group_by(area) %>% summarise(num = n_distinct(row_number()))
summary(sumTemp$num) #Min is 123, max is 265, mean and median are both ~190

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
ggplot(data=s_cat_sf)+geom_sf(aes(color = area))

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

#all grids merged together
ggplot(st_union(surveyGrid, by_feature = FALSE) %>% st_sf()) + geom_sf()

#find the grids and points in region 1.1
region1.1grid<- surveyGrid %>% filter(region_stratum=="1 1") %>% st_union(by_feature = FALSE) #merged together
region1.1points <- s_cat_sf %>% filter(area=="1 1")

#find all points not in region 1.1
not1.1points <- s_cat_sf %>% filter(area!="1 1")

# This plot shows all points not in 1.1 over the entire grid (all areas)
# ggplot() + geom_sf(data = surveyGrid) + geom_sf(data = not1.1points)

# This plot shows all points marked as being in 1.1 over the region of merged 1.1 grids
# ggplot()+ geom_sf(data=region1.1grid)+geom_sf(data = region1.1points)

# Use st_length(surveyGrid) and st_length(s_cat_sf) to check that units are m
# This plot adds a 1000m buffer around each point
# ggplot()+ geom_sf(data=region1.1grid)+geom_sf(data = st_buffer(region1.1points, 1000)) 

# find all of points NOT marked as being in region 1.1 that are located within 2 tows length 
# (2 nautical miles = 3704 meters) of region 1.1 
neighbors_1.1 <- st_intersection(not1.1points, st_buffer(region1.1grid, 3704))

# This plot shows the neighboring points in the context of the entire study region
# ggplot()+geom_sf(data=st_union(surveyGrid, by_feature = FALSE))+geom_sf(data = neighbors_1.1)

# This plot shows the neighboring points over region 1.1 (with grid boundaries) and the buffer around 1.1 - best visual
ggplot()+geom_sf(data = st_buffer(region1.1grid, 3704))+ 
  geom_sf(data=surveyGrid %>% filter(region_stratum=="1 1"))+ 
  geom_sf(data = neighbors_1.1)

nrow(neighbors_1.1) #207

# Start with 10 iterations (10 random samples of 90 neighbors), ideally would do 100

firstStrata <- sample_n(neighbors_1.1, 90, replace = FALSE) %>% mutate(trial = 1, .before=area)
for (i in 2:10) {
  tempStrata <- sample_n(neighbors_1.1, 90, replace = FALSE) %>% mutate(trial = i, .before=area)
  firstStrata<- bind_rows(firstStrata, tempStrata)
}