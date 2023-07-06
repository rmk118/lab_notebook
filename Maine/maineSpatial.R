#Maine spatial analysis
#Ruby Krasnow
#Last modified: July 6, 2023

#spatial packages
library(sf)
library(sfheaders)
library(plotly)
library(spdep)
library(tidyverse)


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
         Grid = grid_id, .keep="unused", .before=last_surve)

surveyGrid$stratum_region <- paste(surveyGrid$Stratum, surveyGrid$Region)

#plot(surveyGrid["Grid"], main="Grid ID")
#plot(surveyGrid["OBJECTID"], main="Object ID")
# plot(surveyGrid["Region"], main="Region")
# plot(surveyGrid["Stratum"], main="Depth Stratum")
#plot(surveyGrid["stratum_region"], main="Study Area")

surveyGrid %>% group_by(Region, Stratum) %>% summarise(num = n_distinct(Grid))

surveyed <- surveyGrid %>% filter(!is.na(surveys))
plot(surveyed["stratum_region"], main="Study Area")
plot(surveyed["Region"], main="Region")

sumTemp <- surveyed %>% group_by(Region, Stratum) %>% summarise(num = n_distinct(Grid))
mean(sumTemp$num)

df_s_cat<- read.csv("data/Maine_inshore_trawl/MEscallopCatch.csv") #scallop catch
df_tows<-read.csv("data/Maine_inshore_trawl/MEtows.csv") #tow data

s_cat_Spatial <- cleanCatch(df_s_cat) %>% 
  mutate(Common_Name = "Scallop") %>% 
  select(-c("End_Latitude","End_Longitude")) %>% 
  mutate(area = paste(Region, Stratum),.before= Survey)

s_cat_sf<- st_as_sf(s_cat_Spatial, coords = c("Start_Latitude", "Start_Longitude"))
#crs=4326
#crs=4269
#crs=4267
head(s_cat_sf)
plot(s_cat_sf["Stratum"])


#spatial2015 <- s_cat_sf %>% filter(Year==2015)
#plot(spatial2015["area"])

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

surveyedNoGeom <- st_drop_geometry(surveyed) %>% select(c("OBJECTID", "stratum_region"))

neighbors_df<-data.frame(sf.sgbp.surveyed)
neighbors_df<- neighbors_df %>% 
  mutate(OBJECTID = surveyedNoGeom[row.id, "OBJECTID"], .keep="unused") %>% 
  mutate(neighborID = surveyedNoGeom[col.id, "OBJECTID"], .keep="unused") 

neighbors_df_test<- left_join(neighbors_df, surveyedNoGeom) %>% rename(objectRegion = stratum_region, objectID = OBJECTID)
neighbors_df_test <- left_join(neighbors_df_test, surveyedNoGeom, by = c("neighborID"="OBJECTID")) %>% rename(neighborRegion = stratum_region)
