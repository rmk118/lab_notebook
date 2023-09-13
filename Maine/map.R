#Map of ME-NH survey area
#Ruby Krasnow
#Last modified: Sept 13, 2023

# Load packages
library(tidyverse)
library(lubridate) #working with dates
library(sf) #for spatial data analysis/visualization

surveyGrid <-st_read("~/Downloads/lab_notebook/Maine/MaineDMR_-_Inshore_Trawl_Survey_Grid") #CRS: WGS 84/EPSG 4326

surveyGrid <- surveyGrid %>% 
  mutate(Region = as.factor(region_id),
         Stratum = as.factor(depth_stra),
         Area = paste(Region, Stratum),
         GridID = grid_id, .keep="unused", .before=last_surve)

plot(surveyGrid[1])
ggplot(data=surveyGrid)+
  geom_sf(aes(fill=Area))+
  scale_fill_viridis_d()

library(mapview)
library(paletteer)
library(leaflet)

pal_orig <- paletteer_d("colorBlindness::SteppedSequential5Steps")
pal1 <- pal_orig[c(1:4, 6:9, 11:14, 16:19, 21:24)]
palReg <- pal_orig[c(1, 6, 11, 16, 21)]
palStrat <- c("#202020", "#606060", "#9f9f9f", "#dfdfdf")

factpalReg <- colorFactor(palette=c(palReg), surveyGrid$Region)
factpalArea <- colorFactor(palette=c(pal1), domain=surveyGrid$Area)
factpalStrat <- colorFactor(palette=c(palStrat), domain=surveyGrid$Stratum)
labelRegions <- c("1: NH & S. Maine", "2: Mid-Coast", "3: Penobscot Bay", "4: Mt. Desert Area", "5: Downeast Maine")
labelStrat <- c("1: 9-37 m", "2: 38-64 m", "3: 65-101 m", "4: 101+ m")

surveyMap <- leaflet(surveyGrid, options = leafletOptions(zoomControl = FALSE)) %>%
  addProviderTiles(providers$CartoDB.Voyager) %>%
  addPolygons(stroke=FALSE,
              color = ~ factpalArea(Area),
    fillOpacity = 1
  )  %>%
  addLegend("bottomright",colors = palReg, opacity = 1, labels= labelRegions, title="Region") %>%
  addLegend("bottomright", colors=palStrat, labels = labelStrat, opacity = 1, title="Depth Stratum")

png_fl = tempfile(fileext = ".png")
mapshot(surveyMap, file = png_fl)
browseURL(png_fl)
