# Code for models and figures in the manuscript
# A holistic approach to modeling the growing fishery for Jonah crab (Cancer borealis) in the Gulf of Maine
# in prep for ICES JMS
# Ruby Krasnow
# Last modified: March 5, 2023


# Packages ----------------------------------------------------------------
#general
library(tidyverse)
library(lubridate) #date formatting

#plotting
library(patchwork) #combining plots
library(paletteer) #collection of color palettes in r

#Spatial data analysis + visualization
library(sf)
library(leaflet) #to create map of ME-NH trawl survey areas



## Fig. 1 - Landings -----------------------------------------
landings <- read_csv("data/noaa_landings.csv", col_types = "inc") %>% 
  rename(year=Year, total_value=Value) %>% 
  select(-Species) %>% 
  mutate(total_value = as.double(total_value))

ggplot(data=landings)+
  geom_line(aes(x=year, y=total_value/(10^6)))+
  theme_minimal()+
  labs(x="Year", y="Total value (million $)")+
  scale_y_continuous(limits=c(0,23), breaks=seq(0,30, 5) , expand=c(0,0))+
  theme(axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        text = element_text(size = 15))



## Fig. 2 - Survey Map -----------------------------------------
surveyGrid <-st_read("~/Downloads/lab_notebook/Maine/MaineDMR_-_Inshore_Trawl_Survey_Grid") #CRS: WGS 84/EPSG 4326

surveyGrid <- surveyGrid %>% 
  mutate(Region = as.factor(region_id),
         Stratum = as.factor(depth_stra),
         Area = paste(Region, Stratum),
         GridID = grid_id, .keep="unused", .before=last_surve)

pal_orig <- paletteer_d("colorBlindness::SteppedSequential5Steps")
pal1 <- pal_orig[c(1:4, 6:9, 11:14, 16:19, 21:24)]
palReg <- pal_orig[c(1, 6, 11, 16, 21)]
palStrat <- c("#202020", "#606060", "#9f9f9f", "#dfdfdf")

factpalReg <- colorFactor(palette=c(palReg), surveyGrid$Region)
factpalArea <- colorFactor(palette=c(pal1), domain=surveyGrid$Area)
factpalStrat <- colorFactor(palette=c(palStrat), domain=surveyGrid$Stratum)
labelRegions <- c("1: NH & S. Maine", "2: Mid-Coast", "3: Penobscot Bay", "4: Mt. Desert Area", "5: Downeast Maine")
labelStrat <- c("1: 9-37 m", "2: 38-64 m", "3: 65-101 m", "4: 101+ m")

states <-st_read("~/Downloads/lab_notebook/data/US_State_Boundaries/US_State_Boundaries.shp") #CRS: WGS 84/EPSG 4326

leaflet(surveyGrid, options = leafletOptions(zoomControl = FALSE)) %>%
  addProviderTiles(providers$Esri.WorldTerrain) %>%
  addPolygons(stroke=FALSE,
              color = ~ factpalArea(Area),
              fillOpacity = 1
  )  %>% 
  addPolylines(data=states,color="black", weight=3) %>% 
  addLegend("bottomright",colors = palReg, opacity = 1, labels= labelRegions, title="Region") %>%
  addLegend("bottomright", colors=palStrat, labels = labelStrat, opacity = 1, title="Depth Stratum") %>%
  fitBounds(lng1=-70, lat1=43, lng2=-66.8, lat2=44.4)
