#Maine spatial analysis
#Ruby Krasnow
#Last modified: July 5, 2023

#spatial packages
library(sf)
library(sfheaders)

#grid_id starts again at 1 for each region
# st_layers("~/Downloads/lab_notebook/Maine/MaineDMR_-_Inshore_Trawl_Survey_Grid")
surveyGrid <-st_read("~/Downloads/lab_notebook/Maine/MaineDMR_-_Inshore_Trawl_Survey_Grid")

surveyGrid <- surveyGrid %>% 
  mutate(Region = region_id,
         Stratum = depth_stra,
         Grid = grid_id, .keep="unused", .before=last_surve)

surveyGrid$stratum_region <- paste(surveyGrid$Stratum, surveyGrid$Region)

plot(surveyGrid["Grid"], main="Grid ID")
#plot(surveyGrid["OBJECTID"], main="Object ID")
# plot(surveyGrid["Region"], main="Region")
# plot(surveyGrid["Stratum"], main="Depth Stratum")

plot(surveyGrid["stratum_region"], main="Study Area")


surveyGrid %>% group_by(Region, Stratum) %>% summarise(num = n_distinct(Grid))

surveyed <- surveyGrid %>% filter(!is.na(surveys))
plot(surveyed["stratum_region"], main="Study Area")
plot(surveyed["OBJECTID"], main="Object ID")

sumTemp <- surveyed %>% group_by(Region, Stratum) %>% summarise(num = n_distinct(Grid))
mean(sumTemp$num)

df_s_cat<- read.csv("data/Maine_inshore_trawl/MEscallopCatch.csv") #scallop catch
s_catSpatial <- cleanCatch(df_s_cat) %>% 
  mutate(Common_Name = "Scallop") %>% 
  select(-c("End_Latitude","End_Longitude")) %>% 
  mutate(area = paste(Region, Stratum),.before= Survey)

sSpatial<- st_as_sf(s_catSpatial, coords = c("Start_Latitude", "Start_Longitude"))
plot(sSpatial["Stratum"])


spatial2015 <- sSpatial %>% filter(Year==2015)
plot(spatial2015["area"])
