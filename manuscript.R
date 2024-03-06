# Code for models and figures in the manuscript
# A holistic approach to modeling the growing fishery for Jonah crab (Cancer borealis) in the Gulf of Maine
# in prep for ICES JMS
# Ruby Krasnow
# Last modified: March 6, 2023


# Packages ----------------------------------------------------------------
#general
library(tidyverse)
library(lubridate) #date formatting
library(mgcv) #Generalized Additive Models (GAMs)


#plotting
library(patchwork) #combining plots
library(paletteer) #collection of color palettes in r
library(gratia) #visualization of GAMs

#Spatial data analysis + visualization
library(sf)
library(spdep)
library(leaflet) #to create map of ME-NH trawl survey areas



# Fig. 1 - Landings -----------------------------------------
landings <- read_csv("data/noaa_landings.csv", col_types = "inc") %>% 
  rename(year=Year, total_value=Value) %>% 
  select(-Species) %>% 
  mutate(total_value = as.double(total_value))

# FIG 1
ggplot(data=landings)+
  geom_line(aes(x=year, y=total_value/(10^6)))+
  theme_minimal()+
  labs(x="Year", y="Total value (million $)")+
  scale_y_continuous(limits=c(0,23), breaks=seq(0,30, 5) , expand=c(0,0))+
  theme(axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        text = element_text(size = 15))



# Fig. 2 - Survey Map -----------------------------------------
surveyGrid <-st_read("~/Downloads/lab_notebook/Maine/MaineDMR_-_Inshore_Trawl_Survey_Grid") #CRS: WGS 84/EPSG 4326

surveyGrid <- surveyGrid %>% 
  mutate(Region = as.factor(region_id),
         Stratum = as.factor(depth_stra),
         Area = paste(Region, Stratum),
         GridID = grid_id, .keep="unused", .before=last_surve)

regionsGrid <- surveyGrid %>% group_by(Region, Stratum) %>% summarise(num = n_distinct(GridID))
stratGrid <- surveyGrid %>% group_by(Stratum) %>% summarise(num = n_distinct(GridID))

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

# FIG 2
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



# Fig. 3 - Relative Abundance -----------------------------------------
raw_catch <- read.csv("data/Maine_inshore_trawl/jonahCatch2024.csv") #Jonah crab catch data
raw_tows<-read.csv("data/Maine_inshore_trawl/MEtows.csv") #tow data

catch1 <- raw_catch %>% full_join(raw_tows) %>%
  arrange(Survey, Tow_Number) %>% 
  dplyr::select(-c("Subsample_Weight_kg", "Subsample_Weight_kg_2", "Male_Wt_kg", "Female_Wt_kg","Date", "Surface_WaterTemp_DegC", "Surface_Salinity", "End_Latitude","End_Longitude", "Air_Temp", "Tow_Time")) %>%
  mutate(Number_Caught = replace_na(Number_Caught,0), #make implicit zeros explicit
         Weight_kg = replace_na(Weight_kg,0),
         Expanded_Catch = replace_na(Expanded_Catch,0),
         Expanded_Weight_kg = replace_na(Expanded_Weight_kg,0)) %>% 
  mutate(Date = date(ymd_hms(Start_Date)), .keep="unused") %>% filter(Year > 2002)

catch_regions <- catch1 %>% group_by(Stratum, Region) %>%
  summarise(avgCatch = mean(Expanded_Catch, na.rm=TRUE),
            avgWt = mean(Expanded_Weight_kg, na.rm=TRUE)) %>% na.omit()

catch_strat <- catch1 %>% group_by(Stratum) %>%
  summarise(avgCatch = mean(Expanded_Catch, na.rm=TRUE),
            avgWt = mean(Expanded_Weight_kg, na.rm=TRUE)) %>% 
  mutate(Stratum=as.factor(Stratum))

catch_regions_geom <- left_join(regionsGrid, catch_regions  %>% 
                                  mutate(Region=as.factor(Region), Stratum=as.factor(Stratum)))
catch_strat_geom <- left_join(stratGrid, catch_strat)

fig3A <- ggplot(data=catch_regions_geom)+geom_sf(aes(fill=avgCatch))+
  scale_fill_viridis_c()+
  labs(fill="Avg catch/tow")

fig3B <-ggplot(data=catch_strat_geom)+geom_sf(aes(fill=avgCatch))+
  scale_fill_viridis_c()+
  labs(fill="Avg catch/tow")

# FIG 3
fig3A + fig3B +
  plot_annotation(tag_levels = 'A') + plot_layout(ncol=2) & 
  theme(plot.margin = margin(t = 0.2,  # Top margin
                             r = 0.2,  # Right margin
                             b = 0.3,  # Bottom margin
                             l = 0.2,  # Left margin
                             unit = "cm"))


# Fig. 4 - GAM -----------------------------------------------------------

catch_areas <- catch_regions_geom %>% mutate(Area = as.factor(paste(Region, Stratum, sep=" ")))

# Demonstrate spatial autocorrelation with Global Moran's I
nb <- poly2nb(catch_areas, row.names = catch_areas$Area) # queen shares point or border
attr(nb, "region.id") <- catch_areas$Area
names(nb) = attr(nb, "region.id")
names(nb)
nbw <- nb2listw(nb, style = "W")
moran.test(catch_areas$avgCatch, nbw)
moran.test(catch_areas$avgWt, nbw)

gam_wt_2024 <- gamm(avgWt ~ Region + s(Stratum, k=4, bs="cr"),
              correlation=corGaus(form=~ Region + Stratum),
              data=catch_regions %>% ungroup())

summary(gam_wt_2024$gam)

gam_catch_2024 <- gamm(avgCatch ~ Region + s(Stratum, k=4, bs="cr"),
                    correlation=corGaus(form=~ Region + Stratum),
                    data=catch_regions %>% ungroup())

summary(gam_catch_2024$gam)

# All model diagnostics look good (replace wt with catch to see results for other model)
appraise(gam_wt_2024$gam)
shapiro.test(resid(gam_wt_2024$lme, type="normalized"))
Box.test(resid(gam_wt_2024$lme, type="normalized"), type="L")
acf(resid(gam_wt_2024$lme, type="normalized"))
pacf(resid(gam_wt_2024$lme, type="normalized"))


gam_wt_2024plot1 <- draw(gam_wt_2024$gam)+
  theme_bw()+
  theme(axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        text = element_text(size = 13))

plot.para_wt <- termplot(gam_wt_2024$gam, se = TRUE, plot = FALSE)

gam_wt_2024plot2 <- ggplot(data=plot.para_wt$Region)+
  geom_line(aes(x=x,y=y))+
  geom_ribbon(data = plot.para_wt$Region, aes(x=x, y=y, ymin = y-se, ymax = y+se), alpha = 0.2) +
  labs(x="Region", y="Partial effect")+
  theme_bw()+
  ggtitle("Region")+
  theme(axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10, l=20)),
        text = element_text(size = 13))

gam_catch_2024plot1 <- draw(gam_catch_2024$gam)+
  theme_bw()+
  theme(axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        text = element_text(size = 13))

plot.para_catch <- termplot(gam_catch_2024$gam, se = TRUE, plot = FALSE)

gam_catch_2024plot2 <- ggplot(data=plot.para_catch$Region)+
  geom_line(aes(x=x,y=y))+
  geom_ribbon(data = plot.para_catch$Region, aes(x=x, y=y, ymin = y-se, ymax = y+se), alpha = 0.2) +
  labs(x="Region", y="Partial effect")+
  theme_bw()+
  ggtitle("Region")+
  theme(axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10, l=20)),
        text = element_text(size = 13))

# FIG 4
gam_catch_2024plot1+gam_catch_2024plot2 +
  plot_annotation(tag_levels = 'A') + plot_layout(nrow=2,ncol=2)

#Supp. figure X
gam_wt_2024plot1+gam_wt_2024plot2 +
  plot_annotation(tag_levels = 'A') + plot_layout(nrow=2,ncol=2)


# Fig. 5 - Seasonal differences -----------------------------------------------------------

catch_seasonal <- catch1 %>% group_by(Stratum, Year, Season) %>%
  summarise(avgCatch = mean(Expanded_Catch, na.rm=TRUE)) %>% 
  ungroup() %>% complete(Season, Stratum) %>% 
  filter(!(is.na(Stratum)))

seasonal_diffs <- catch_seasonal %>% 
  pivot_wider(names_from = "Season", id_cols = c("Stratum", "Year"), values_from = "avgCatch")  %>% 
  mutate(diff = Fall-Spring,
         pdiff=diff/Fall) %>% filter(Year!=2020) %>% 
  group_by(Stratum) %>% 
  summarise(med_diff = median(diff, na.rm=TRUE),
            med_pdiff = median(pdiff, na.rm=TRUE)) %>% 
  mutate(Stratum=as.factor(Stratum))

seasonal_diffs <- left_join(stratGrid, seasonal_diffs)

# Jonah catch/tow fall-spring by stratum - median
abs_diff <- ggplot()+
  geom_sf(data=seasonal_diffs, aes(fill=med_diff))+
  labs(fill="Abs. diff")+
  theme(axis.text.x = element_text(size = 10))+
  scale_fill_fermenter(palette = "Blues", direction = 1)+
  ggtitle("Seasonal difference in catch/tow")

# Jonah catch/tow (fall-spring)/fall by stratum - median
perc_diff <- ggplot()+
  geom_sf(aes(fill=med_pdiff*100), data=seasonal_diffs)+
  labs(fill="% diff")+
  theme(axis.text.x = element_text(size = 10))+
  scale_fill_fermenter(palette = "Blues", direction = 1)+
  ggtitle("Seasonal percent difference in catch/tow")

# FIG 5
abs_diff+perc_diff+
  plot_annotation(tag_levels = 'A')

# Fig. 6 - Sex ratio -----------------------------------------------------------
