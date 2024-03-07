# Code for models and figures in the manuscript
# A holistic approach to modeling the growing fishery for Jonah crab (Cancer borealis) in the Gulf of Maine
# in prep for ICES JMS
# Ruby Krasnow
# Last modified: March 7, 2023


# Packages ----------------------------------------------------------------
#general
library(tidyverse)
library(lubridate) #date formatting
library(mgcv) #Generalized Additive Models (GAMs)
library(broom.mixed) #for tidy model output
library(zoo) #for time series functions
library(rEDM) #for EDM functions

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

# Import data
raw_len<- read.csv("data/Maine_inshore_trawl/jonahLength2024.csv") #jonah crab length and sex data

length1 <- raw_len %>% 
  filter(Year > 2003) %>% 
  dplyr::select(c("Season", "Year", "Tow_Number", "Region", "Stratum", "Frequency", "Sex")) %>% 
  arrange(Season, Year, Stratum, Region, Tow_Number)

length_complete <- length1 %>% 
  group_by(Season, Year, Region, Stratum) %>% 
  complete(Sex, Tow_Number) #makes implicit NAs explicit by filling in all combinations of tow number and sex

length_complete <- length_complete %>% 
  group_by(Season, Year, Region, Stratum, Tow_Number, Sex) %>% 
  mutate(Num_Each_Sex = cumsum(Frequency)) %>% #add up the number of crabs caught per tow of each sex
  slice_tail()  %>% #retrieve the total
  replace(is.na(.),0) %>% filter(Sex %in% c("Male", "Female"))

length_perc <- length_complete %>%
  group_by(Season, Year, Region, Stratum, Tow_Number) %>% 
  mutate(Total = cumsum(Num_Each_Sex)) %>% #add both sexes to find the total number of crabs caught per tow
  slice_tail() %>% 
  mutate(perc_f = ifelse(Sex=="Female", Num_Each_Sex/Total, (1-(Num_Each_Sex/Total))), #calculate percent female
         perc_m = 1-perc_f) #calculate percent male

sex_by_area <- length_perc %>%
  group_by(Season, Year, Stratum, Region) %>% 
  summarise(perc_f = mean(perc_f, na.rm=TRUE)) %>% 
  pivot_wider(names_from="Season", values_from = "perc_f", id_cols=c("Year", "Stratum", "Region")) %>% 
  mutate(Diff = Fall-Spring, .keep="unused") %>%
  na.omit() %>% 
  filter(Year > 2004) %>% 
  ungroup()

sex_diff_geom <- stratGrid %>% left_join(sex_by_area %>% group_by(Stratum) %>% summarise(Diff=mean(Diff, na.rm = TRUE)) %>% mutate(Stratum=as.factor(Stratum)))

# FIG 6
ggplot()+
  geom_sf(data=sex_diff_geom, aes(fill=Diff))+
  labs(fill=NULL)

# gls for highly linear sex differences, can still account for autocorrelation
gls2024 <- gls(Diff ~ Stratum + Year + Region, data = sex_by_area,
            correlation = corExp(form = ~ Region + Stratum|Year))

summary(gls2024)
tidy(gls2024)

acf(resid(gls2024, type="normalized"))
Box.test(residuals(gls2024, type="normalized"), type="L")
shapiro.test(resid(gls2024, type="normalized"))

plot(gls2024)
plot(gls2024, resid(.) ~ Stratum | Region, abline = 0, cex = 0.3)
plot(gls2024, resid(.) ~ Year, abline = 0, cex = 0.3)
plot(gls2024, resid(.) ~ Year | Region, abline = 0, cex = 0.3)
plot(gls2024, resid(.) ~ Year | Stratum, abline = 0, cex = 0.3)

# Fig. 7 - Aggregate EDM -----------------------------------------------------------

catch1 <- raw_catch %>% full_join(raw_tows) %>%
  arrange(Survey, Tow_Number) %>% 
  dplyr::select(-c("Subsample_Weight_kg", "Subsample_Weight_kg_2", "Male_Wt_kg", "Female_Wt_kg","Date", "Surface_WaterTemp_DegC", "Surface_Salinity", "End_Latitude","End_Longitude", "Air_Temp", "Tow_Time")) %>%
  mutate(Number_Caught = replace_na(Number_Caught,0), #make implicit zeros explicit
         Weight_kg = replace_na(Weight_kg,0),
         Expanded_Catch = replace_na(Expanded_Catch,0),
         Expanded_Weight_kg = replace_na(Expanded_Weight_kg,0)) %>% 
  mutate(Date = date(ymd_hms(Start_Date)), .keep="unused") %>% filter(Year > 2000)

catch_agg <- catch1 %>% group_by(Season, Year) %>%
  summarise(avgCatch = mean(Expanded_Catch, na.rm=TRUE),
            avgWt = mean(Expanded_Weight_kg, na.rm=TRUE))

catch_dates <- catch_agg %>% 
ungroup() %>% complete(Season, Year) %>% 
  mutate(date=paste(Year, case_when(Season== "Fall" ~ "-11-01", Season =="Spring" ~"-05-01"), sep = "")) 


catch_ts <- ts(catch_dates %>% 
            mutate(date=as.Date(date)) %>% 
            arrange(date), frequency = 2, start=c(2001, 1))

catch_ts<- na.spline(catch_ts) #using na.spline produces better EDM results than linear interpolation via na.approx

tsdf <- data.frame(catch_ts)

EmbedDimension(dataFrame=tsdf, columns="avgCatch", target="avgCatch", lib = "1 30", pred="31 46", maxE = 7)

PredictNonlinear(dataFrame=tsdf, columns="avgCatch", target="avgCatch", lib = "1 30", pred="31 46", E=6)

smap_out <- SMap(dataFrame=tsdf, columns="avgCatch", target="avgCatch", lib = "1 30", pred="31 46", E=6, theta=6)

ComputeError(smap_out$predictions$Observations, smap_out$predictions$Predictions)

yrs <- seq(2016.5, 2023.5, 0.5)

edm_df <- data.frame(obs = smap_out$predictions$Observations, 
                     preds = smap_out$predictions$Predictions, 
                     pred_var = smap_out$predictions$Pred_Variance) %>% na.omit()

edm_df <- edm_df %>% mutate(Lo.95 = preds - 1.96*sqrt(pred_var),
                            Hi.95 = preds + 1.96*sqrt(pred_var),
                            Lo.80 = preds - 1.28*sqrt(pred_var),
                            Hi.80 = preds + 1.28*sqrt(pred_var),
                            yrs = yrs)

# FIG 7
ggplot()+
  geom_path(data = data.frame(catch_ts), aes(x = index(catch_ts), y = avgCatch), linewidth=0.6) +
  geom_path(data = edm_df, aes(x=yrs, y=preds), linewidth=0.7, color="#4f9ff0")+
  geom_ribbon(data = edm_df, aes(x = yrs, y =preds, ymin = Lo.80, ymax = Hi.80), fill = "#4f9ff0", alpha = 0.2) +
  labs(x="", y="Avg. catch/tow", fill=NULL, color=NULL)+
  ylim(c(-4.5, 36))+
  theme_classic()+
  theme(text = element_text(size=12),
        legend.position = "right",
        axis.title.y = element_text(margin = margin(0,10,0,0)))

# Fig. 8 - Multispatial EDM -----------------------------------------------------------

reg_names <- c("NH/South", "Midcoast", "Penobscot", "MDI", "Downeast")
names(reg_names) <- c(1,2,3,4,5)

strat_names <- c("9-37m", "38-64m", "65-101m", ">101m")
names(strat_names) <- c(1,2,3,4)

catch_area <- catch1 %>% group_by(Stratum, Region, Year, Season) %>%
  summarise(avgCatch = mean(Expanded_Catch, na.rm=TRUE),
            avgWt = mean(Expanded_Weight_kg, na.rm=TRUE)) %>% 
  ungroup() %>% 
  complete(Season, Year, Stratum, Region) %>%
  mutate(Season = as.factor(Season), Stratum = as.factor(Stratum), Region = as.factor(Region), Area = paste0(Region, Stratum))

catch_area_complete<- complete(data = catch_area %>% ungroup(),Region, Stratum, Season, Year) %>% 
  mutate(date=paste(Year, case_when(Season== "Fall" ~ "-11-01", Season =="Spring" ~"-05-01"), sep = ""), .before=Season) %>%
  filter(as.Date(date) > as.Date("2003-05-01")) %>% 
  ungroup() %>% 
  arrange(Area, date) %>% 
  group_by(Region, Stratum, Area, Season) %>% 
  mutate(avgCatch = na.spline(avgCatch),avgWt = na.spline(avgWt)) %>% 
  ungroup() 



areaList <- c(11, 12, 13, 14, 21, 22, 23, 24, 31, 32, 33, 34, 41, 42, 43, 44, 51, 52, 53, 54)

areaE <- map_dfr(areaList, function(x) {
  reg <- substr(x, 1, 1)
  strat <- substr(x, 2, 2)
  v <- catch_area_complete %>% filter(Region==reg, Stratum==strat) %>% pull(avgCatch)
  lib_vec <- paste(1, length(v))
  indices <- c(1:length(v))
  df <- data.frame(indices,v)
  colnames(df)<-c("index", "value")
  rho_E<- EmbedDimension(dataFrame = df, lib = lib_vec, pred = lib_vec, columns = "value",target = "value", maxE = 7)
  rho_E %>% mutate(Region = reg, Stratum = strat)
})

areaE <- areaE %>% mutate(method = "orig")

delay <- function(x,n){
  if(n>=0) 
    lead(x,n) 
  else 
    lag(x,abs(n))
}

areaList_E <- expand_grid(E = c(1:7), reg = c(1:5), strat = c(1:4)) %>% arrange(reg, strat)

make_block_ID <- function(df,predictor,target,ID_col,E,cause_lag=1){
  
  v_delays <- 0:-(E-1)
  
  df_ccm <- df %>%
    dplyr::select({{ID_col}},{{target}}) %>%
    group_by({{ID_col}}) %>%
    transmute(target=delay({{target}},cause_lag))
  
  df_lags <- map_dfc(v_delays,function(d_i){
    df %>%
      group_by({{ID_col}}) %>%
      transmute("pred_t{d_i}" := delay({{predictor}},d_i)) %>%
      ungroup({{ID_col}}) %>% dplyr::select(-{{ID_col}})
  })
  
  df_out <- bind_cols(df_ccm,df_lags) %>% 
    ungroup() %>%
    mutate(index=row_number()) %>%
    dplyr::select(index,everything())
  
  return(df_out)
  
}


catch_area_complete <- catch_area_complete %>% mutate(Region = as.integer(Region), Stratum = as.integer(Stratum))

# Adding the other four regions to the library of points used to make predictions about the target region
# i.e., concatenation OF regions WITHIN a stratum

areaReg <- pmap_dfr(areaList_E, function(E, reg, strat) {
  v <- catch_area_complete %>% 
    filter(Stratum == strat) %>% 
    dplyr::select(Region,avgCatch)
  
  block <- make_block_ID(df=v, predictor = avgCatch, target = avgCatch, ID_col = Region, E=E) %>% na.omit() %>% mutate(index = row_number())
  
  columns_i <- names(block)[4:(E+3)]
  
  lib_whole <- paste(1, nrow(block))
  first <- block %>% filter(Region == reg) %>% slice_head() %>% pull(index)
  last <- block %>% filter(Region == reg) %>% slice_tail() %>% pull(index)
  lib_pred <- paste(first, last)
  
  out_i <- Simplex(dataFrame=block,
                   lib=lib_whole,
                   pred=lib_pred,
                   Tp=0, # The target has already been manually lagged
                   target="target",
                   columns=columns_i,
                   embedded=TRUE,
                   E=E)
  out <- data.frame(ComputeError(out_i$Observations, out_i$Predictions)$rho) %>% 
    mutate(E = E, Region = reg, Stratum = strat) 
  out
  
})
areaReg <- areaReg %>% rename(rho = ComputeError.out_i.Observations..out_i.Predictions..rho) %>% mutate(method = "cat_of_reg_in_strat")

areaE <- rbind(areaE, areaReg)

# Adding the other three strata to the library of points used to make predictions about the target stratum
# i.e., concatenation OF strata WITHIN a region

areaStrat <- pmap_dfr(areaList_E, function(E, reg, strat) {
  v <- catch_area_complete %>% 
    filter(Region == reg) %>% 
    dplyr::select(Stratum,avgCatch)
  
  block <- make_block_ID(df=v, predictor = avgCatch, target = avgCatch, ID_col = Stratum, E=E) %>% na.omit() %>% mutate(index = row_number())
  
  columns_i <- names(block)[4:(E+3)]
  
  lib_whole <- paste(1, nrow(block))
  first <- block %>% filter(Stratum == strat) %>% slice_head() %>% pull(index)
  last <- block %>% filter(Stratum == strat) %>% slice_tail() %>% pull(index)
  lib_pred <- paste(first, last)
  
  out_i <- Simplex(dataFrame=block,
                   lib=lib_whole,
                   pred=lib_pred,
                   Tp=0, # The target has already been manually lagged
                   target="target",
                   columns=columns_i,
                   embedded=TRUE,
                   E=E)
  out <- data.frame(ComputeError(out_i$Observations, out_i$Predictions)$rho) %>% 
    mutate(E = E, Region = reg, Stratum = strat) 
  out
  
})

areaStrat <- areaStrat %>% rename(rho = ComputeError.out_i.Observations..out_i.Predictions..rho) %>% mutate(method = "cat_of_strat_in_reg")

areaE <- rbind(areaE, areaStrat)

# FIG 8
ggplot()+
  geom_abline(intercept = 0, slope = 0, lty=2) +
  geom_line(data=areaE, aes(x=E, y=rho, color=method))+
  facet_grid(Region~Stratum, labeller = labeller(Region = reg_names, Stratum=strat_names))+
  theme_light()+
  labs(y="Prediction skill (\U03C1)", x="Embedding dimension")+
  scale_y_continuous(breaks=c(-0.2, 0, 0.2, 0.4, 0.6), labels=c("-0.2","", "0.2", "","0.6"))+
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        text = element_text(size = 15),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "white", color="black"),
        strip.text = element_text(colour = "black"))+
  scale_color_brewer( name="Method",  palette = "Dark2",
                      labels=c('Including data from other regions', 'Including data from other strata', 'Single time series'),
                      guide = guide_legend(reverse = TRUE))



# Fig. S1 --------------------------------------------------------------

gam_wt_2024plot1+gam_wt_2024plot2 +
  plot_annotation(tag_levels = 'A') + plot_layout(nrow=2,ncol=2)

# Fig. S2 --------------------------------------------------------------

E_by_method <- areaE %>%
  group_by(Region, Stratum, method) %>% 
  slice_max(rho)

density_E <- ggplot(data=E_by_method)+
  geom_density(aes(x=E, color=method))+
  theme_bw()+
  labs(x="E", y="Density")+
  scale_color_brewer( name="Method",  palette = "Dark2",
                      labels=c('Including data from other regions', 'Including data from other strata', 'Single time series'),
                      guide = guide_legend(reverse = TRUE))

density_rho <- ggplot(data=E_by_method)+
  geom_density(aes(x=rho, color=method))+
  theme_bw()+
  labs(x="Prediction skill (\U03C1)", y="Density")+
  scale_color_brewer( name="Method",  palette = "Dark2",
                      labels=c('Including data from other regions', 'Including data from other strata', 'Single time series'),
                      guide = guide_legend(reverse = TRUE))

# FIG S2
density_E + density_rho +
  plot_layout(guides = 'collect') &
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        text = element_text(size = 12),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "white", color="black"),
        strip.text = element_text(colour = "black"))
