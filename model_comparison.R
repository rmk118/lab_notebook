# Comparing EDM models to linear models + Poster/presentation figures
# Example: Jonah crab abundance in the Gulf of Maine
# Ruby Krasnow
# Last modified: Aug 17, 2023

#Load packages
library(tidyverse)
library(lubridate) #date formatting
library(patchwork) #combining plots
library(scales) #plot scales
library(sf) #spatial analysis
library(car)
library(mgcv)

## Time series packages
library(rEDM) #EDM
library(tseries)
library(forecast)
library(zoo)


#Import data
df_tows<-read.csv("data/Maine_inshore_trawl/MEtows.csv") #tow data
df_j_cat<- read.csv("data/Maine_inshore_trawl/MEjonahCatch.csv") #Jonah crab catch data
df_s_cat<- read.csv("data/Maine_inshore_trawl/MEscallopCatch.csv") #Scallop catch data
df_r_cat<- read.csv("data/Maine_inshore_trawl/MErockCatch.csv") #Atlantic rock crab catch data

# Remove unused columns
cleanCatch <- function(x) {
  full_join(x, df_tows) %>%
    arrange(Survey, Tow_Number) %>% 
    dplyr::select(-c("Stratum", "Subsample_Weight_kg", "Subsample_Weight_kg_2", "Male_Wt_kg", "Female_Wt_kg","Date", "Surface_WaterTemp_DegC", "Surface_Salinity", "End_Latitude","End_Longitude", "Air_Temp", "Tow_Time")) %>%
    mutate(Number_Caught = replace_na(Number_Caught,0), #make implicit zeros explicit
           Weight_kg = replace_na(Weight_kg,0),
           Expanded_Catch = replace_na(Expanded_Catch,0),
           Expanded_Weight_kg = replace_na(Expanded_Weight_kg,0)) %>% 
    mutate(Stratum = Depth_Stratum, Date = date(ymd_hms(Start_Date)), .keep="unused")
}

s_cat_clean_seasons <- cleanCatch(df_s_cat) %>% 
  mutate(Common_Name = "Scallop")

r_cat_clean_seasons <- cleanCatch(df_r_cat) %>% 
  mutate(Common_Name = "Rock")

j_cat_clean_seasons <- cleanCatch(df_j_cat) %>% 
  mutate(Common_Name = "Jonah")

# Aggregate ---------------------------------------------------------------

summaryAgg <- function(df) {
  df %>% group_by(Season, Year) %>%
    summarise(avgCatch = mean(Expanded_Catch, na.rm=TRUE),
              avgWt = mean(Expanded_Weight_kg, na.rm=TRUE),
              temp = mean(Bottom_WaterTemp_DegC, na.rm=TRUE)) 
}

#computes averages
j_cat_agg <- summaryAgg(j_cat_clean_seasons)
r_cat_agg <- summaryAgg(r_cat_clean_seasons)
s_cat_agg <- summaryAgg(s_cat_clean_seasons)

catch_agg <- s_cat_agg %>% left_join(j_cat_agg, by=c("Season", "Year", "temp"), suffix = c("_s", "_j"))

catch_agg <- catch_agg %>% left_join(r_cat_agg, by=c("Season", "Year", "temp")) %>% 
  mutate(avgCatch_r = avgCatch,avgWt_r = avgWt, .keep="unused") %>% ungroup() %>% complete(Season, Year)

catchTidy_agg <- pivot_longer(catch_agg, 
                              cols = starts_with("avg")) %>% 
  mutate(Type = case_when(
    startsWith(name, "avgCatch_") ~"catch",
    startsWith(name,"avgWt_") ~"wt")) %>% 
  mutate(Species = case_when(
    endsWith(name, "s") ~"scallop",
    endsWith(name, "r") ~"rock",
    endsWith(name, "j") ~"jonah")) %>% ungroup() %>% complete(Season, Year)

catchTidy_agg <- catchTidy_agg %>% 
  mutate(Species = as.factor(Species),Season = as.factor(Season)) %>% 
  dplyr::select(-name) %>%
  mutate(date=paste(Year, case_when(Season== "Fall" ~ "-11-01", Season =="Spring" ~"-05-01"), sep = "")) %>% 
  filter(Year > 2000)

catch_agg <- catch_agg %>% 
  mutate(date=paste(Year, case_when(Season== "Fall" ~ "-11-01", Season =="Spring" ~"-05-01"), sep = "")) %>% 
  filter(Year > 2000)

ts1 <- ts(catchTidy_agg %>% filter(Species=="jonah", Type=="catch") %>% mutate(date=as.Date(date)) %>% arrange(date), frequency = 2, start=c(2001, 1))

ts1<- na.spline(ts1) #using na.spline produces better EDM results than linear interpolation via na.approx


# ARIMA vs S-Map ----------------------------------------------------------
ts_val <- ts1[,c("Season", "Year", "value", "date")]

tsdf <- data.frame(ts_val)
ggplot(data=tsdf)+geom_line(aes(x=date, y=value))

## EDM model ----

EmbedDimension(dataFrame=tsdf, columns="value", target="value", lib = "1 30", pred="31 44")

PredictNonlinear(dataFrame=tsdf, columns="value", target="value", lib = "1 30", pred="31 44", E=3) #ballpark
PredictNonlinear(dataFrame=tsdf, columns="value", target="value", lib = "1 30", pred="31 44", E=3, #slightly more specific
                 theta="0.85 0.9 0.95 1 1.05 1.1") %>% filter(rho == max(rho))
 
s_out <- SMap(dataFrame=tsdf, columns="value", target="value", lib = "1 30", pred="31 44", E=3, theta=1.05)

## ARIMA ----
train <- 1:30

fit_train <- auto.arima(ts_val[train,"value"])
summary(fit_train)
checkresiduals(fit_train)

yrs <- seq(2016, 2022.5, 0.5)

forecast(fit_train, h=14)
arima_preds <- data.frame(forecast(fit_train, h=14)) %>% mutate(yrs = yrs)

# Compare stats - EDM has higher rho, lower error

compute_stats(s_out$predictions$Observations[1:14], arima_preds$Point.Forecast)

# ASK ETHAN WHY THESE ARE DIFFERENT
compute_stats(s_out$predictions$Observations, s_out$predictions$Predictions)
compute_stats(s_out$predictions$Observations[1:14], s_out$predictions$Predictions[2:15])
ComputeError(s_out$predictions$Observations, s_out$predictions$Predictions)
ComputeError(s_out$predictions$Observations[1:14], s_out$predictions$Predictions[2:15])

(tsdf[31:44, "value"] == s_out$predictions$Observations[1:14]) #sanity check



# Poster figures ----------------------------------------------------------

## Fig. 2 - Landings -----------------------------------------
# Prep: poster and slides fig. 2 - Jonah crab landings in Maine
landings <- read.csv("data/MaineDMR_Landings.csv")
ggplot(data=landings)+geom_line(aes(x=year, y=total_weight, color=species))


ggplot(data=landings %>% filter(species=="Crab Jonah"))+
  geom_line(aes(x=year, y=total_value))+
  theme_classic()+
  labs(x="Year", y="Total value ($)")+
  scale_y_continuous(limits=c(0,2E6), breaks=seq(0,2E6, 5E5) , expand=c(0,0), labels=comma)+
  theme(axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        text = element_text(size = 14))

# Fig. 3 on poster is map of survey area
# Fig. 3 on slides is Deyle et al., 2018 time delay diagram
# Fig. 4 on poster and slides is attractor reconstruction diagram

## Fig. 5A - E (agg.)-----------------------------------------
#Prep: Poster fig. 5 and slides 5a - Embedding dimension for aggregate catch
fig <- EmbedDimension(dataFrame=tsdf, columns="value", target="value", lib = "1 44", pred="1 44")

fig5a <- ggplot(data=fig, aes(x=E, y=rho))+
  geom_line()+
  scale_x_continuous(breaks=seq(0,10,2))+
  theme_classic()+
  labs(y="Prediction skill (\U03C1)", x="Embedding dimension")+
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))+
  theme(text = element_text(size = 14))


## Fig. 6 - EDM vs ARIMA -------------------------------------
edm_df <- data.frame(obs = s_out$predictions$Observations[1:14], 
                     preds = s_out$predictions$Predictions[2:15], 
                     pred_var = s_out$predictions$Pred_Variance[2:15])

edm_df <- edm_df %>% mutate(Lo.95 = preds - 1.96*sqrt(pred_var),
                            Hi.95 = preds + 1.96*sqrt(pred_var),
                            Lo.80 = preds - 1.28*sqrt(pred_var),
                            Hi.80 = preds + 1.28*sqrt(pred_var),
                            yrs = yrs)

### For poster
edmPlot_manual <- ggplot()+
  geom_path(data = data.frame(ts_val), aes(x = index(ts_val), y = value), linewidth=0.6) +
  geom_path(data = edm_df, aes(x=yrs, y=preds,color="EDM"), linewidth=0.7)+
  geom_ribbon(data = edm_df, aes(x = yrs, y =preds, ymin = Lo.95, ymax = Hi.95), fill = "#4f9ff0", alpha = 0.2) +
  labs(x="", y="Avg. catch/tow")+
  scale_color_manual(values=c("#4f9ff0"), name="")+ylim(c(-8, 25))+theme_classic()+
  theme(text = element_text(size=12),
        legend.position = "right",
        axis.title.y = element_text(margin = margin(0,10,0,0)))
edmPlot_manual #6A

arimaPlot_manual <- ggplot()+
  geom_path(data = data.frame(ts_val), aes(x = index(ts_val), y = value), linewidth=0.6) +
  geom_path(data = arima_preds, aes(x=yrs, y=Point.Forecast,color="ARIMA"), linewidth=0.7)+
  geom_ribbon(data = arima_preds, aes(x = yrs, y = Point.Forecast, ymin = Lo.95, ymax = Hi.95), fill = "#c93237", alpha = 0.2) +
  labs(x="Year", y="Avg. catch/tow")+
  scale_color_manual(values=c("#c93237"), name="")+ylim(c(-8, 25))+theme_classic()+
 #theme(legend.position = "none",
     theme(text = element_text(size=12),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(0,10,0,0)))
arimaPlot_manual #6B

# Figure 6 (poster) combined A and B
(edmPlot_manual/arimaPlot_manual) +
  plot_layout(guides = 'collect')+
   plot_annotation(tag_levels = 'A')

### For slides
# Prep: slides fig. 6 - individual graphs figure plus no-confidence interval combined plot
edmPlot_manual2 <- ggplot()+
  geom_path(data = data.frame(ts_val), aes(x = index(ts_val), y = value)) +
  geom_path(data = edm_df, aes(x=yrs, y=preds,color="line"))+
  geom_ribbon(data = edm_df, aes(x = yrs, y =preds, ymin = Lo.95, ymax = Hi.95), fill = "#009ade", alpha = 0.2) +
  labs(x="Year", y="Avg. catch/tow")+
  scale_color_manual(values=c("#009ade"))+
  ylim(c(-8, 25))+
  theme_classic()+
  theme(legend.position = "none")
edmPlot_manual2 #6A

arimaPlot_manual2 <- ggplot()+
  geom_path(data = data.frame(ts_val), aes(x = index(ts_val), y = value)) +
  geom_path(data = arima_preds, aes(x=yrs, y=Point.Forecast,color="line"))+
  geom_ribbon(data = arima_preds, aes(x = yrs, y = Point.Forecast, ymin = Lo.95, ymax = Hi.95), fill = "#ff1f5b", alpha = 0.2) +
  labs(x="Year", y="Avg. catch/tow")+
  scale_color_manual(values=c("#ff1f5b"))+
  ylim(c(-8, 25))+
  theme_classic()+
  theme(legend.position = "none")
arimaPlot_manual2 #6B

both_noCI <-ggplot()+
  theme_classic()+
  geom_path(data = data.frame(ts_val), aes(x = index(ts_val), y = value)) +
  geom_path(data = edm_df, aes(x=yrs, y=preds, color="EDM"), size=1)+
  geom_path(data=arima_preds, aes(x=yrs, y=Point.Forecast, color="ARIMA"), size=1)+
  labs(y="Avg. catch/tow", x="Year")+
  scale_color_manual(name="Model", values=c("#FF1f5B", "#009ade"))+
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))+
  theme(text = element_text(size = 12))
both_noCI #6C

# Figure 6 (slides) combined A, B, and C
(((arimaPlot_manual2/edmPlot_manual2) +  plot_layout(guides = 'collect')) |  both_noCI  ) +
  plot_annotation(tag_levels = 'A') +   plot_layout(widths = c(1, 2.5))


# Strata analysis ---------------------------------------------------------

surveyGrid <-st_read("~/Downloads/lab_notebook/Maine/MaineDMR_-_Inshore_Trawl_Survey_Grid") #CRS: WGS 84/EPSG 4326

surveyGrid <- surveyGrid %>% 
  mutate(Region = region_id,
         Stratum = depth_stra,
         GridID = grid_id, .keep="unused", .before=last_surve)

summaryStrat <- function(df) {
  df %>% group_by(Season, Year, Stratum) %>%
    summarise(avgCatch = mean(Expanded_Catch, na.rm=TRUE),
              avgWt = mean(Expanded_Weight_kg, na.rm=TRUE),
              temp = mean(Bottom_WaterTemp_DegC, na.rm=TRUE))
}

#computes averages for each stratum
j_cat_strat <- summaryStrat(j_cat_clean_seasons) %>% ungroup() %>% complete(Season, Year, Stratum)
r_cat_strat <- summaryStrat(r_cat_clean_seasons) %>% ungroup() %>% complete(Season, Year, Stratum)
s_cat_strat <- summaryStrat(s_cat_clean_seasons) %>% ungroup() %>% complete(Season, Year, Stratum)

catch_strat <- s_cat_strat %>% left_join(j_cat_strat, by=c("Season", "Stratum", "Year", "temp"), suffix = c("_s", "_j"))

catch_strat <- catch_strat %>% left_join(r_cat_strat, by=c("Season", "Stratum", "Year", "temp")) %>%
  mutate(avgCatch_r = avgCatch,avgWt_r = avgWt, .keep="unused")

catchTidy_strat <- pivot_longer(catch_strat,
                                cols = starts_with("avg")) %>%
  mutate(Type = case_when(
    startsWith(name, "avgCatch_") ~"catch",
    startsWith(name,"avgWt_") ~"wt")) %>%
  mutate(Species = case_when(
    endsWith(name, "s") ~"scallop",
    endsWith(name, "r") ~"rock",
    endsWith(name, "j") ~"jonah"))

catchTidy_strat <- catchTidy_strat %>%
  mutate(Species = as.factor(Species),Season = as.factor(Season), Stratum = as.factor(Stratum)) %>%
  dplyr::select(-name)

catchTidy_strat_complete<- complete(data = catchTidy_strat %>% ungroup(),Stratum, Season, Year) %>% 
  mutate(date=paste(Year, case_when(Season== "Fall" ~ "-11-01", Season =="Spring" ~"-05-01"), sep = ""), .before=Stratum) %>%
  filter(as.Date(date) > as.Date("2003-05-01")) %>% ungroup()


## Fig. 5B - E (strat) --------------------------------------------------
catchTidy_strat_complete_j <- catchTidy_strat_complete %>% 
  filter(Species =="jonah") %>% 
  arrange(Stratum, date) %>% 
  group_by(Stratum, Season, Type) %>% 
  mutate(temp = na.spline(temp),value = na.spline(value)) %>% 
  ungroup() 

ggplot(data=catchTidy_strat_complete_j %>% filter(Type=="catch"), aes(x=date, y=value))+
  geom_line(data=catchTidy_strat_complete_j %>% filter(Type=="catch"), aes(x=date, y=value, group=Stratum, color=Stratum))

stratE <- map_dfr(c(1:4), function(x) {
  v <- catchTidy_strat_complete_j %>% filter(Type=="catch", Stratum==x) %>% pull(value)
  lib_vec <- paste(1, length(v))
  indices <- c(1:length(v))
  df <- data.frame(indices,v)
  colnames(df)<-c("index", "value")
  rho_E<- EmbedDimension(dataFrame = df, lib = lib_vec, pred = lib_vec, columns = "value",target = "value", maxE = 7)
  rho_E %>% mutate(Stratum = paste("Stratum ",x))
})

#Figure 5B
fig5b <- ggplot()+
  geom_line(data=stratE, aes(x=E, y=rho))+
  facet_wrap(~Stratum)+
  theme_bw()+
  labs(y="Prediction skill (\U03C1)", x="Embedding dimension")+
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))+
  theme(text = element_text(size = 14))
fig5b


# Area analysis --------------------------------------------------


summaryArea <- function(df) {
  df %>% group_by(Season, Year, Stratum, Region) %>%
    summarise(avgCatch = mean(Expanded_Catch, na.rm=TRUE),
              avgWt = mean(Expanded_Weight_kg, na.rm=TRUE),
              temp = mean(Bottom_WaterTemp_DegC, na.rm=TRUE))
}

#computes averages for each Area
j_cat_area <- summaryArea(j_cat_clean_seasons) %>% ungroup() %>% complete(Season, Year, Stratum, Region)
r_cat_area <- summaryArea(r_cat_clean_seasons) %>% ungroup() %>% complete(Season, Year, Stratum, Region)
s_cat_area <- summaryArea(s_cat_clean_seasons) %>% ungroup() %>% complete(Season, Year, Stratum, Region)

catch_area <- s_cat_area %>% left_join(j_cat_area, by=c("Season", "Stratum", "Region", "Year", "temp"), suffix = c("_s", "_j"))

catch_area <- catch_area %>% left_join(r_cat_area, by=c("Season", "Stratum", "Region", "Year", "temp")) %>%
  mutate(avgCatch_r = avgCatch,avgWt_r = avgWt, .keep="unused")

catchTidy_area <- pivot_longer(catch_area,
                                cols = starts_with("avg")) %>%
  mutate(Type = case_when(
    startsWith(name, "avgCatch_") ~"catch",
    startsWith(name,"avgWt_") ~"wt")) %>%
  mutate(Species = case_when(
    endsWith(name, "s") ~"scallop",
    endsWith(name, "r") ~"rock",
    endsWith(name, "j") ~"jonah"))

catchTidy_area <- catchTidy_area %>%
  mutate(Species = as.factor(Species),Season = as.factor(Season), Stratum = as.factor(Stratum), Region = as.factor(Region), Area = paste0(Region, Stratum)) %>%
  dplyr::select(-name)

catchTidy_area_complete<- complete(data = catchTidy_area %>% ungroup(),Region, Stratum, Season, Year) %>% 
  mutate(date=paste(Year, case_when(Season== "Fall" ~ "-11-01", Season =="Spring" ~"-05-01"), sep = ""), .before=Season) %>%
  filter(as.Date(date) > as.Date("2003-05-01")) %>% ungroup()

catchTidy_area_complete_j <- catchTidy_area_complete %>% 
  filter(Species =="jonah") %>% 
  arrange(Area, date) %>% 
  group_by(Region, Stratum, Area, Season, Type) %>% 
  mutate(temp = na.spline(temp),value = na.spline(value)) %>% 
  ungroup() 



## Fig. 5C - E (areas) --------------------------------------------------

areaList <- c(11, 12, 13, 14, 21, 22, 23, 24, 31, 32, 33, 34, 41, 42, 43, 44, 51, 52, 53, 54)

areaE <- map_dfr(areaList, function(x) {
  reg <- substr(x, 1, 1)
  strat <- substr(x, 2, 2)
  v <- catchTidy_area_complete_j %>% filter(Type=="catch", Area==x) %>% pull(value)
  lib_vec <- paste(1, length(v))
  indices <- c(1:length(v))
  df <- data.frame(indices,v)
  colnames(df)<-c("index", "value")
  rho_E<- EmbedDimension(dataFrame = df, lib = lib_vec, pred = lib_vec, columns = "value",target = "value", maxE = 7)
  rho_E %>% mutate(Region = reg, Stratum = strat)
})

areaE <- areaE %>% mutate(method = "orig")

#Figure 5C - single time series only
ggplot()+
  geom_line(data=areaE %>% filter(method=="orig"), aes(x=E, y=rho))+
  facet_grid(Region~Stratum)+
theme_light()+
  labs(y="Prediction skill (\U03C1)", x="Embedding dimension")+
  scale_y_continuous(breaks=c(-0.2, 0, 0.2, 0.4, 0.6), labels=c("-0.2","", "0.2", "","0.6"))+
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))+
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        text = element_text(size = 14),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "white", color="black"),
        strip.text = element_text(colour = "black"))

# Concatenation -----------------------------------------------------------

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


catchTidy_area_complete_j <- catchTidy_area_complete_j %>% mutate(Region = as.integer(Region), Stratum = as.integer(Stratum))

# Adding the other four regions to the library of points used to make predictions about the target region
# i.e., concatenation OF regions WITHIN a stratum

areaReg <- pmap_dfr(areaList_E, function(E, reg, strat) {
  v <- catchTidy_area_complete_j %>% 
    filter(Type=="catch", Stratum == strat) %>% 
    dplyr::select(Region,value)
  
  block <- make_block_ID(df=v, predictor = value, target = value, ID_col = Region, E=E) %>% na.omit() %>% mutate(index = row_number())
  
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
  out <- data.frame(compute_stats(out_i$Observations, out_i$Predictions)$rho) %>% 
    mutate(E = E, Region = reg, Stratum = strat) 
  out
  
})
areaReg <- areaReg %>% rename(rho = compute_stats.out_i.Observations..out_i.Predictions..rho) %>% mutate(method = "cat_of_reg_in_strat")

areaE <- rbind(areaE, areaReg)

# Adding the other three strata to the library of points used to make predictions about the target stratum
# i.e., concatenation OF strata WITHIN a region

areaStrat <- pmap_dfr(areaList_E, function(E, reg, strat) {
  v <- catchTidy_area_complete_j %>% 
    filter(Type=="catch", Region == reg) %>% 
    dplyr::select(Stratum,value)
  
  block <- make_block_ID(df=v, predictor = value, target = value, ID_col = Stratum, E=E) %>% na.omit() %>% mutate(index = row_number())
  
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
  out <- data.frame(compute_stats(out_i$Observations, out_i$Predictions)$rho) %>% 
    mutate(E = E, Region = reg, Stratum = strat) 
  out
  
})

areaStrat <- areaStrat %>% rename(rho = compute_stats.out_i.Observations..out_i.Predictions..rho) %>% mutate(method = "cat_of_strat_in_reg")

areaE <- rbind(areaE, areaStrat)

## Fig. 5C - E (multispatial) --------------------------------------------------
# Figure 5C - with multispatial
fig5c <- ggplot()+
  geom_line(data=areaE, aes(x=E, y=rho, color=method))+
  facet_grid(Region~Stratum)+
  theme_light()+
  labs(y="Prediction skill (\U03C1)", x="Embedding dimension")+
  scale_y_continuous(breaks=c(-0.2, 0, 0.2, 0.4, 0.6), labels=c("-0.2","", "0.2", "","0.6"))+
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        text = element_text(size = 15),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "white", color="black"),
        strip.text = element_text(colour = "black"))+
  scale_color_brewer(palette = "Accent", name="Method", 
  labels=c('Including data from other regions', 'Including data from other strata', 'Single time series'),
                     guide = guide_legend(reverse = TRUE))
fig5c

# areaE %>% group_by(method, Region, Stratum) %>% 
  # summarise(max = max(rho)) %>% 
  # pivot_wider(names_from = method, values_from = max) %>%
  # mutate(row_max = pmap_chr(across(where(is.double)), ~ names(c(...)[which.max(c(...))])))

E_by_method <- areaE %>%
  group_by(Region, Stratum, method) %>% 
  slice_max(rho)

ggplot(data=E_by_method, aes(x=method, y=E))+geom_boxplot()+geom_jitter(width=0.2, height=0)

density_E <- ggplot(data=E_by_method)+geom_density(aes(x=E, color=method))+theme_bw()

density_rho <- ggplot(data=E_by_method)+geom_density(aes(x=rho, color=method))+theme_bw()

density_E + density_rho +
  plot_layout(guides = 'collect')

ecdf_E <- ggplot(data=E_by_method)+stat_ecdf(aes(x=E, color=method))+theme_bw()
ecdf_rho <- ggplot(data=E_by_method)+stat_ecdf(aes(x=rho, color=method))+theme_bw()
ggplot(data=E_by_method)+stat_ecdf(aes(x=rho))+theme_bw()+facet_wrap(~method)

ecdf_E + ecdf_rho +
  plot_layout(guides = 'collect')

# Strata - no time -----------------------------------------------------

summaryStrat_no_time <- function(df) {
  df %>% group_by(Stratum) %>%
    summarise(avgCatch = mean(Expanded_Catch, na.rm=TRUE),
              avgWt = mean(Expanded_Weight_kg, na.rm=TRUE),
              temp = mean(Bottom_WaterTemp_DegC, na.rm=TRUE))
}

#computes averages for each stratum
j_cat_strat_no <- summaryStrat_no_time(j_cat_clean_seasons)
r_cat_strat_no <- summaryStrat_no_time(r_cat_clean_seasons)

catch_strat_no <- j_cat_strat_no %>% left_join(r_cat_strat_no, by=c("Stratum","temp"), suffix = c("_j", "_r"))

catchTidy_strat_no <- pivot_longer(catch_strat_no,
                                cols = starts_with("avg")) %>%
  mutate(Type = case_when(
    startsWith(name, "avgCatch_") ~"catch",
    startsWith(name,"avgWt_") ~"wt")) %>%
  mutate(Species = case_when(
    endsWith(name, "r") ~"rock",
    endsWith(name, "j") ~"jonah"))

catchTidy_strat_no <- catchTidy_strat_no %>%
  mutate(Species = as.factor(Species), Stratum = as.factor(Stratum)) %>%
  dplyr::select(-name)

## Jonah/rock by stratum (no time) --------------------------------------------------

#Prep: Fig. 7 - Map of rock and Jonah crab abundance by strat
regionsGrid_orig <- surveyGrid %>% group_by(Stratum) %>% summarise(num = n_distinct(GridID))

regionsGrid_no_seas <- left_join(regionsGrid_orig %>% mutate(Stratum = as.factor(Stratum)), 
                                 catchTidy_strat_no %>% filter(Type=="catch",) %>% 
  group_by(Stratum, Species)) %>% 
  filter(Species != "scallop")

new <- c("Jonah crab", "Rock crab")
names(new) <- c("jonah", "rock")

# Figure 7 - Jonah and rock
fig7 <- ggplot()+
  geom_sf(aes(fill=value), data=regionsGrid_no_seas)+
  coord_sf()+
  scale_x_continuous(n.breaks = 2)+
  facet_wrap(.~Species, labeller = labeller(Species = new))+
  labs(fill="Avg catch/tow")+
  theme(axis.text.x = element_text(size = 10))
fig7

# Jonah only, avg catch/tow
ggplot()+
  geom_sf(aes(fill=value), data=regionsGrid_no_seas %>% filter(Species=="jonah"))+
  labs(fill="Avg catch/tow")+
  theme(axis.text.x = element_text(size = 10))+
  scale_fill_fermenter(palette = "BuPu", direction="reverse")+
  theme_light()

## Jonah/rock by stratum by season --------------------------------------------------

summaryStrat_seas <- function(df) {
  df %>% group_by(Stratum, Season) %>%
    summarise(avgCatch = mean(Expanded_Catch, na.rm=TRUE),
              avgWt = mean(Expanded_Weight_kg, na.rm=TRUE),
              temp = mean(Bottom_WaterTemp_DegC, na.rm=TRUE))
}

#computes averages for each stratum
j_cat_strat_seas <- summaryStrat_seas(j_cat_clean_seasons) %>% ungroup() %>% complete(Season, Stratum)
r_cat_strat_seas <- summaryStrat_seas(r_cat_clean_seasons) %>% ungroup() %>% complete(Season, Stratum)

catch_strat_seas <- j_cat_strat_seas %>% 
  left_join(r_cat_strat_seas, by=c("Stratum","Season", "temp"), suffix = c("_j", "_r"))

catchTidy_strat_seas <- pivot_longer(catch_strat_seas,
                                   cols = starts_with("avg")) %>%
  mutate(Type = case_when(
    startsWith(name, "avgCatch_") ~"catch",
    startsWith(name,"avgWt_") ~"wt")) %>%
  mutate(Species = case_when(
    endsWith(name, "r") ~"rock",
    endsWith(name, "j") ~"jonah"))

catchTidy_strat_seas <- catchTidy_strat_seas %>%
  mutate(Species = as.factor(Species), Season = as.factor(Season), Stratum = as.factor(Stratum)) %>%
  dplyr::select(-name) %>% 
  filter(Type=="catch")

catchTidy_strat_seas <- left_join(regionsGrid_orig %>% mutate(Stratum = as.factor(Stratum)), 
                                  catchTidy_strat_seas)

# Jonah and rock avg catch/tow by stratum and season
ggplot()+
  geom_sf(aes(fill=value), data=catchTidy_strat_seas)+
  coord_sf()+
  facet_grid(Season~Species, labeller = labeller(Species = new))+
  labs(fill="Avg catch/tow")+
  theme(axis.text.x = element_text(size = 10))


diff_geom <- left_join(regionsGrid_orig %>% mutate(Stratum = as.factor(Stratum)), 
              catchTidy_strat_seas %>% st_drop_geometry() %>% pivot_wider(names_from = "Season", id_cols = c("Stratum", "Species"), values_from = "value") %>% 
               mutate(diff = Fall-Spring)) %>% mutate(avg = "diff of yearly avgs") %>% 
  mutate(pdiff = diff/Fall)

# Jonah and rock crab fall-spring by stratum - diff of seasonal averages
ggplot()+
  geom_sf(aes(fill=diff), data=diff_geom)+
  facet_wrap(.~Species, labeller = labeller(Species = new))+
  labs(fill="fall-spring")+
  theme(axis.text.x = element_text(size = 10))+scale_fill_fermenter(palette = "RdBu")

# Jonah only fall-spring by stratum - diff of seasonal averages
ggplot()+
  geom_sf(aes(fill=diff ), data=diff_geom %>% filter(Species=="jonah"))+
  labs(fill="fall-spring")+
  theme(axis.text.x = element_text(size = 10))+scale_fill_fermenter(palette = "RdBu")

# Jonah only (fall-spring)/fall by stratum (percentage) - diff of averages
ggplot()+
  geom_sf(aes(fill=pdiff), data=diff_geom %>% filter(Species=="jonah"))+
  labs(fill="(fall-spring)/fall")+
  theme(axis.text.x = element_text(size = 10))+
  scale_fill_fermenter(palette = "GnBu", direction = "reverse")+theme_bw()

seas_diff_geom1 <- catchTidy_strat_complete %>% filter(Type == "catch", Species != "scallop") %>% 
  pivot_wider(names_from = "Season", id_cols = c("Stratum", "Year", "Species"), values_from = "value") %>% mutate(diff = Fall-Spring)  %>% 
  mutate(pdiff = diff/Fall) %>% filter(Species=="jonah")

seas_diff_geom1 %>%  na.omit() %>% group_by(Stratum) %>% 
  wilcox_test(pdiff ~ 1, mu=0, alternative="l") %>%
  adjust_pvalue(method = "holm") %>%
  add_significance("p.adj")

ggplot(data=seas_diff_geom1 %>% na.omit())+geom_histogram(aes(x=pdiff))+facet_wrap(~Stratum)+xlim(-5,5)

seas_diff_geom <- left_join(regionsGrid_orig %>% mutate(Stratum = as.factor(Stratum)), 
                            seas_diff_geom1 %>% group_by(Stratum, Species) %>% 
                              summarise(diff = mean(diff, na.rm = TRUE), 
                                        mean_pdiff=mean(pdiff, na.rm = TRUE),
                                        med_pdiff = median(pdiff, na.rm=TRUE))) %>% 
  mutate(avg = "avg of yearly diffs")


# Jonah and rock fall-spring by stratum - avg of yearly differences
# ggplot()+
#   geom_sf(aes(fill=diff), data=seas_diff_geom)+
#   facet_wrap(.~Species, labeller = labeller(Species = new))+
#   labs(fill="fall-spring")+
#   theme(axis.text.x = element_text(size = 10))+scale_fill_fermenter(palette = "RdBu")

seas_diff_geom1 %>% na.omit() %>% group_by(Stratum) %>% shapiro_test(diff)

seas_diff_for_test <- catchTidy_strat_complete %>% 
  filter(Type == "catch", Species=="jonah", Year!=2003, Year!=2020)

seas_diff_for_test %>% group_by(Stratum) %>% wilcox_test(value ~ Season, paired=TRUE)
seas_diff_for_test %>% filter(Stratum==1) %>% wilcox_test(value ~ Season, paired=TRUE, alternative = "g")
seas_diff_for_test %>% filter(Stratum==1) %>% wilcox_test(value ~ Season, paired=TRUE, alternative = "l")

# Jonah only fall-spring by stratum - avg of yearly differences
ggplot()+
  geom_sf(aes(fill=diff), data=seas_diff_geom %>% filter(Species=="jonah"))+
  labs(fill="fall-spring")+
  theme(axis.text.x = element_text(size = 10))+
  scale_fill_fermenter(palette = "Blues", direction = 1)

ggplot()+
  geom_sf(aes(fill=diff), data=seas_diff_geom_wt)+
  labs(fill="fall-spring")+
  theme(axis.text.x = element_text(size = 10))+
  scale_fill_fermenter(palette = "Blues", direction = 1)

# Jonah only (fall-spring)/fall by stratum - avg of yearly differences
ggplot()+
  geom_sf(aes(fill=mean_pdiff), data=seas_diff_geom %>% filter(Species=="jonah"))+
  labs(fill="(fall-spring)/fall")+
  theme(axis.text.x = element_text(size = 10))+
  scale_fill_fermenter(palette = "Blues", direction = 1)

ggplot()+
  geom_sf(aes(fill=med_pdiff), data=seas_diff_geom %>% filter(Species=="jonah"))+
  labs(fill="(fall-spring)/fall")+
  theme(axis.text.x = element_text(size = 10))+
  scale_fill_fermenter(palette = "Blues", direction = 1)

# Jonah and rock fall-spring by stratum - both methods
# ggplot()+
#   geom_sf(aes(fill=diff), data=rbind(diff_geom %>% dplyr::select(-c(Fall, Spring)), seas_diff_geom %>% dplyr::select(-med_pdiff)))+
#   facet_grid(avg~Species, labeller = labeller(Species = new))+
#   labs(fill="fall-spring")+
#   theme(axis.text.x = element_text(size = 10))+scale_fill_fermenter(palette = "RdBu")

# Jonah only fall-spring by stratum - both methods
ggplot()+
  geom_sf(aes(fill=diff), data=rbind(diff_geom %>% dplyr::select(-c(Fall, Spring)) %>% filter(Species=="jonah"), seas_diff_geom %>% filter(Species=="jonah")  %>% dplyr::select(-med_pdiff)))+
  labs(fill="fall-spring")+
  facet_wrap(~avg)+
  theme(axis.text.x = element_text(size = 10))+
  scale_fill_fermenter(palette = "BuPu", direction=1)+
  theme_light()+
  theme(strip.background = element_rect(color="grey37", fill="white"),
        strip.text = element_text(color="black"))


# Relative abundance ------------------------------------------------------
regionsGrid_reg <- surveyGrid %>% group_by(Region, Stratum) %>% summarise(num = n_distinct(GridID))

totals <- df_j_cat %>% group_by(Year, Season, Region, Stratum) %>% summarise(tot = sum(Expanded_Catch))  %>%
  mutate(date=paste(Year, case_when(Season== "Fall" ~ "-11-01", Season =="Spring" ~"-05-01"), sep = "")) %>% ungroup() %>% mutate(date = as.POSIXct(date)) %>% filter(Year > 2002)

ggplot(data = totals, aes(x=date, y=tot))+
  geom_line()+
  facet_grid(Region ~ Stratum)

totals2 <- left_join(totals, totals %>% group_by(Year) %>% summarise(sum = sum(tot))) %>% 
  mutate(perc = tot/sum) %>% complete(Region, Stratum, Year, Season)
  #mutate(across(where(is.numeric), replace_na,0))

totals_geom <- left_join(regionsGrid_reg, totals2)
       

ggplot(data = totals2 %>% filter(Season=="Fall"), aes(x=Region, y=Stratum, fill=perc))+
  geom_tile(color= "white",size=0.1)+
  facet_wrap(~Year)+scale_fill_viridis_c()

totals_no_time<- df_j_cat %>% group_by(Region, Stratum) %>% summarise(tot = sum(Expanded_Catch))
s <- sum(totals_no_time$tot)
totals_no_time <- totals_no_time %>% mutate(perc = tot/s)

ggplot(data = totals_no_time, aes(x=Region, y=Stratum, fill=perc))+
  geom_tile(color= "white",size=0.1)+scale_fill_viridis_c()

jplot1 <- ggplot(data = totals_geom %>% group_by(Region, Stratum) %>% summarise(perc = mean(perc, na.rm=TRUE)))+
  geom_sf(aes(fill=perc))+
  scale_fill_viridis_c()+
  theme_light()

            
j_c_c_s<-j_cat_clean_seasons %>% group_by(Stratum, Region) %>%
  summarise(avgCatch = mean(Expanded_Catch, na.rm=TRUE),
            avgWt = mean(Expanded_Weight_kg, na.rm=TRUE))

j_c_c_s_geom <- left_join(regionsGrid_reg, j_c_c_s)

jplot2 <- ggplot(data=j_c_c_s_geom)+geom_sf(aes(fill=avgCatch))+
  scale_fill_viridis_c()+
  theme_light()

j_c_c_s_geom_no_time <- left_join(regionsGrid_reg, totals_no_time)

jplot3 <- ggplot(data=j_c_c_s_geom_no_time)+geom_sf(aes(fill=perc))+scale_fill_viridis_c()

(jplot1 + jplot3) / jplot2
jplot1 + jplot2

totals_no_time_strat<- df_j_cat %>% group_by(Stratum) %>% summarise(tot = sum(Expanded_Catch))
s_strat <- sum(totals_no_time_strat$tot)
totals_no_time_strat <- totals_no_time_strat %>% mutate(perc = tot/s_strat)

jplot4 <- ggplot(data=left_join(regionsGrid_orig,totals_no_time_strat))+
  geom_sf(aes(fill=perc))+
 # scale_fill_fermenter(palette = "Blues", direction="reverse")+
  scale_fill_viridis_c()+
  theme_light()

# Jonah only, avg catch/tow
jplot5 <-ggplot()+
  geom_sf(aes(fill=value), data=regionsGrid_no_seas %>% filter(Species=="jonah"))+
  labs(fill="Avg catch/tow")+
  theme(axis.text.x = element_text(size = 10))+
 # scale_fill_fermenter(palette = "Blues", direction="reverse")+
  scale_fill_viridis_c()+
  theme_light()

(jplot1 + jplot2)/(jplot4+jplot5)
