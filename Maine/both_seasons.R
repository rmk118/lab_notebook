#Analysis using both seasons of ME-NH survey data
#Ruby Krasnow
#Last modified: July 18, 2023

#Load packages
library(tidyverse)
library(lubridate) #date formatting
library(patchwork) #combining plots
# library(tseries) #for KPSS test for stationarity
library(rEDM) #EDM
library(forecast)
library(seastests)

# library(ggfortify)
# library(xts)

#spatial packages
# library(sf)
# library(sfheaders)
# library(spdep)

#import help functions and data
source('~/Downloads/lab_notebook/Maine/helpFunctionsMaine.R')
df_tows<-read.csv("data/Maine_inshore_trawl/MEtows.csv") #tow data
df_s_cat<- read.csv("data/Maine_inshore_trawl/MEscallopCatch.csv") #scallop catch
df_r_cat<- read.csv("data/Maine_inshore_trawl/MErockCatch.csv") #rock crab catch
df_j_cat<- read.csv("data/Maine_inshore_trawl/MEjonahCatch.csv") #jonah crab catch

s_cat_clean_seasons <- cleanCatch(df_s_cat) %>% 
mutate(Common_Name = "Scallop")

r_cat_clean_seasons <- cleanCatch(df_r_cat) %>% 
  mutate(Common_Name = "Rock")

j_cat_clean_seasons <- cleanCatch(df_j_cat) %>% 
  mutate(Common_Name = "Jonah")

#Reorder columns
colOrder<-c("area", "Survey", "Tow_Number", "Region", "Stratum", "logCatch", "logWt", "Expanded_Catch", 
            "Expanded_Weight_kg", "Date", "Common_Name", "Number_Caught", "Weight_kg",
            "Start_Latitude", "Start_Longitude","Season",
            "Year","Grid", "Start_Depth_fathoms", "End_Depth_fathoms",
            "Bottom_WaterTemp_DegC", "Bottom_Salinity")

j_cat_clean_seasons <- j_cat_clean_seasons %>% select(all_of(colOrder))
r_cat_clean_seasons <- r_cat_clean_seasons %>% select(all_of(colOrder))
s_cat_clean_seasons <- s_cat_clean_seasons %>% select(all_of(colOrder))

#computes averages for each study area (area = region-stratum combination)
j_cat_sum_seasons <- summaryCatch(j_cat_clean_seasons)
r_cat_sum_seasons <- summaryCatch(r_cat_clean_seasons)
s_cat_sum_seasons <- summaryCatch(s_cat_clean_seasons)

catch_seasons <- s_cat_sum_seasons %>% left_join(j_cat_sum_seasons, by=c("area", "Season", "Region", "Stratum", "Year"), suffix = c("_s", "_j"))
catch_seasons <- catch_seasons %>% left_join(r_cat_sum_seasons, by=c("area", "Season", "Region", "Stratum", "Year")) %>% 
  mutate(avgCatch_r = avgCatch,avgWt_r = avgWt,avgLogCatch_r = avgLogCatch,avgLogWt_r = avgLogWt, .keep="unused")

catchTidy_seasons <- pivot_longer(catch_seasons, 
                          cols = 6:ncol(catch_seasons)) %>% 
  mutate(Type = case_when(
    startsWith(name, "avgCatch_") ~"catch",
    startsWith(name,"avgWt_") ~"wt",
    startsWith(name,"avgLogWt") ~"logWt",
    startsWith(name,"avgLogCatch") ~"logCatch")) %>% 
  mutate(Species = case_when(
    endsWith(name, "s") ~"scallop",
    endsWith(name, "r") ~"rock",
    endsWith(name, "j") ~"jonah"))

catchTidy_seasons <- catchTidy_seasons %>% mutate(area = as.factor(area), Species = as.factor(Species),Season = as.factor(Season),Region = as.factor(Region), Stratum = as.factor(Stratum)) %>% 
  select(-name)

# logCatch <- catchTidy_seasons %>% filter(Type == "logCatch")
# logWt <- catchTidy_seasons %>% filter(Type == "logWt") %>% 
#   mutate(date=paste(Year, case_when(Season== "Fall" ~ "-11-01",
#                                     Season =="Spring" ~"-05-01"), sep = ""))



# logWt %>% group_by(area, Species) %>% arrange(Year) %>% select(value) %>% 
#   group_map(~ts(., frequency = 2, start=c(2000, 2)))

# logWt_complete <- complete(data=logWt %>% ungroup(), date, Species, Region, Stratum, explicit = TRUE) %>% mutate(area = paste(Region, Stratum), Type="logWt") %>% select(date, Species, area, value)

# logWt_areas <- logWt_complete %>% group_by(area, Species) %>% arrange(date) %>% select(value) %>% 
#   group_map(~ts(., frequency = 2, start=c(2000, 2)))
# 
# logWt_areas2 <- logWt_complete %>% group_by(area) %>% arrange(date) %>% select(Species, value) %>% 
#   group_map(~ts(., frequency = 2, start=c(2000, 2)))


catch_complete <- complete(data=catch_seasons %>% ungroup(), Region, Stratum, Season, Year) %>% 
  mutate(area = paste(Region, Stratum)) %>% 
  mutate(date=paste(Year, case_when(Season== "Fall" ~ "-11-01", Season =="Spring" ~"-05-01"), sep = ""), .before=Region) %>% 
  filter(date != "2000-05-01")

catch_ts <- catch_complete %>% group_by(area) %>% arrange(date) %>% select(avgLogWt_s) %>% 
  group_map(~ts(., frequency = 2, start=c(2000, 2)))# .keep = FALSE))

catch_complete <- catch_complete %>% mutate(date = lubridate::ymd(date))

lag2 <- function(x) {
  x_lagged <- (x - lag(x, 2))
  return(x_lagged)
} 

# lag2(c(1, 3, 3, 5, 6, 9, 12))
catch_complete_diff <- catch_complete %>% arrange(date) %>% group_by(area) %>% 
  mutate(across(where(is.double) & !date, lag2)) %>% 
  arrange(area) %>% 
  filter(date != "2000-11-01" & date != "2001-05-01") %>%  filter(date < as.Date("2020-05-01"))

#ggplot(data=logCatch, aes(x=Year, y=value, color=Species)) +geom_line()+facet_grid(Region~Stratum)

# logWt_ts <- ts(logWt, frequency = 2)
# logCatch_ts <- ts(logCatch, frequency = 2)
# catch_seasons_ts <- ts(catch_seasons %>% ungroup() %>% filter(area== "1 1") %>% select(Season, Year,avgLogCatch_s) %>% arrange(Year), frequency = 2)
# 
# autoplot(catch_seasons_ts)
# monthplot(catch_seasons_ts, phase = "Season")
# 
# s_cat_seasons <- ts(s_cat_clean %>% filter(area== "1 1") %>% select(Date,Tow_Number, Region, Stratum, logWt) %>% group_by(Date, Region, Stratum) %>% mutate(date = as.POSIXct(Date)) %>% summarise(lWt = mean(logWt)))

# plot(catch_ts[[1]])
# Box.test(catch_ts[[1]], type = "Ljung-Box")
# #apply(catch_ts[[1]], 2, kpss.test, null = "Trend")                      
# 
# decompose(catch_ts2)
# plot(decompose(catch_ts2))
# ggAcf(catch_ts[[1]])
# 
# catch_ts2 <- na.interp(catch_ts[[1]])
# catch_ts2[[1]] %>%
#   stl() %>%
#   autoplot()
# 
 #catch_ts2diff <- map(catch_ts, diff, lag=2)
# 
# plot(catch_ts2)
# plot(catch_ts2diff)
# 
 #fried(catch_ts[[1]])
#map(catch_ts2diff, fried, freq=2)
#map(catch_ts2diff, isSeasonal, test="qs",freq=2)
# summary(seastests::wo(catch_ts[[1]]))
# # plot(decompose(catch_ts2diff))

# catch_complete_tidy <-pivot_longer(catch_complete, 
#                                    cols = 7:ncol(catch_complete)) %>% 
#   mutate(Type = case_when(
#     startsWith(name, "avgCatch_") ~"catch",
#     startsWith(name,"avgWt_") ~"wt",
#     startsWith(name,"avgLogWt") ~"logWt",
#     startsWith(name,"avgLogCatch") ~"logCatch")) %>% 
#   mutate(Species = case_when(
#     endsWith(name, "s") ~"scallop",
#     endsWith(name, "r") ~"rock",
#     endsWith(name, "j") ~"jonah")) %>%
#   mutate(area = as.factor(area), Species = as.factor(Species),
#          Region = as.factor(Region), Type = as.factor(Type),
#          Stratum = as.factor(Stratum)) %>% 
#   select(-name)

# logCatchComplete <- catch_complete_tidy %>% filter(Type == "logCatch")
# logWtComplete <- catch_complete_tidy %>% filter(Type == "logWt")

complete_tidy_diff <- pivot_longer(catch_complete_diff, 
                                   cols = 7:ncol(catch_complete)) %>% 
  mutate(Type = case_when(
    startsWith(name, "avgCatch_") ~"catch",
    startsWith(name,"avgWt_") ~"wt",
    startsWith(name,"avgLogWt") ~"logWt",
    startsWith(name,"avgLogCatch") ~"logCatch")) %>% 
  mutate(Species = case_when(
    endsWith(name, "s") ~"scallop",
    endsWith(name, "r") ~"rock",
    endsWith(name, "j") ~"jonah")) %>%
  mutate(area = as.factor(area), Species = as.factor(Species),
         Region = as.factor(Region), Type = as.factor(Type),
         Stratum = as.factor(Stratum)) %>% 
  select(-name) %>%  filter(date < as.Date("2020-05-01"))

ggplot(data = complete_tidy_diff %>% filter(Type == "logWt", Species=="scallop"), aes(x=date, y=value, color=area))+geom_line()
ggplot(data = complete_tidy_diff %>% filter(Type == "logWt"), aes(x=date, y=value, color=area))+geom_line()+facet_wrap(~Species) #a lot more variation in the jonah crabs

ggplot(data = complete_tidy_diff %>% filter(Type == "logWt") %>% group_by(date, Species) %>% summarise(avg = mean(value, na.rm = TRUE)), aes(x=date, y=avg))+geom_line()+facet_wrap(~Species) #a lot more variation in the jonah crabs

ggplot(data = catch_complete_tidy %>% filter(Type == "logWt"), aes(x=date, y=value, color=area))+geom_line()+facet_wrap(~Species)

do_xmap_ID(df=complete_tidy_diff %>% filter(Species=="scallop", Type=="catch") %>% group_by(date) %>% 
               summarise(avg = mean(value, na.rm = TRUE)) %>% 
               ungroup() %>% select(date, avg) %>% mutate(ID=1), ID_col = "ID",
             predictor="avg", target="avg", E_max=7, tp=1) #better than linear!

EmbedDimension(dataFrame=complete_tidy_diff %>% filter(Species=="scallop", Type=="catch") %>% group_by(date) %>% 
                 summarise(avg = mean(value, na.rm = TRUE)) %>% 
                 ungroup() %>% select(date, avg),  columns ="avg", target="avg", lib = "1 37", pred="1 37") #E=2, rho=0.55

EmbedDimension(dataFrame=complete_tidy_diff %>% filter(Species=="scallop", Type=="wt") %>% group_by(date) %>% 
                 summarise(avg = mean(value, na.rm = TRUE)) %>% 
                 ungroup() %>% select(date, avg),  columns ="avg", target="avg", lib = "1 37", pred="1 37") #E=2, rho=0.4

do_xmap_ID(df=complete_tidy_diff %>% filter(Species=="scallop", Type=="wt") %>% group_by(date) %>% 
               summarise(avg = mean(value, na.rm = TRUE)) %>% 
               ungroup() %>% select(date, avg) %>% mutate(ID=1), ID_col = "ID",
             predictor="avg", target="avg", E_max=7, tp=1) #also better than linear

# Areas -------------------------------------------------------------------

#Scallops
#Catch
findSpeciesE(complete_tidy_diff %>% filter(Species=="scallop") %>% na.omit(), type="catch")
findSpeciesErho(complete_tidy_diff %>% filter(Species=="scallop") %>% na.omit(), type="catch")

findSpeciesTheta(complete_tidy_diff %>% filter(Species=="scallop") %>% na.omit(), type="catch")
findSpeciesTheta_rho(complete_tidy_diff %>% filter(Species=="scallop") %>% na.omit(), type="catch")

findSpeciesKPSS(complete_tidy_diff %>% filter(Species=="scallop") %>% na.omit(), type="wt")

#univariate dewdrop with areas as replicates
catchDiffInt <- complete_tidy_diff %>% mutate(areaInt = as.integer(paste0(Region, Stratum)))

do_xmap_ID(df=(catchDiffInt %>% filter(Species=="scallop", Type=="catch") %>% ungroup() %>% select(areaInt, Year, value)), predictor="value", target="value", ID_col="areaInt", E_max=7, tp=1)

make_xmap_block_ID(df=(catchDiffInt %>% filter(Species=="scallop", Type=="catch") %>% ungroup() %>% select(areaInt, Year, value)), predictor=value, target=value, ID_col=areaInt, E_max=7, cause_lag=1)

#Wt
findSpeciesE(complete_tidy_diff %>% filter(Species=="scallop") %>% na.omit(), type="wt")
findSpeciesErho(complete_tidy_diff %>% filter(Species=="scallop") %>% na.omit(), type="wt")

findSpeciesTheta(complete_tidy_diff %>% filter(Species=="scallop") %>% na.omit(), type="wt")
findSpeciesTheta_rho(complete_tidy_diff %>% filter(Species=="scallop") %>% na.omit(), type="wt")

do_xmap_ID(df=(catchDiffInt %>% filter(Species=="scallop", Type=="wt") %>% ungroup() %>% select(areaInt, Year, value)), predictor="value", target="value", ID_col="areaInt", E_max=7, tp=1)


findSpeciesGroups_both<- function(df, species, type, g) {
  df_out <- df %>% na.omit() %>% 
    filter(Type == type, Species == species) %>% 
    group_by(!!sym(g), date) %>% 
    summarise(avg = mean(value)) %>% 
    group_by(!!sym(g))  %>%
    summarise(E_opt = findE_v(avg),
              rho_E = findErho_v(avg),
              Theta = findTheta_v(avg, E_opt),
              rho_theta = findThetaRho_v(avg, E_opt))
  return(df_out)
}


# Regions -----------------------------------------------------------------

findSpeciesGroups_both(complete_tidy_diff, type="catch", g="Region", species="scallop")
findSpeciesGroups_both(complete_tidy_diff, type="wt", g="Region", species="scallop")

findSpeciesGroups_both(complete_tidy_diff, type="catch", g="Region", species="rock")
findSpeciesGroups_both(complete_tidy_diff, type="wt", g="Region", species="rock")

findSpeciesGroups_both(complete_tidy_diff, type="catch", g="Region", species="jonah")
findSpeciesGroups_both(complete_tidy_diff, type="wt", g="Region", species="jonah")

do_xmap_ID(df=complete_tidy_diff %>% filter(Species=="scallop", Type=="catch") %>%
             ungroup() %>% na.omit() %>% 
             mutate(Region = as.numeric(Region)) %>% group_by(Region, date) %>% 
             summarise(avg = mean(value)) %>% 
             ungroup() %>% select(Region, date, avg),
           predictor="avg", target="avg", ID_col="Region", E_max=7, tp=1)

do_xmap_ID(df=complete_tidy_diff %>% filter(Species=="scallop", Type=="wt") %>%
             ungroup() %>% na.omit() %>% 
             mutate(Region = as.numeric(Region)) %>% group_by(Region, date) %>% 
             summarise(avg = mean(value)) %>% 
             ungroup() %>% select(Region, date, avg),
           predictor="avg", target="avg", ID_col="Region", E_max=7, tp=1)


# Strata ------------------------------------------------------------------

findSpeciesGroups_both(complete_tidy_diff, type="catch", g="Stratum", species="scallop")
findSpeciesGroups_both(complete_tidy_diff, type="wt", g="Stratum", species="scallop")

findSpeciesGroups_both(complete_tidy_diff, type="catch", g="Stratum", species="rock")
findSpeciesGroups_both(complete_tidy_diff, type="wt", g="Stratum", species="rock")

findSpeciesGroups_both(complete_tidy_diff, type="catch", g="Stratum", species="jonah")
findSpeciesGroups_both(complete_tidy_diff, type="wt", g="Stratum", species="jonah")

do_xmap_ID(df=complete_tidy_diff %>% filter(Species=="scallop", Type=="catch") %>%
             ungroup() %>% na.omit() %>% 
             mutate(Stratum = as.numeric(Stratum)) %>% group_by(Stratum, date) %>% 
             summarise(avg = mean(value)) %>% 
             ungroup() %>% select(Stratum, date, avg),
           predictor="avg", target="avg", ID_col="Stratum", E_max=7, tp=1)

do_xmap_ID(df=complete_tidy_diff %>% filter(Species=="scallop", Type=="wt") %>%
             ungroup() %>% na.omit() %>% 
             mutate(Stratum = as.numeric(Stratum)) %>% group_by(Stratum, date) %>% 
             summarise(avg = mean(value)) %>% 
             ungroup() %>% select(Stratum, date, avg),
           predictor="avg", target="avg", ID_col="Stratum", E_max=7, tp=1)

###### CCM - catch

catchCCMdf <- catch_complete_diff %>% ungroup() %>% na.omit() %>% 
  select(date, Region, Stratum, area, avgCatch_s, avgCatch_r, avgCatch_j) %>% 
  rename(rock = avgCatch_r , scallop = avgCatch_s , jonah= avgCatch_j)  %>% 
  mutate(area = as.integer(paste0(Region, Stratum)))

params_ccm_combos <- data.frame(predator=c("rock", "jonah", "jonah"), prey=c("scallop", "scallop", "rock"))

# area as replicate
RESULTS_ccm_combos_areas <- pmap_dfr(params_ccm_combos,function(predator,prey){
  
  out_1 <- do_xmap_ID(catchCCMdf,predictor=predator,target=prey,"area",E_max = 7, tp=1) %>% 
    mutate(predator=predator,
           prey=prey,
           direction= paste("predator","->","prey"))
  
  out_2 <- do_xmap_ID(catchCCMdf,predictor=prey,target=predator,"area",E_max = 7, tp=1) %>% 
    mutate(predator=predator,
           prey=prey,
           direction= paste("prey","->","predator"))
  
  bind_rows(out_1,out_2) %>% select(all_of(v_keep_col))
  
})

v_keep_col <- c("E","Tp","num_pred", "rho", "mae","rmse","perc","p_val","rho_linear", "mae_linear",
  "rmse_linear","perc_linear","p_val_linear", "predator", "prey", "direction")

regionsDf<- catchCCMdf %>% ungroup() %>% 
  group_by(Region, date) %>% 
  summarise(across(scallop:jonah, ~ mean(.x, na.rm = TRUE)))

strataDf<- catchCCMdf %>% ungroup() %>% 
  group_by(Stratum, date) %>% 
  summarise(across(scallop:jonah, ~ mean(.x, na.rm = TRUE)))

#Regions as replicates
RESULTS_ccm_combos_regions <- pmap_dfr(params_ccm_combos,function(predator,prey){
  
  out_1 <- do_xmap_ID(regionsDf,predictor=predator,target=prey,"Region",E_max = 7, tp=1)  %>%
    mutate(predator=predator,
           prey=prey,
           direction= paste("predator","->","prey"))
  
  out_2 <- do_xmap_ID(regionsDf,predictor=prey,target=predator,"Region",E_max = 7, tp=1) %>% 
    mutate(predator=predator,
           prey=prey,
           direction= paste("prey","->","predator"))
  
  bind_rows(out_1,out_2) %>% 
    select(all_of(v_keep_col))
  
})

#Strata as replicates
RESULTS_ccm_combos_strata <- pmap_dfr(params_ccm_combos,function(predator,prey){
  
  out_1 <- do_xmap_ID(strataDf,predictor=predator,target=prey,"Stratum",E_max = 7, tp=1)  %>%
    mutate(predator=predator,
           prey=prey,
           direction= paste("predator","->","prey"))
  
  out_2 <- do_xmap_ID(strataDf,predictor=prey,target=predator,"Stratum",E_max = 7, tp=1) %>% 
    mutate(predator=predator,
           prey=prey,
           direction= paste("prey","->","predator"))
  
  bind_rows(out_1,out_2) %>% select(all_of(v_keep_col))
  
})


params_areas_ccm_combos<- expand.grid(predator=c("jonah", "rock"),
            prey=c("scallop", "rock"),
            areaInput=levels(as.factor(catchCCMdf$area)), stringsAsFactors = FALSE) %>% filter(predator != prey) %>% 
  mutate(areaInput = as.integer(areaInput))

RESULTS_ccm_combos_by_area <- pmap_dfr(params_areas_ccm_combos,function(predator,prey, areaInput){
  
  df_temp <- catchCCMdf %>% filter(area == areaInput)
  #out1<- catchCCMdf %>% filter(area == areaInput) %>% mutate(predator=predator,
  #                                                                         prey=prey,
  #                                                                         direction= paste("prey","->","predator")
 
  out_1 <- do_xmap_noID(df= df_temp, predictor=predator, target=prey, E_max = 7, tp=1)  %>%
    mutate(predator=predator,
           prey=prey,
           direction= paste("predator","->","prey"),
           area = areaInput)

  out_2 <- do_xmap_noID(df= df_temp,predictor=prey,target=predator,E_max = 7, tp=1) %>%
    mutate(predator=predator,
           prey=prey,
           direction= paste("prey","->","predator"),
           area = areaInput)

  bind_rows(out_1,out_2) #%>% select(areaInput, all_of(v_keep_col))
  
})


ggplot(RESULTS_ccm_combos_by_area2, aes(x=xmap, y=rho)) + geom_boxplot()+
  geom_jitter()


RESULTS_ccm_combos_by_area2 <- RESULTS_ccm_combos_by_area %>% 
  mutate(xmap = case_when(predator=="jonah" & prey=="scallop" & direction=="predator -> prey" ~ "jonah -> scallop",
                          predator=="jonah" & prey=="scallop" & direction=="prey -> predator" ~ "scallop -> jonah",
                          predator=="rock" & prey=="scallop" & direction=="predator -> prey" ~ "rock -> scallop",
                          predator=="rock" & prey=="scallop" & direction=="prey -> predator" ~ "scallop -> rock",
                          predator=="jonah" & prey=="rock" & direction=="predator -> prey" ~ "jonah -> rock",
                          predator=="jonah" & prey=="rock" & direction=="prey -> predator" ~ "rock -> jonah"))
            
            


make_xmap_block_noID(df=catchCCMdf %>% filter(area==11), predictor = scallop, target = jonah, E_max = 7, 
                   cause_lag = 1)

do_xmap_noID(df=catchCCMdf %>% filter(area==14) %>% select(-c(Region, Stratum, area)), predictor = "scallop", target = "jonah", E_max = 7,tp = 1)
