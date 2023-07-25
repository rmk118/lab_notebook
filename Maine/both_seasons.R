# Analysis using both seasons of ME-NH survey data
#Ruby Krasnow
#Last modified: July 24, 2023

#Load packages
library(tidyverse)
library(lubridate) #date formatting
library(patchwork) #combining plots
library(rEDM) #EDM
library(clipr) #Copying code to keyboard

#spatial packages
library(sf)
library(sfheaders)
library(spdep)

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

catch_complete <- complete(data=catch_seasons %>% ungroup(), Region, Stratum, Season, Year) %>% 
  mutate(area = paste(Region, Stratum)) %>% 
  mutate(date=paste(Year, case_when(Season== "Fall" ~ "-11-01", Season =="Spring" ~"-05-01"), sep = ""), .before=Region) %>% 
  filter(date != "2000-05-01")

catch_complete <- catch_complete %>% mutate(date = lubridate::ymd(date))

lag2 <- function(x) {
  x_lagged <- (x - lag(x, 2))
  return(x_lagged)
}  # test with lag2(c(1, 3, 3, 5, 6, 9, 12))

catch_complete_diff <- catch_complete %>% arrange(date) %>% group_by(area) %>% 
  mutate(across(where(is.double) & !date, lag2)) %>% 
  arrange(area) %>% 
  filter(date != "2000-11-01" & date != "2001-05-01") %>%  filter(date < as.Date("2020-05-01"))

complete_tidy_diff <- pivot_longer(catch_complete_diff,cols = 7:ncol(catch_complete)) %>% 
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
  select(-name)

ggplot(data = complete_tidy_diff %>% 
         filter(Type == "wt", Species=="scallop"), aes(x=date, y=value, color=area))+geom_line()

ggplot(data = complete_tidy_diff %>% 
         filter(Type == "wt"), aes(x=date, y=value, color=area))+geom_line()+facet_wrap(~Species) 
#a lot more variation in the jonah crabs

ggplot(data = complete_tidy_diff %>% filter(Type == "wt") %>% group_by(date, Species) %>% 
    summarise(avg = mean(value, na.rm = TRUE)), aes(x=date, y=avg))+geom_line()+facet_wrap(~Species)
#a lot more variation in the jonah crabs

catchCCMdf <- catch_complete_diff %>% ungroup() %>% na.omit() %>% 
  select(date, Region, Stratum, area, avgCatch_s, avgCatch_r, avgCatch_j) %>% 
  rename(rock = avgCatch_r , scallop = avgCatch_s , jonah= avgCatch_j)  %>% 
  mutate(area = as.integer(paste0(Region, Stratum)))

wtCCMdf <- catch_complete_diff %>% ungroup() %>% na.omit() %>% 
  select(date, Region, Stratum, area, avgWt_s, avgWt_r, avgWt_j) %>% 
  rename(rock = avgWt_r , scallop = avgWt_s , jonah= avgWt_j)  %>% 
  mutate(area = as.integer(paste0(Region, Stratum)))

catchCCMdf_agg <- catchCCMdf %>% group_by(date) %>% summarise(across(scallop:jonah, mean))
wtCCMdf_agg <- wtCCMdf %>% group_by(date) %>% summarise(across(scallop:jonah, mean))

addDirection <- function(df) {
  df_out <- df %>% 
    mutate(xmap = case_when(predator=="jonah" & prey=="scallop" & direction=="predator -> prey" ~ "jonah -> scallop",
                            predator=="jonah" & prey=="scallop" & direction=="prey -> predator" ~ "scallop -> jonah",
                            predator=="rock" & prey=="scallop" & direction=="predator -> prey" ~ "rock -> scallop",
                            predator=="rock" & prey=="scallop" & direction=="prey -> predator" ~ "scallop -> rock",
                            predator=="jonah" & prey=="rock" & direction=="predator -> prey" ~ "jonah -> rock",
                            predator=="jonah" & prey=="rock" & direction=="prey -> predator" ~ "rock -> jonah"))
  return(df_out)
}

combos <- c("jonah:scallop", "scallop:jonah", "rock:scallop", "scallop:rock","jonah:rock","rock:jonah")

# Aggregate stats ---------------------------------------------------------

EmbedDimension(dataFrame=complete_tidy_diff %>% filter(Species=="scallop", Type=="catch") %>% group_by(date) %>% 
                 summarise(avg = mean(value, na.rm = TRUE)) %>% 
                 ungroup() %>% select(date, avg),  columns ="avg", target="avg", lib = "1 37", pred="1 37") #E=2, rho=0.55

EmbedDimension(dataFrame=complete_tidy_diff %>% filter(Species=="scallop", Type=="wt") %>% group_by(date) %>% 
                 summarise(avg = mean(value, na.rm = TRUE)) %>% 
                 ungroup() %>% select(date, avg),  columns ="avg", target="avg", lib = "1 37", pred="1 37") #E=2, rho=0.4

PredictNonlinear(dataFrame=complete_tidy_diff %>% filter(Species=="scallop", Type=="catch") %>% group_by(date) %>% 
                   summarise(avg = mean(value, na.rm = TRUE)) %>% 
                   ungroup() %>% select(date, avg),  columns ="avg", target="avg", lib = "1 37", pred="1 37", E=2)

# CCM of average over all areas

params_ccm_combos <- data.frame(predator=c("rock", "jonah", "jonah"), prey=c("scallop", "scallop", "rock"))

par(mfrow=c(2,2), mar=c(1,1,1,1))

RESULTS_ccm_combos_aggregate <- pmap_dfr(params_ccm_combos,function(predator,prey){
  lib_vec <- paste(1, nrow(catchCCMdf_agg))
  
  rho_E_1<- EmbedDimension(dataFrame = catchCCMdf_agg, lib = lib_vec, pred = lib_vec, 
                           columns = predator,target = prey, maxE = 7, showPlot = FALSE)
  E_out_1<-rho_E_1[which.max(rho_E_1$rho),"E"][1]
  out_1 <- CCM(dataFrame= catchCCMdf_agg, columns=predator, target=prey, E = E_out_1, Tp=1,
               libSizes = paste(E_out_1+2, nrow(catchCCMdf_agg) - E_out_1, "1",sep=" "), sample=100, verbose=FALSE, showPlot = TRUE) %>%
    mutate(predator=predator,
           prey=prey,
           direction= paste("predator","->","prey"),
           E = E_out_1)
  
  rho_E_2<- EmbedDimension(dataFrame = catchCCMdf_agg, lib = lib_vec, pred = lib_vec, 
                           columns = prey,target = predator, maxE = 7, showPlot = FALSE)
  E_out_2<-rho_E_2[which.max(rho_E_1$rho),"E"][1]
  
  out_2 <- CCM(dataFrame= catchCCMdf_agg, columns=prey, target=predator, E = E_out_2, Tp=1, 
               libSizes = paste(E_out_2+2, nrow(catchCCMdf_agg)-E_out_2, "1",sep=" "), 
               sample=100, verbose=FALSE, showPlot = FALSE) %>%
    mutate(predator=predator,
           prey=prey,
           direction= paste("prey","->","predator"),
           E = E_out_2)
  
  bind_rows(out_1,out_2)
  
}) %>% addDirection()

RESULTS_ccm_combos_aggregate <- RESULTS_ccm_combos_aggregate %>% select(-c(prey, predator, direction))
RESULTS_ccm_combos_aggregate %>% ungroup() %>% group_by(LibSize) %>% summarise(across(all_of(combos), ~mean(.x, na.rm = TRUE))) %>% pivot_longer(cols = 2:7)

RESULTS_ccm_combos_aggregate_wt <- pmap_dfr(params_ccm_combos,function(predator,prey){
  lib_vec <- paste(1, nrow(wtCCMdf_agg))
  
  rho_E_1<- EmbedDimension(dataFrame = wtCCMdf_agg, lib = lib_vec, pred = lib_vec, 
                           columns = predator,target = prey, maxE = 7, showPlot = FALSE)
  E_out_1<-rho_E_1[which.max(rho_E_1$rho),"E"][1]
  out_1 <- CCM(dataFrame= wtCCMdf_agg, columns=predator, target=prey, E = E_out_1, Tp=1,
               libSizes = paste(E_out_1+2, nrow(wtCCMdf_agg) - E_out_1, "1",sep=" "), sample=100, verbose=FALSE, showPlot = TRUE) %>%
    mutate(predator=predator,
           prey=prey,
           direction= paste("predator","->","prey"),
           E = E_out_1)
  
  rho_E_2<- EmbedDimension(dataFrame = wtCCMdf_agg, lib = lib_vec, pred = lib_vec, 
                           columns = prey,target = predator, maxE = 7, showPlot = FALSE)
  E_out_2<-rho_E_2[which.max(rho_E_1$rho),"E"][1]
  out_2 <- CCM(dataFrame= wtCCMdf_agg, columns=prey, target=predator, E = E_out_2, Tp=1, 
               libSizes = paste(E_out_2+2, nrow(wtCCMdf_agg)-E_out_2, "1",sep=" "), sample=100, verbose=FALSE, showPlot = FALSE)%>%
    mutate(predator=predator,
           prey=prey,
           direction= paste("prey","->","predator"),
           E = E_out_2)
  
  bind_rows(out_1,out_2)
  
}) %>% addDirection()
RESULTS_ccm_combos_aggregate_wt <- RESULTS_ccm_combos_aggregate_wt %>% select(-c(prey, predator, direction))
RESULTS_ccm_combos_aggregate_wt %>% ungroup() %>% group_by(LibSize) %>% summarise(across(all_of(combos), ~mean(.x, na.rm = TRUE))) %>% pivot_longer(cols = 2:7)

#Overall xmap skill
ggplot(data = RESULTS_ccm_combos_aggregate %>% ungroup() %>% group_by(LibSize) %>% summarise(across(all_of(combos), ~mean(.x, na.rm = TRUE))) %>% pivot_longer(cols = 2:7)) +geom_line(aes(x=LibSize, y=value, color=name))

ggplot(data = RESULTS_ccm_combos_aggregate_wt %>% ungroup() %>% group_by(LibSize) %>% summarise(across(all_of(combos), ~mean(.x, na.rm = TRUE))) %>% pivot_longer(cols = 2:7)) +geom_line(aes(x=LibSize, y=value, color=name))

# Areas -------------------------------------------------------------------

#Scallops
#Catch
findSpeciesE(complete_tidy_diff %>% filter(Species=="scallop") %>% na.omit(), type="catch") 
findSpeciesErho(complete_tidy_diff %>% filter(Species=="scallop") %>% na.omit(), type="catch")%>% round(digits=3)

findSpeciesTheta(complete_tidy_diff %>% filter(Species=="scallop") %>% na.omit(), type="catch")
findSpeciesTheta_rho(complete_tidy_diff %>% filter(Species=="scallop") %>% na.omit(), type="catch")

#findSpeciesKPSS(complete_tidy_diff %>% filter(Species=="scallop") %>% na.omit(), type="wt")

#univariate dewdrop with areas as replicates
catchDiffInt <- complete_tidy_diff %>% mutate(areaInt = as.integer(paste0(Region, Stratum)))

do_xmap_ID(df=(catchDiffInt %>% filter(Species=="scallop", Type=="catch") %>% ungroup() %>% select(areaInt, Year, value)), predictor="value", target="value", ID_col="areaInt", E_max=7, tp=1)

make_xmap_block_ID(df=(catchDiffInt %>% filter(Species=="scallop", Type=="catch") %>% ungroup() %>% select(areaInt, Year, value)), predictor=value, target=value, ID_col=areaInt, E_max=7, cause_lag=1)

# write_clip(data.frame(t(do_xmap_ID(df=(catchDiffInt %>% filter(Species=="jonah", Type=="wt") %>% ungroup() %>% select(areaInt, Year, value)), predictor="value", target="value", ID_col="areaInt", E_max=7, tp=1))) %>% tail(n=12))

#Wt
findSpeciesE(complete_tidy_diff %>% filter(Species=="scallop") %>% na.omit(), type="wt")
findSpeciesErho(complete_tidy_diff %>% filter(Species=="scallop") %>% na.omit(), type="wt")

findSpeciesTheta(complete_tidy_diff %>% filter(Species=="scallop") %>% na.omit(), type="wt")
findSpeciesTheta_rho(complete_tidy_diff %>% filter(Species=="scallop") %>% na.omit(), type="wt")

do_xmap_ID(df=(catchDiffInt %>% filter(Species=="scallop", Type=="wt") %>% ungroup() %>% select(Year, areaInt, value)), predictor="value", target="value", ID_col="areaInt", E_max=7, tp=1)

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

# write_clip(findSpeciesGroups_both(complete_tidy_diff, type="catch", g="Stratum", species="jonah") %>% mutate(across(where(is.numeric), round, 3)))

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

################################################ Multispatial CCM - catch ########################


v_keep_col <- c("E","Tp","num_pred", "rho", "mae","rmse","perc","p_val","rho_linear", "mae_linear",
                "rmse_linear","perc_linear","p_val_linear", "predator", "prey", "direction")



# Areas as replicates
RESULTS_ccm_combos_areas <- pmap_dfr(params_ccm_combos,function(predator,prey){
  
  out_1 <- do_xmap_ID(catchCCMdf,predictor=predator,target=prey,ID_col = "area",E_max = 7, tp=1) %>% 
    mutate(predator=predator,
           prey=prey,
           direction= paste("predator","->","prey"))
  
  out_2 <- do_xmap_ID(catchCCMdf,predictor=prey,target=predator,ID_col = "area",E_max = 7, tp=1) %>% 
    mutate(predator=predator, 
           prey=prey,
           direction= paste("prey","->","predator"))
  
  bind_rows(out_1,out_2) %>% select(all_of(v_keep_col))
  
}) %>% addDirection()

regionsDf<- catchCCMdf %>% ungroup() %>% 
  group_by(Region, date) %>% 
  summarise(across(scallop:jonah, ~ mean(.x, na.rm = TRUE)))

strataDf<- catchCCMdf %>% ungroup() %>% 
  group_by(Stratum, date) %>% 
  summarise(across(scallop:jonah, ~ mean(.x, na.rm = TRUE)))

# Regions as replicates
RESULTS_ccm_combos_regions <- pmap_dfr(params_ccm_combos,function(predator,prey){
  
  out_1 <- do_xmap_ID(regionsDf,predictor=predator,target=prey,ID_col="Region",E_max = 7, tp=1)  %>%
    mutate(predator=predator,
           prey=prey,
           direction= paste("predator","->","prey"))
  
  out_2 <- do_xmap_ID(regionsDf,predictor=prey,target=predator,ID_col="Region",E_max = 7, tp=1) %>% 
    mutate(predator=predator,
           prey=prey,
           direction= paste("prey","->","predator"))
  
  bind_rows(out_1,out_2) %>% 
    select(all_of(v_keep_col))
  
}) %>% addDirection()

# Strata as replicates
RESULTS_ccm_combos_strata <- pmap_dfr(params_ccm_combos,function(predator,prey){
  
  out_1 <- do_xmap_ID(strataDf,predictor=predator,target=prey,ID_col="Stratum",E_max = 7, tp=1)  %>%
    mutate(predator=predator,
           prey=prey,
           direction= paste("predator","->","prey"))
  
  out_2 <- do_xmap_ID(strataDf,predictor=prey,target=predator,ID_col="Stratum",E_max = 7, tp=1) %>% 
    mutate(predator=predator,
           prey=prey,
           direction= paste("prey","->","predator"))
  
  bind_rows(out_1,out_2) %>% select(all_of(v_keep_col))
  
}) %>% addDirection()

################################################ Multispatial CCM - weight ########################



# Areas as replicates - weight
RESULTS_ccm_combos_areas_wt <- pmap_dfr(params_ccm_combos,function(predator,prey){
  
  out_1 <- do_xmap_ID(wtCCMdf,predictor=predator,target=prey,ID_col = "area",E_max = 7, tp=1) %>% 
    mutate(predator=predator,
           prey=prey,
           direction= paste("predator","->","prey"))
  
  out_2 <- do_xmap_ID(wtCCMdf,predictor=prey,target=predator,ID_col = "area",E_max = 7, tp=1) %>% 
    mutate(predator=predator, 
           prey=prey,
           direction= paste("prey","->","predator"))
  
  bind_rows(out_1,out_2) %>% select(all_of(v_keep_col))
  
}) %>% addDirection()


regionsDf_wt<- wtCCMdf %>% ungroup() %>% 
  group_by(Region, date) %>% 
  summarise(across(scallop:jonah, ~ mean(.x, na.rm = TRUE)))

strataDf_wt<- wtCCMdf %>% ungroup() %>% 
  group_by(Stratum, date) %>% 
  summarise(across(scallop:jonah, ~ mean(.x, na.rm = TRUE)))

# Regions as replicates - weight
RESULTS_ccm_combos_regions_wt <- pmap_dfr(params_ccm_combos,function(predator,prey){
  
  out_1 <- do_xmap_ID(regionsDf_wt,predictor=predator,target=prey,ID_col="Region",E_max = 7, tp=1)  %>%
    mutate(predator=predator,
           prey=prey,
           direction= paste("predator","->","prey"))
  
  out_2 <- do_xmap_ID(regionsDf_wt,predictor=prey,target=predator,ID_col="Region",E_max = 7, tp=1) %>% 
    mutate(predator=predator,
           prey=prey,
           direction= paste("prey","->","predator"))
  
  bind_rows(out_1,out_2) %>% 
    select(all_of(v_keep_col))
  
}) %>% addDirection()

# Strata as replicates - weight
RESULTS_ccm_combos_strata_wt <- pmap_dfr(params_ccm_combos,function(predator,prey){
  
  out_1 <- do_xmap_ID(strataDf_wt,predictor=predator,target=prey,ID_col="Stratum",E_max = 7, tp=1)  %>%
    mutate(predator=predator,
           prey=prey,
           direction= paste("predator","->","prey"))
  
  out_2 <- do_xmap_ID(strataDf_wt,predictor=prey,target=predator,ID_col="Stratum",E_max = 7, tp=1) %>% 
    mutate(predator=predator,
           prey=prey,
           direction= paste("prey","->","predator"))
  
  bind_rows(out_1,out_2) %>% select(all_of(v_keep_col))
  
}) %>% addDirection()

################################################ Non-multispatial CCM by area ########################

params_areas_ccm_combos<- expand.grid(predator=c("jonah", "rock"),
            prey=c("scallop", "rock"),
            areaInput=levels(as.factor(catchCCMdf$area)), stringsAsFactors = FALSE) %>% 
    filter(predator != prey) %>% 
    mutate(areaInput = as.integer(areaInput))

# By area, each individually - CATCH
RESULTS_ccm_combos_by_area <- pmap_dfr(params_areas_ccm_combos,function(predator,prey, areaInput){
  
  df_temp <- catchCCMdf %>% filter(area == areaInput)
  lib_vec <- paste(1, nrow(df_temp))

  rho_E_1<- EmbedDimension(dataFrame = df_temp, lib = lib_vec, pred = lib_vec, 
                           columns = predator,target = prey, maxE = 7, showPlot = FALSE)
  E_out_1<-rho_E_1[which.max(rho_E_1$rho),"E"][1]
  out_1 <- CCM(dataFrame= df_temp, columns=predator, target=prey, E = E_out_1, Tp=1,
               libSizes = paste(E_out_1+2, nrow(df_temp) - E_out_1, "1",sep=" "), sample=100, verbose=FALSE, showPlot = FALSE) %>%
    mutate(predator=predator,
           prey=prey,
           direction= paste("predator","->","prey"),
           area = areaInput, 
           E = E_out_1)

  rho_E_2<- EmbedDimension(dataFrame = df_temp, lib = lib_vec, pred = lib_vec, 
                           columns = prey,target = predator, maxE = 7, showPlot = FALSE)
  E_out_2<-rho_E_2[which.max(rho_E_1$rho),"E"][1]
  out_2 <- CCM(dataFrame= df_temp, columns=prey, target=predator, E = E_out_2, Tp=1, 
               libSizes = paste(E_out_2+2, nrow(df_temp)-E_out_2, "1",sep=" "), sample=100, verbose=FALSE, showPlot = FALSE)  %>%
    mutate(predator=predator,
           prey=prey,
           direction= paste("prey","->","predator"),
           area = areaInput,
           E = E_out_2)

  bind_rows(out_1,out_2)
  
}) %>% addDirection()

# By area, each individually - WEIGHT
RESULTS_ccm_combos_by_area_wt <- pmap_dfr(params_areas_ccm_combos,function(predator,prey, areaInput){
  
  df_temp <- wtCCMdf %>% filter(area == areaInput)
  lib_vec <- paste(1, nrow(df_temp))
  
  rho_E_1<- EmbedDimension(dataFrame = df_temp, lib = lib_vec, pred = lib_vec, 
                           columns = predator,target = prey, maxE = 7, showPlot = FALSE)
  E_out_1<-rho_E_1[which.max(rho_E_1$rho),"E"][1]
  out_1 <- CCM(dataFrame= df_temp, columns=predator, target=prey, E = E_out_1, Tp=1,
               libSizes = paste(E_out_1+2, nrow(df_temp) - E_out_1, "1",sep=" "), sample=100, verbose=FALSE, showPlot = FALSE) %>%
    mutate(predator=predator,
           prey=prey,
           direction= paste("predator","->","prey"),
           area = areaInput, 
           E = E_out_1)
  
  rho_E_2<- EmbedDimension(dataFrame = df_temp, lib = lib_vec, pred = lib_vec, 
                           columns = prey,target = predator, maxE = 7, showPlot = FALSE)
  E_out_2<-rho_E_2[which.max(rho_E_1$rho),"E"][1]
  out_2 <- CCM(dataFrame= df_temp, columns=prey, target=predator, E = E_out_2, Tp=1, 
               libSizes = paste(E_out_2+2, nrow(df_temp)-E_out_2, "1",sep=" "), sample=100, verbose=FALSE, showPlot = FALSE)  %>%
    mutate(predator=predator,
           prey=prey,
           direction= paste("prey","->","predator"),
           area = areaInput,
           E = E_out_2)
  
  bind_rows(out_1,out_2)
  
}) %>% addDirection()

ccm_col_order <- c("LibSize", "xmap", "area", "E", "jonah:scallop", "scallop:jonah", "rock:scallop", "scallop:rock","jonah:rock","rock:jonah")


RESULTS_ccm_combos_by_area <- RESULTS_ccm_combos_by_area %>% select(all_of(ccm_col_order))
RESULTS_ccm_combos_by_area_wt <- RESULTS_ccm_combos_by_area_wt %>% select(all_of(ccm_col_order))

find_max <- function(df) {
  df_out <- df %>% summarise(
    max_J_S = max(`jonah:scallop`, na.rm = TRUE),
    max_S_J = max(`scallop:jonah`, na.rm = TRUE),
    max_R_S = max(`rock:scallop`, na.rm = TRUE),
    max_S_R = max(`scallop:rock`, na.rm = TRUE),
    max_J_R = max(`jonah:rock`, na.rm = TRUE),
    max_R_J = max(`rock:jonah`, na.rm = TRUE))
  return(df_out)
}

max_rho_by_area<- RESULTS_ccm_combos_by_area %>% group_by(area) %>% find_max()

max_rho_by_area_wt<- RESULTS_ccm_combos_by_area_wt %>% group_by(area) %>% find_max()

find_delta = function(x) {
  delta_rho = last(x)-first(x)
 return(delta_rho)
}

min_max_lib <- RESULTS_ccm_combos_by_area %>% group_by(area, xmap) %>% 
  summarise(across(.cols=all_of(combos), find_delta))

converges<- pivot_longer(min_max_lib, cols=3:8) %>% na.omit() %>% mutate(optE = substr(xmap, 1, 3)==substr(name, 1, 3)) %>% group_by(area, name) %>% summarise(delta_rho = mean(value)) %>% filter(delta_rho >0)

no_convergence <- pivot_longer(min_max_lib, cols=3:8) %>% na.omit() %>% 
  mutate(optE = substr(xmap, 1, 3)==substr(name, 1, 3)) %>% group_by(area, name) %>% 
  summarise(delta_rho = mean(value)) %>% 
  filter(delta_rho <=0)

conv_table <- data.frame(rbind(table(converges$name), table(no_convergence$name))) %>% 
  mutate(conv = c("Yes", "No"), .before=1)

min_max_lib_wt <- RESULTS_ccm_combos_by_area_wt %>% group_by(area, xmap) %>% 
  summarise(across(.cols=all_of(combos), find_delta))

converges_wt<- pivot_longer(min_max_lib_wt, cols=3:8) %>% na.omit() %>% 
  mutate(optE = substr(xmap, 1, 3)==substr(name, 1, 3)) %>% 
  group_by(area, name) %>% summarise(delta_rho = mean(value)) %>%
  filter(delta_rho >0)

no_convergence_wt <- pivot_longer(min_max_lib_wt, cols=3:8) %>% na.omit() %>% 
  mutate(optE = substr(xmap, 1, 3)==substr(name, 1, 3)) %>% group_by(area, name) %>% 
  summarise(delta_rho = mean(value)) %>% 
  filter(delta_rho <=0)

conv_table_wt <- data.frame(rbind(table(converges_wt$name), table(no_convergence_wt$name))) %>% mutate(conv = c("Yes", "No"), .before=1)

#ggplot(data=pivot_longer(regionsGrid_tp1, cols=4:ncol(regionsGrid_tp1)))+geom_sf(aes(fill=value))+
#scale_fill_viridis_b()+facet_wrap(~name)

#ggplot(data=pivot_longer(regionsGrid_tp1, cols=4:ncol(regionsGrid_tp1)))+geom_sf(aes(fill=value#))+scale_fill_viridis_b()+facet_wrap(~name)

################################################ Non-multispatial CCM by region ########################

params_regions_ccm_combos<- expand.grid(predator=c("jonah", "rock"), prey=c("scallop", "rock"),
                                      region=c(1:5), stringsAsFactors = FALSE) %>% 
  filter(predator != prey)

# By region, each individually - CATCH
catchCCMdf_reg <- catchCCMdf %>% group_by(Region, date) %>% summarise(scallop = mean(scallop), rock = mean(rock), jonah = mean(jonah))

RESULTS_ccm_combos_by_reg <- pmap_dfr(params_regions_ccm_combos,function(predator,prey, region){
  df_temp <- catchCCMdf_reg %>% filter(Region == region)
  lib_vec <- paste(1, nrow(df_temp))
  rho_E_1<- EmbedDimension(dataFrame = df_temp, lib = lib_vec, pred = lib_vec,
                           columns = predator,target = prey, maxE = 7, showPlot = FALSE)
  E_out_1<-rho_E_1[which.max(rho_E_1$rho),"E"][1]
  out_1 <- CCM(dataFrame= df_temp, columns=predator, target=prey, E = E_out_1, Tp=1,
               libSizes = paste(E_out_1+2, nrow(df_temp) - E_out_1, "1",sep=" "), sample=100, verbose=FALSE, showPlot = TRUE) %>%
    mutate(predator=predator,
           prey=prey,
           direction= paste("predator","->","prey"),
           region=region,
           E = E_out_1)
  rho_E_2<- EmbedDimension(dataFrame = df_temp, lib = lib_vec, pred = lib_vec,
                           columns = prey,target = predator, maxE = 7, showPlot = FALSE)
  E_out_2<-rho_E_2[which.max(rho_E_1$rho),"E"][1]
  out_2 <- CCM(dataFrame= df_temp, columns=prey, target=predator, E = E_out_2, Tp=1,
               libSizes = paste(E_out_2+2, nrow(df_temp)-E_out_2, "1",sep=" "), sample=100, verbose=FALSE, showPlot = TRUE)  %>%
    mutate(predator=predator,
           prey=prey,
           direction= paste("prey","->","predator"),
           region=region,
           E = E_out_2)

  bind_rows(out_1,out_2)
}) %>% addDirection()

max_rho_by_reg<- RESULTS_ccm_combos_by_reg %>% group_by(region) %>% find_max()

# By region, each individually - weight
wtCCMdf_reg <- wtCCMdf %>% group_by(Region, date) %>% summarise(scallop = mean(scallop), rock = mean(rock), jonah = mean(jonah))

RESULTS_ccm_combos_by_reg_wt <- pmap_dfr(params_regions_ccm_combos,function(predator,prey, region){
  df_temp <- wtCCMdf_reg %>% filter(Region == region)
  lib_vec <- paste(1, nrow(df_temp))
  rho_E_1<- EmbedDimension(dataFrame = df_temp, lib = lib_vec, pred = lib_vec,
                           columns = predator,target = prey, maxE = 7, showPlot = FALSE)
  E_out_1<-rho_E_1[which.max(rho_E_1$rho),"E"][1]
  out_1 <- CCM(dataFrame= df_temp, columns=predator, target=prey, E = E_out_1, Tp=1,
               libSizes = paste(E_out_1+2, nrow(df_temp) - E_out_1, "1",sep=" "), sample=100, verbose=FALSE, showPlot = TRUE) %>%
    mutate(predator=predator,
           prey=prey,
           direction= paste("predator","->","prey"),
           region=region,
           E = E_out_1)
  rho_E_2<- EmbedDimension(dataFrame = df_temp, lib = lib_vec, pred = lib_vec,
                           columns = prey,target = predator, maxE = 7, showPlot = FALSE)
  E_out_2<-rho_E_2[which.max(rho_E_1$rho),"E"][1]
  out_2 <- CCM(dataFrame= df_temp, columns=prey, target=predator, E = E_out_2, Tp=1,
               libSizes = paste(E_out_2+2, nrow(df_temp)-E_out_2, "1",sep=" "), sample=100, verbose=FALSE, showPlot = TRUE)  %>%
    mutate(predator=predator,
           prey=prey,
           direction= paste("prey","->","predator"),
           region=region,
           E = E_out_2)
  
  bind_rows(out_1,out_2)
}) %>% addDirection()

max_rho_by_reg_wt<- RESULTS_ccm_combos_by_reg_wt %>% group_by(region) %>% find_max()

################################################ Non-multispatial CCM by stratum ########################

params_strata_ccm_combos<- expand.grid(predator=c("jonah", "rock"), prey=c("scallop", "rock"),
                                        stratum=c(1:4), stringsAsFactors = FALSE) %>% 
  filter(predator != prey)

# By stratum, each individually - CATCH
catchCCMdf_strat <- catchCCMdf %>% group_by(Stratum, date) %>% summarise(scallop = mean(scallop), rock = mean(rock), jonah = mean(jonah))


RESULTS_ccm_combos_by_strat <- pmap_dfr(params_strata_ccm_combos,function(predator,prey, stratum){
  df_temp <- catchCCMdf_strat %>% filter(Stratum == stratum)
  lib_vec <- paste(1, nrow(df_temp))
  rho_E_1<- EmbedDimension(dataFrame = df_temp, lib = lib_vec, pred = lib_vec,
                           columns = predator,target = prey, maxE = 7, showPlot = FALSE)
  E_out_1<-rho_E_1[which.max(rho_E_1$rho),"E"][1]
  out_1 <- CCM(dataFrame= df_temp, columns=predator, target=prey, E = E_out_1, Tp=1,
               libSizes = paste(E_out_1+2, nrow(df_temp) - E_out_1, "1",sep=" "), sample=100, verbose=FALSE, showPlot = TRUE) %>%
    mutate(predator=predator,
           prey=prey,
           direction= paste("predator","->","prey"),
           stratum=stratum,
           E = E_out_1)
  rho_E_2<- EmbedDimension(dataFrame = df_temp, lib = lib_vec, pred = lib_vec,
                           columns = prey,target = predator, maxE = 7, showPlot = FALSE)
  E_out_2<-rho_E_2[which.max(rho_E_1$rho),"E"][1]
  out_2 <- CCM(dataFrame= df_temp, columns=prey, target=predator, E = E_out_2, Tp=1,
               libSizes = paste(E_out_2+2, nrow(df_temp)-E_out_2, "1",sep=" "), sample=100, verbose=FALSE, showPlot = TRUE)  %>%
    mutate(predator=predator,
           prey=prey,
           direction= paste("prey","->","predator"),
           stratum=stratum,
           E = E_out_2)
  
  bind_rows(out_1,out_2)
}) %>% addDirection()

max_rho_by_strat<- RESULTS_ccm_combos_by_strat %>% group_by(stratum) %>% find_max()

# By stratum, each individually - weight
wtCCMdf_strat <- wtCCMdf %>% group_by(Stratum, date) %>% summarise(scallop = mean(scallop), rock = mean(rock), jonah = mean(jonah))


RESULTS_ccm_combos_by_strat_wt <- pmap_dfr(params_strata_ccm_combos,function(predator,prey, stratum){
  df_temp <- wtCCMdf_strat %>% filter(Stratum == stratum)
  lib_vec <- paste(1, nrow(df_temp))
  rho_E_1<- EmbedDimension(dataFrame = df_temp, lib = lib_vec, pred = lib_vec,
                           columns = predator,target = prey, maxE = 7, showPlot = FALSE)
  E_out_1<-rho_E_1[which.max(rho_E_1$rho),"E"][1]
  out_1 <- CCM(dataFrame= df_temp, columns=predator, target=prey, E = E_out_1, Tp=1,
               libSizes = paste(E_out_1+2, nrow(df_temp) - E_out_1, "1",sep=" "), sample=100, verbose=FALSE, showPlot = TRUE) %>%
    mutate(predator=predator,
           prey=prey,
           direction= paste("predator","->","prey"),
           stratum=stratum,
           E = E_out_1)
  rho_E_2<- EmbedDimension(dataFrame = df_temp, lib = lib_vec, pred = lib_vec,
                           columns = prey,target = predator, maxE = 7, showPlot = FALSE)
  E_out_2<-rho_E_2[which.max(rho_E_1$rho),"E"][1]
  out_2 <- CCM(dataFrame= df_temp, columns=prey, target=predator, E = E_out_2, Tp=1,
               libSizes = paste(E_out_2+2, nrow(df_temp)-E_out_2, "1",sep=" "), sample=100, verbose=FALSE, showPlot = TRUE)  %>%
    mutate(predator=predator,
           prey=prey,
           direction= paste("prey","->","predator"),
           stratum=stratum,
           E = E_out_2)
  
  bind_rows(out_1,out_2)
}) %>% addDirection()

max_rho_by_strat_wt<- RESULTS_ccm_combos_by_strat_wt %>% group_by(stratum) %>% find_max()

############ findAreasCorr -------------------------------------------------------

corrOut_areas <- data.frame(matrix(ncol=4))
corrOut_areas_wt <- data.frame(matrix(ncol=4))
colnames(corrOut_areas) <- c("area","s:r", "s:j", "r:j")
colnames(corrOut_areas_wt) <- c("area","s:r", "s:j", "r:j")

find_corr_area <- function(df, areaInput) {
  
  df_filtered <- df %>% filter(area == areaInput) 
  
  absmax <- function(x) { x[which.max( abs(x) )]}

  s.r_catch<-absmax(ccf(df_filtered$scallop, df_filtered$rock, type = "correlation",lag.max = 4, plot = FALSE)$acf)
  s.j_catch<-absmax(ccf(df_filtered$scallop, df_filtered$jonah, type = "correlation",lag.max = 4, plot = FALSE)$acf)
  r.j_catch<-absmax(ccf(df_filtered$rock, df_filtered$jonah, type = "correlation",lag.max = 4, plot = FALSE)$acf)

  catchV<- c(df_filtered$area[1], s.r_catch, s.j_catch, r.j_catch)
  df_area<- data.frame(t(catchV))
  colnames(df_area)<- c("area","s:r", "s:j", "r:j")
  df_area <- df_area %>% mutate(area = round(area))
  
  return(df_area)
}
#catch
for (i in 1:5) {
  for (j in 1:4) {
    areaTemp <- as.integer(paste0(i, j))
    corrOut_areas<- bind_rows(corrOut_areas,find_corr_area(df=catchCCMdf,areaInput=areaTemp))
  }
}
#weight
for (i in 1:5) {
  for (j in 1:4) {
    areaTemp <- as.integer(paste0(i, j))
    corrOut_areas_wt<- bind_rows(corrOut_areas_wt,find_corr_area(df=wtCCMdf,areaInput=areaTemp))
  }
}
corrOut_areas <- corrOut_areas %>% slice(-1)
corrOut_areas_wt <- corrOut_areas_wt %>% slice(-1)

########## CCM randomization #################
# compute_perm <- function(df, predictor,target,E,tp,keep_preds=FALSE){
#   
#   df_2 <- df %>% filter(complete.cases(.)) %>% slice_sample(prop=1)
#   L <- paste(1,nrow(df_2))
#   out <- Simplex(dataFrame=df_2, lib=L,pred=L, Tp=1,target=target,columns=predictor, embedded=FALSE,parameterList = TRUE,E=E)
#   
#   params <- out$parameters
#   out <- out$predictions %>% filter(complete.cases(.))
#   
#   stats <- compute_stats(out$Observations,out$Predictions)
#   stats$lib_size = L
#   stats$E=E
#   stats$Tp=1
#   
#   return(stats)
# }


########## Environment #################

envi_df <- df_tows %>%
  group_by(Region, Depth_Stratum, Season, Year) %>%
  summarise(temp = mean(Bottom_WaterTemp_DegC, na.rm = TRUE)) %>%
  rename(Stratum = Depth_Stratum) %>%
  mutate(area = as.numeric(paste0(Region, Stratum)), .before=temp) %>%
  mutate(date= paste(Year, case_when(Season== "Fall" ~ "-11-01", Season =="Spring" ~"-05-01"), sep = ""), .before=Region) %>% ungroup() %>%
  select(-c(Year, Season)) %>%
  mutate(date = lubridate::ymd(date)) %>%  filter(date < as.Date("2020-05-01"))

envi_df <- envi_df %>% 
  mutate(temp2 = case_when(
    !is.na(temp) ~ temp,
    is.na(temp)  ~ (lag(temp) + lead(temp))/2
  )) %>% select(-temp) %>% rename(temp = temp2)

envi <- envi_df %>% arrange(date) %>% group_by(area) %>% 
  mutate(temp = lag2(temp)) %>% 
  arrange(area)

envi_catch <- left_join(envi, catchCCMdf) %>% na.omit()
#envi_df %>% group_by(area) %>% summarise(num = n_distinct(date))

envi_ccm_combos<- expand.grid(species=c("scallop", "jonah", "rock"), areaInput=levels(as.factor(catchCCMdf$area)), stringsAsFactors = FALSE) %>%
  mutate(areaInput = as.integer(areaInput))


# By area, each individually - CATCH with depth
RESULTS_ccm_temp_by_area <- pmap_dfr(envi_ccm_combos,function(species, areaInput){
  
  df_temp <- envi_catch %>% filter(area == areaInput)
  lib_vec <- paste(1, nrow(df_temp))
  
  rho_E_1<- EmbedDimension(dataFrame = df_temp, lib = lib_vec, pred = lib_vec, 
                           columns = species,target = species, maxE = 7, showPlot = FALSE)
  E_out_1<-rho_E_1[which.max(rho_E_1$rho),"E"][1]
  out_1 <- CCM(dataFrame= df_temp, columns = "temp",target = species, E = E_out_1, Tp=1,
               libSizes = paste(E_out_1+2, nrow(df_temp) - E_out_1, "1",sep=" "), sample=100, verbose=FALSE, showPlot = FALSE) %>%
    mutate(species=species,
           area = areaInput, 
           E = E_out_1)
})

max_rho_temp_by_area <- RESULTS_ccm_temp_by_area %>% group_by(area) %>% 
 summarise(
    max_T_S = max(`temp:scallop`, na.rm = TRUE),
    max_S_T = max(`scallop:temp`, na.rm = TRUE),
    max_R_T = max(`rock:temp`, na.rm = TRUE),
    max_T_R = max(`temp:rock`, na.rm = TRUE),
    max_J_T = max(`jonah:temp`, na.rm = TRUE),
    max_T_J = max(`temp:jonah`, na.rm = TRUE))


p1 <- ggplot(data=regionsGrid)+geom_sf(aes(fill=max_R_S))+lims(fill = c(-0.3, .8))+labs(fill=NULL)+ 
  ggtitle('R->S')
p2 <- ggplot(data=regionsGrid)+geom_sf(aes(fill=max_S_R))+lims(fill = c(-0.3, .8))+labs(fill=NULL)+ 
  ggtitle('S->R')
p3 <- ggplot(data=regionsGrid)+geom_sf(aes(fill=max_R_J))+lims(fill = c(-0.3, .8))+labs(fill=NULL)+ 
  ggtitle('R->J')
p4 <- ggplot(data=regionsGrid)+geom_sf(aes(fill=max_J_R))+lims(fill = c(-0.3, .8))+labs(fill=NULL)+ 
  ggtitle('J->R')
p5 <- ggplot(data=regionsGrid)+geom_sf(aes(fill=max_J_S))+lims(fill = c(-0.3, .8))+labs(fill=NULL)+ 
  ggtitle('J->S')
p6 <- ggplot(data=regionsGrid)+geom_sf(aes(fill=max_S_J))+lims(fill = c(-0.3, .8))+labs(fill=NULL)+ 
  ggtitle('S->J')

((p1 + p2) / (p3 + p4) / (p5 + p6) ) +plot_layout(guides = 'collect') #& 
  #scale_colour_continuous(limits = range(c(-0.3, 0.7)))

t1 <- ggplot(data=regionsGrid)+geom_sf(aes(fill=max_T_S))+lims(fill = c(-0.3, .6))+labs(fill=NULL)+ 
  ggtitle('T->S')
t2 <- ggplot(data=regionsGrid)+geom_sf(aes(fill=max_S_T))+lims(fill = c(-0.3, .6))+labs(fill=NULL)+ 
  ggtitle('S->T')
t3 <- ggplot(data=regionsGrid)+geom_sf(aes(fill=max_T_R))+lims(fill = c(-0.3, .6))+labs(fill=NULL)+ 
  ggtitle('T->R')
t4 <- ggplot(data=regionsGrid)+geom_sf(aes(fill=max_R_T))+lims(fill = c(-0.3, .6))+labs(fill=NULL)+ 
  ggtitle('R->T')
t5 <- ggplot(data=regionsGrid)+geom_sf(aes(fill=max_T_J))+lims(fill = c(-0.3, .6))+labs(fill=NULL)+ 
  ggtitle('T->J')
t6 <- ggplot(data=regionsGrid)+geom_sf(aes(fill=max_J_T))+lims(fill = c(-0.3, .6))+labs(fill=NULL)+ 
  ggtitle('J->T')

((t1 + t2) / (t3 + t4) / (t5 + t6) ) +plot_layout(guides = 'collect')# & scale_fill_viridis_b()

########## Neighbors #################

surveyGrid <-st_read("~/Downloads/lab_notebook/Maine/MaineDMR_-_Inshore_Trawl_Survey_Grid") #CRS: WGS 84/EPSG 4326

surveyGrid <- surveyGrid %>% 
  mutate(Region = region_id,
         Stratum = depth_stra,
         GridID = grid_id, .keep="unused", .before=last_surve)

surveyGrid$area <- as.integer(paste0(surveyGrid$Region, surveyGrid$Stratum))

regionsGrid_orig <- surveyGrid %>% group_by(area) %>% summarise(num = n_distinct(GridID))
plot(regionsGrid_orig[1])
regionsGrid <- left_join(regionsGrid_orig, max_rho_temp_by_area)
regionsGrid <- left_join(regionsGrid_orig, max_rho_by_area)

scalCatchFall<- catchTidy_seasons %>% mutate(area = as.integer(paste0(Region, Stratum))) %>% filter(Type=="catch", Season=="Fall") %>% group_by(area, Species) %>% summarise(avg = mean(value))

scalCatchFall <- left_join(regionsGrid_orig, scalCatchFall)

scalCatchSpring<- catchTidy_seasons %>% mutate(area = as.integer(paste0(Region, Stratum))) %>% filter(Type=="catch", Season=="Spring") %>% group_by(area, Species) %>% summarise(avg = mean(value))

scalCatchSpring <- left_join(regionsGrid_orig, scalCatchSpring)

ggplot(data=scalCatchFall)+geom_sf(aes(fill=log(avg+1)))+facet_wrap(~Species)+
scale_fill_viridis_c(option="F", name="catch")

ggplot(data=scalCatchFall %>% filter(Species != "scallop"))+geom_sf(aes(fill=log(avg+1)))+facet_wrap(~Species)+
  scale_fill_viridis_c(option="F", name="log catch")

ggplot(data=scalCatchSpring %>% filter(Species != "scallop"))+geom_sf(aes(fill=log(avg+1)))+facet_wrap(~Species)+
  scale_fill_viridis_c(option="F", name="log catch")

jonahCatch <- left_join(regionsGrid_orig, catchTidy_seasons %>% mutate(area = as.integer(paste0(Region, Stratum))) %>% filter(Species=="jonah"))
                        #%>% group_by(area, Type, Season) %>% summarise(avg = mean(value)))

jonahCatchFall <- jonahCatch %>% filter(Season == "Fall", Type == "catch") %>% rename(valueFall = value) %>% select(-Season)
jonahCatchSpring <- jonahCatch %>% filter(Season == "Spring", Type == "catch") %>% st_drop_geometry() %>% rename(valueSpring = value) %>% select(-Season)

jonahCatchDiff <- left_join(jonahCatchFall, jonahCatchSpring) %>% arrange(area, Year)
jonahCatchDiff <- jonahCatchDiff %>% mutate(diff = valueFall -valueSpring)

#difference fall vs spring by year
ggplot(data = jonahCatchDiff %>% filter(Type=="catch") %>% filter(Year != 2000 & Year != 2020))+geom_sf(aes(fill=diff))+
scale_fill_viridis_b(name="fall catch minus spring catch")+facet_wrap(~Year)

#Fall catch by year
ggplot(data = jonahCatch %>% filter(Type=="catch", Season=="Fall") %>% filter(Year != 2000 & Year != 2020))+geom_sf(aes(fill=value))+
  scale_fill_viridis_b()+facet_wrap(~Year)

#line graph of abundance over time by season, no spatial distinction
ggplot(data = catchTidy_seasons %>% filter(Type=="catch", Species=="jonah") %>% group_by(Year, Season) %>% summarise(value = mean(value)))+geom_line(aes(x=Year, y=value))+facet_wrap(~Season)+theme_classic()+labs(y="Abundance (catch/tow)")



                                                                                                                                                       