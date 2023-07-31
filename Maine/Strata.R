#Load packages
library(tidyverse)
library(lubridate) #date formatting
library(patchwork) #combining plots
library(viridis)
library(rEDM) #EDM
library(sf) #for spatial data
library(tseries)

df_tows<-read.csv("data/Maine_inshore_trawl/MEtows.csv") #tow data
df_s_cat<- read.csv("data/Maine_inshore_trawl/MEscallopCatch.csv") #scallop catch
df_r_cat<- read.csv("data/Maine_inshore_trawl/MErockCatch.csv") #rock crab catch
df_j_cat<- read.csv("data/Maine_inshore_trawl/MEjonahCatch.csv") #jonah crab catch

surveyGrid <-st_read("~/Downloads/lab_notebook/Maine/MaineDMR_-_Inshore_Trawl_Survey_Grid") #CRS: WGS 84/EPSG 4326

surveyGrid <- surveyGrid %>% 
  mutate(Region = region_id,
         Stratum = depth_stra,
         GridID = grid_id, .keep="unused", .before=last_surve)

surveyGrid$area <- as.numeric(paste0(surveyGrid$Region, surveyGrid$Stratum))

cleanCatch <- function(x) {
  full_join(x, df_tows) %>%
    arrange(Survey, Tow_Number) %>% 
    select(-c("Stratum", "Subsample_Weight_kg", "Subsample_Weight_kg_2", "Male_Wt_kg", "Female_Wt_kg","Date", "Surface_WaterTemp_DegC", "Surface_Salinity", "End_Latitude","End_Longitude", "Air_Temp", "Tow_Time")) %>%
    mutate(Number_Caught = replace_na(Number_Caught,0),
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

#Reorder columns
colOrder<-c("Survey", "Tow_Number", "Region", "Stratum", "Expanded_Catch", 
            "Expanded_Weight_kg", "Date", "Common_Name", "Number_Caught", "Weight_kg",
            "Start_Latitude", "Start_Longitude","Season",
            "Year","Grid", "Start_Depth_fathoms", "End_Depth_fathoms",
            "Bottom_WaterTemp_DegC", "Bottom_Salinity")

j_cat_clean_seasons <- j_cat_clean_seasons %>% select(all_of(colOrder))
r_cat_clean_seasons <- r_cat_clean_seasons %>% select(all_of(colOrder))
s_cat_clean_seasons <- s_cat_clean_seasons %>% select(all_of(colOrder))

summaryStrat <- function(df) {
  df %>% group_by(Season, Year, Stratum) %>%
    summarise(avgCatch = mean(Expanded_Catch, na.rm=TRUE),
              avgWt = mean(Expanded_Weight_kg, na.rm=TRUE),
              temp = mean(Bottom_WaterTemp_DegC, na.rm=TRUE)) 
}

#computes averages for each stratum
j_cat_strat <- summaryStrat(j_cat_clean_seasons)
r_cat_strat <- summaryStrat(r_cat_clean_seasons)
s_cat_strat <- summaryStrat(s_cat_clean_seasons)

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
  select(-name)

catch_strat_complete <- complete(data=catch_strat %>% ungroup(), Stratum, Season, Year) %>% 
 mutate(date=paste(Year, case_when(Season== "Fall" ~ "-11-01", Season =="Spring" ~"-05-01"), sep = ""), .before=Stratum) %>%
   filter(as.Date(date) > as.Date("2003-05-01"))

catchTidy_strat_complete<- complete(data = catchTidy_strat %>% ungroup(),Stratum, Season, Year) %>% 
  mutate(date=paste(Year, case_when(Season== "Fall" ~ "-11-01", Season =="Spring" ~"-05-01"), sep = ""), .before=Stratum) %>%
  filter(as.Date(date) > as.Date("2003-05-01"))

ggplot(data = catchTidy_strat %>% filter(Type=="catch", Species=="jonah") %>% group_by(Year, Season) %>% summarise(value = mean(value, na.rm=TRUE)))+geom_line(aes(x=Year, y=value))+facet_wrap(~Season)+theme_classic()+labs(y="Abundance (catch/tow)")

ggplot(data = catchTidy_strat_complete %>% filter(Type == "catch", Species != "scallop") %>% group_by(date, Species) %>% 
         summarise(avg = mean(value, na.rm = TRUE)), aes(x=as.Date(date), y=avg))+geom_line()+facet_wrap(~Species)+labs(y="catch")+theme_classic()


ggplot(data = catchTidy_strat_complete %>% filter(Type == "catch", Species != "scallop") %>% group_by(Year, Season, Species) %>% 
         summarise(avg = mean(value, na.rm = TRUE)), aes(x=Year, y=avg))+geom_line()+facet_grid(Season~Species)+labs(y="catch")+theme_classic()


regionsGrid_orig <- surveyGrid %>% group_by(Stratum) %>% summarise(num = n_distinct(GridID))
regionsGrid <- left_join(regionsGrid_orig %>% mutate(Stratum = as.factor(Stratum)), catchTidy_strat_complete %>% filter(Type=="catch",) %>% group_by(Stratum, date, Species))

ggplot(data=regionsGrid %>% filter(Species != "scallop") %>% group_by(Species, Stratum) %>% summarise(avg = mean(value, na.rm=TRUE)))+
  geom_sf(aes(fill=avg))+facet_wrap(~Species)#+scale_fill_viridis_c()

regionsGrid_seasons <- left_join(regionsGrid_orig %>% mutate(Stratum = as.factor(Stratum)), catchTidy_strat_complete %>% filter(Type=="catch"))

ggplot(data=regionsGrid_seasons %>% group_by(Stratum, Species, Season) %>% summarize(avg = mean(value, na.rm=TRUE)))+geom_sf(aes(fill=avg))+facet_grid(Season~Species)+scale_fill_viridis_c(option = "F", name="avg catch")

lag2 <- function(x) {
  x_lagged <- (x - lag(x, 2))
  return(x_lagged)
} 

catch_complete_diff <- catch_strat_complete %>% arrange(date) %>% group_by(Stratum) %>%
  mutate(across(where(is.double) & !date, lag2)) %>% 
  filter(date != "2003-11-01" & date != "2004-05-01") %>%  filter(as.Date(date) < as.Date("2020-05-01"))

complete_tidy_diff <- pivot_longer(catch_complete_diff,cols =starts_with("avg")) %>% 
  mutate(Type = case_when(
    startsWith(name, "avgCatch_") ~"catch",
    startsWith(name,"avgWt_") ~"wt")) %>% 
  mutate(Species = case_when(
    endsWith(name, "s") ~"scallop",
    endsWith(name, "r") ~"rock",
    endsWith(name, "j") ~"jonah")) %>%
  mutate(Species = as.factor(Species),
         Type = as.factor(Type),
         Stratum = as.factor(Stratum)) %>% 
  select(-name)  

#All areas on one graph, split by species
ggplot(data = complete_tidy_diff %>% 
         filter(Type == "catch", Species != "scallop"), aes(x=date, y=value, group=Stratum,color=Stratum))+geom_line()+
  facet_wrap(~Species) +
  labs(y="2nd-differenced catch", x="Year")

#Colored by species, split by area
ggplot(data = complete_tidy_diff %>% filter(Type == "catch", Species != "scallop"), 
       aes(x=date, y=value, group=Species,color=Species))+
  geom_line()+
  facet_wrap(~Stratum)+
  labs(x="Depth stratum")+
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))


EmbedDimension(dataFrame=complete_tidy_diff %>% filter(Species=="jonah", Type=="catch") %>% group_by(date) %>% 
                 summarise(avg = mean(value, na.rm = TRUE)) %>% 
                 ungroup() %>% select(date, avg),  columns ="avg", target="avg", lib = "1 31", pred="1 31")


params_ccm_combos <- data.frame(jonah=c("jonah", "jonah"), prey=c("scallop", "rock"))
catch_ccm <- catch_complete_diff %>% select(-c(Season, avgWt_s, avgWt_j, avgWt_r)) %>% rename("jonah" = "avgCatch_j", "rock"="avgCatch_r", "scallop"="avgCatch_s")


v_keep_col <- c("E","Tp","num_pred", "rho", "mae","rmse","perc","p_val","rho_linear", "mae_linear",
                "rmse_linear","perc_linear","p_val_linear")

# Strata as replicates
RESULTS_ccm_rep_strata <- pmap_dfr(params_ccm_combos,function(jonah,prey){
  
  out_1 <- do_xmap_ID(catch_ccm,predictor=jonah,target=prey,ID_col="Stratum",E=3, tp=1)  %>%
    mutate(direction= paste("jonah","->",prey))
  
  out_2 <- do_xmap_ID(catch_ccm,predictor=prey,target=jonah,ID_col="Stratum",E=3, tp=1) %>% 
    mutate(direction= paste(prey,"->","jonah"))
  
  bind_rows(out_1,out_2) %>% select(direction, all_of(v_keep_col))  %>% mutate(type="catch", replicate = "stratum")
})

do_xmap_ID(catch_ccm,predictor="jonah",target="temp",ID_col="Stratum",E=3, tp=1) %>% select(rho)
do_xmap_ID(catch_ccm,predictor="temp",target="jonah",ID_col="Stratum",E=3, tp=1) %>% select(rho)

#WEIGHT
strataDf_wt<- wtCCMdf %>% ungroup() %>% 
  group_by(Stratum, date) %>% 
  summarise(across(scallop:jonah, ~ mean(.x, na.rm = TRUE)))

# Strata as replicates - weight
RESULTS_ccm_wt_combos_strata <- pmap_dfr(params_ccm_combos,function(jonah,prey){
  
  out_1 <- do_xmap_ID(strataDf_wt,predictor=jonah,target=prey,ID_col="Stratum",E=2, tp=1)  %>%
    mutate(direction= paste("jonah","->",prey))
  
  out_2 <- do_xmap_ID(strataDf_wt,predictor=prey,target=jonah,ID_col="Stratum",E=2, tp=1) %>% 
    mutate(direction= paste(prey,"->","jonah"))
  
  bind_rows(out_1,out_2) %>% select(direction, all_of(v_keep_col)) %>% mutate(type="wt", replicate = "stratum") })