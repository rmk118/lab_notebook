#Load packages
library(tidyverse)
library(lubridate) #date formatting
library(patchwork) #combining plots
library(viridis)
library(rEDM) #EDM
library(mgcv) #GAMs
# Time series packages
library(tseries)
library(zoo)
library(forecast)

df_tows<-read.csv("data/Maine_inshore_trawl/MEtows.csv") #tow data
df_s_cat<- read.csv("data/Maine_inshore_trawl/MEscallopCatch.csv") #scallop catch
df_r_cat<- read.csv("data/Maine_inshore_trawl/MErockCatch.csv") #rock crab catch
df_j_cat<- read.csv("data/Maine_inshore_trawl/MEjonahCatch.csv") #jonah crab catch

cleanCatch <- function(x) {
  full_join(x, df_tows) %>%
    arrange(Survey, Tow_Number) %>% 
    select(-c("Stratum", "Subsample_Weight_kg", "Subsample_Weight_kg_2", "Male_Wt_kg", "Female_Wt_kg","Date",  "Surface_Salinity", "End_Latitude","End_Longitude", "Air_Temp", "Tow_Time")) %>%
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


# Strata analysis ---------------------------------------------------------
library(sf)
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

regionsGrid_orig <- surveyGrid %>% group_by(Stratum) %>% summarise(num = n_distinct(GridID))
regionsGrid <- left_join(regionsGrid_orig %>% mutate(Stratum = as.factor(Stratum)), catchTidy_strat_complete %>% filter(Type=="catch",) %>% group_by(Stratum, date, Species))

j <- j_cat_strat  %>% filter(Year > 2000) %>% 
  mutate(date=paste(Year, case_when(Season== "Fall" ~ "-11-01", Season =="Spring" ~"-05-01"), sep = "")) %>% arrange(Stratum, date)

j <- j %>%  group_by(Stratum, Season) %>% mutate(avgCatch = na.spline(avgCatch),
                                            avgWt = na.spline(avgWt),
                                            temp = na.spline(temp))

do_xmap_ID(df = j, predictor = "avgCatch", target="temp",ID_col="Stratum",E_max=8, tp=1)%>% select(E, rho)
do_xmap_ID(df = j, predictor = "temp", target="avgCatch",ID_col="Stratum",E_max=8, tp=1)%>% select(E, rho)

do_xmap_ID(df = j, predictor = "avgWt", target="temp",ID_col="Stratum",E_max=8, tp=1)%>% select(E, rho)
do_xmap_ID(df = j, predictor = "temp", target="avgWt",ID_col="Stratum",E_max=8, tp=1)%>% select(E, rho)

# Linear + GAM/GAMM models -----------------------------------------------------------

tsdf <- tsdf %>% mutate(yrs = index(ts1))
jgam1 <- gam(value ~ s(yrs), data=tsdf[train,])
summary(jgam1)
# gam(value ~ s(date), data=tsdf[train,]) #produces a model essentially equal to s(yrs)

jgam2 <- gam(value ~ s(Year, bs = "gp") + Season, data=tsdf[train,]) #better than default
summary(jgam2)
acf(jgam2$residuals)

jgam3 <- gam(value ~ s(Year) + Season, data=tsdf[train,])
summary(jgam3)


gam_ar1 <- gamm(value ~ s(Year, bs = "gp") + Season, data=tsdf[train,], correlation = corARMA(form = ~ 1|Year, p = 1))
summary(gam_ar1$gam)
acf(gam_ar1$gam$residuals)

gam_ar1b <- gamm(value ~ s(Year) + Season, data=tsdf[train,], correlation = corARMA(form = ~ 1|Year, p = 1))
summary(gam_ar1b$gam)
acf(gam_ar1b$lme$residuals)

gam_ar2 <- gamm(value ~ s(Year, bs = "gp") + Season, data=tsdf[train,], correlation = corARMA(form = ~ 1|Year, p = 2))
summary(gam_ar2$gam)
acf(gam_ar2$gam$residuals)

anova(gam_ar1$lme, gam_ar1b$lme)

plot(jgam1)
predict.gam(jgam1,newdata=tsdf[test,] )

# in-sample
gamPlot_manual <- ggplot()+
    geom_path(data = tsdf[train,], aes(x = yrs, y = value)) + 
   geom_path(aes(x=tsdf[train,]$yrs, y=jgam2$fitted.values,color="s(Year, bs=gp) + Season"))+ 
  geom_path(aes(x=tsdf[train,]$yrs, y=jgam1$fitted.values,color="s(yrs)"))+ 
  geom_path(aes(x=tsdf[train,]$yrs, y=jgam3$fitted.values,color="s(Year) + Season"))+
  geom_path(aes(x=tsdf[train,]$yrs, y=gam_ar1$gam$fitted.values,color="s(Year), bs=gp + Season + AR1"))
gamPlot_manual

gamPlot_manual_out <- ggplot()+
  geom_path(data = tsdf[train,], aes(x = yrs, y = value)) + 
  geom_path(aes(x=tsdf[test,]$yrs, y=predict.gam(jgam1,newdata=tsdf[test,] ),color="s(yrs)"))+ 
  geom_path(aes(x=tsdf[test,]$yrs, y=predict.gam(jgam3,newdata=tsdf[test,] ),color="s(Year) + Season"))+
  geom_path(aes(x=tsdf[test,]$yrs, y=predict.gam(jgam2,newdata=tsdf[test,] ),
                color="s(Year, bs=gp) + Season"))+ 
  geom_path(aes(x=tsdf[test,]$yrs, y=predict.gam(gam_ar1$gam,newdata=tsdf[test,] ),
                color="s(Year, bs=gp) + Season + AR1"))
gamPlot_manual_out

tslm1<- tslm(value ~ Year + season + trend, data=window(ts1, 2001.0, 2015.5))
summary(tslm1)
plot(tslm1$fitted.values)

# Aggregate ---------------------------------------------------------------

summaryAgg <- function(df) {
  df %>% group_by(Season, Year) %>%
    summarise(avgCatch = mean(Expanded_Catch, na.rm=TRUE),
              avgWt = mean(Expanded_Weight_kg, na.rm=TRUE),
              temp = mean(Bottom_WaterTemp_DegC, na.rm=TRUE)) 
}

#computes averages for each stratum
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
  select(-name)

catch_agg_complete <- complete(data=catch_agg %>% ungroup(), Season, Year) %>% 
  mutate(date=paste(Year, case_when(Season== "Fall" ~ "-11-01", Season =="Spring" ~"-05-01"), sep = ""))%>% 
  filter(Year > 2000)

catchTidy_agg_complete<- complete(data = catchTidy_agg %>% ungroup(),Season, Year) %>% 
  mutate(date=paste(Year, case_when(Season== "Fall" ~ "-11-01", Season =="Spring" ~"-05-01"), sep = "")) %>% 
  filter(Year > 2000)
