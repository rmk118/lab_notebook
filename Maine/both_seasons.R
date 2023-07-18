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

logCatch <- catchTidy_seasons %>% filter(Type == "logCatch")
logWt <- catchTidy_seasons %>% filter(Type == "logWt") %>% 
  mutate(date=paste(Year, case_when(Season== "Fall" ~ "-11-01",
                                    Season =="Spring" ~"-05-01"), sep = ""))



logWt %>% group_by(area, Species) %>% arrange(Year) %>% select(value) %>% 
  group_map(~ts(., frequency = 2, start=c(2000, 2)))

logWt_complete <- complete(data=logWt %>% ungroup(), date, Species, Region, Stratum, explicit = TRUE) %>% mutate(area = paste(Region, Stratum), Type="logWt") %>% select(date, Species, area, value)

logWt_areas <- logWt_complete %>% group_by(area, Species) %>% arrange(date) %>% select(value) %>% 
  group_map(~ts(., frequency = 2, start=c(2000, 2)))

logWt_areas2 <- logWt_complete %>% group_by(area) %>% arrange(date) %>% select(Species, value) %>% 
  group_map(~ts(., frequency = 2, start=c(2000, 2)))


catch_complete <- complete(data=catch_seasons %>% ungroup(), Region, Stratum, Season, Year) %>% 
  mutate(area = paste(Region, Stratum)) %>% 
  mutate(date=paste(Year, case_when(Season== "Fall" ~ "-11-01",
                                    Season =="Spring" ~"-05-01"), sep = ""), .before=Region) %>% 
  filter(date != "2000-05-01")

catch_ts <- catch_complete %>% group_by(area) %>% arrange(date) %>% select(avgLogWt_s) %>% 
  group_map(~ts(., frequency = 2, start=c(2000, 2)))# .keep = FALSE))


#ggplot(data=logCatch, aes(x=Year, y=value, color=Species)) +geom_line()+facet_grid(Region~Stratum)

# logWt_ts <- ts(logWt, frequency = 2)
# logCatch_ts <- ts(logCatch, frequency = 2)
# catch_seasons_ts <- ts(catch_seasons %>% ungroup() %>% filter(area== "1 1") %>% select(Season, Year,avgLogCatch_s) %>% arrange(Year), frequency = 2)
# 
# autoplot(catch_seasons_ts)
# monthplot(catch_seasons_ts, phase = "Season")
# 
# s_cat_seasons <- ts(s_cat_clean %>% filter(area== "1 1") %>% select(Date,Tow_Number, Region, Stratum, logWt) %>% group_by(Date, Region, Stratum) %>% mutate(date = as.POSIXct(Date)) %>% summarise(lWt = mean(logWt)))
library(ggfortify)
library(xts)

plot(catch_ts[[1]])
Box.test(catch_ts[[1]], type = "Ljung-Box")
#apply(catch_ts[[1]], 2, kpss.test, null = "Trend")                      

decompose(catch_ts2)
plot(decompose(catch_ts2))
ggAcf(catch_ts[[1]])

catch_ts2 <- na.interp(catch_ts[[1]])
catch_ts2[[1]] %>%
  stl() %>%
  autoplot()

# catch_ts2diff <- diff(catch_ts2, lag=2, differences = 1)
# plot(decompose(catch_ts2diff))
