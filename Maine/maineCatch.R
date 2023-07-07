#ME-NH Inshore Trawl Survey - Catch/Weight Analysis
#Ruby Krasnow
#Last modified: July 7, 2023

library(tidyverse)
library(patchwork)
library(lubridate)

#spatial packages
library(sf)
library(sfheaders)

library(rEDM)

df_s_cat<- read.csv("data/Maine_inshore_trawl/MEscallopCatch.csv") #scallop catch
df_r_cat<- read.csv("data/Maine_inshore_trawl/MErockCatch.csv") #rock crab catch
df_j_cat<- read.csv("data/Maine_inshore_trawl/MEjonahCatch.csv") #jonah crab catch
df_tows<-read.csv("data/Maine_inshore_trawl/MEtows.csv") #tow data

numTows <- df_tows %>% group_by(Survey) %>% summarise(tows = n_distinct(Tow_Number))

cleanCatch <- function(x) {
    full_join(x, df_tows) %>%
    arrange(Survey, Tow_Number) %>% 
    select(-c("Stratum", "Subsample_Weight_kg_2", "Date", "Surface_WaterTemp_DegC", "Surface_Salinity", "Air_Temp", "Tow_Time")) %>%
    mutate(Number_Caught = replace_na(Number_Caught,0),
           Weight_kg = replace_na(Weight_kg,0),
           Expanded_Catch = replace_na(Expanded_Catch,0),
           Expanded_Weight_kg = replace_na(Expanded_Weight_kg,0)) %>% 
    mutate(Stratum = Depth_Stratum, Date = date(ymd_hms(Start_Date)), .keep="unused")
}

r_catFull <- cleanCatch(df_r_cat) %>% 
  mutate(Common_Name = "Rock crab")
j_catFull <- cleanCatch(df_j_cat) %>% 
  mutate(Common_Name = "Jonah crab")
s_catFull <- cleanCatch(df_s_cat) %>% 
  mutate(Common_Name = "Scallop")

rm(df_j_cat, df_r_cat, df_s_cat)

colOrder<-c("Survey", "Tow_Number", "Region", "Stratum", "Expanded_Catch", 
  "Expanded_Weight_kg", "Date", "Common_Name", "Number_Caught", "Weight_kg",
  "Start_Latitude", "Start_Longitude", "End_Latitude", "End_Longitude","Season",
  "Year","Grid","Subsample_Weight_kg","Male_Wt_kg", "Female_Wt_kg",
  "Start_Depth_fathoms", "End_Depth_fathoms", "Tow_Type","Tow_LengthNM",
  "Bottom_WaterTemp_DegC", "Bottom_Salinity")

j_catFull <- j_catFull %>% select(all_of(colOrder))
r_catFull <- r_catFull %>% select(all_of(colOrder))
s_catFull <- s_catFull %>% select(all_of(colOrder))

summaryCatch <- function(df) {
  df %>% group_by(Season, Year, Region,Stratum) %>%
  summarise(avgCatch = mean(Expanded_Catch),
            avgWt = mean(Expanded_Weight_kg))
  }

j_cat <- summaryCatch(j_catFull)
r_cat <- summaryCatch(r_catFull)
s_cat <- summaryCatch(s_catFull)

catch <- s_cat %>% left_join(j_cat, by=c("Season", "Region", "Stratum", "Year"), suffix = c("_s", "_j"))
catch <- catch %>% left_join(r_cat, by=c("Season", "Region", "Stratum", "Year")) %>%
          mutate(avgCatch_r = avgCatch, avgWt_r = avgWt, .keep="unused")

catch$logJ_catch <- log(catch$avgCatch_j+1)
catch$logR_catch <- log(catch$avgCatch_r+1)
catch$logS_catch <- log(catch$avgCatch_s+1)

catch$logJ_wt <- log(catch$avgWt_j+1)
catch$logR_wt <- log(catch$avgWt_r+1)
catch$logS_wt <- log(catch$avgWt_s+1)

# ggplot(catch, aes(x=logS_catch,y=logJ_catch, color=Year))+geom_point()+theme_classic()+geom_smooth(method = "lm")+labs(x="scallops", y="jonah crabs")+facet_grid(Region~Stratum) #+ scale_color_gradientn(colours = rainbow(30))
# ggplot(catch, aes(x=logS_catch,y=logR_catch, color=Year))+geom_point()+theme_classic()+labs(x="scallops", y="rock crabs")+geom_smooth(method = "lm")+facet_grid(Region~Stratum)
# ggplot(catch, aes(x=logJ_catch,y=logR_catch, color=Year))+geom_point()+theme_classic()+labs(x="jonah crabs", y="rock crabs")
# ggplot(catch, aes(x=logS_catch,y=logJ_catch, color=Year))+geom_point()+theme_classic()+labs(x="scallops", y="jonah crabs")+facet_wrap(~Stratum)

catchTidy <- pivot_longer(catch, 
        cols = 5:16) %>% 
  mutate(Type = case_when(
    startsWith(name, "avgCatch_") ~"avgCatch",
    startsWith(name,"avgWt_") ~"avgWt",
    endsWith(name,"_wt") ~"logWt",
    endsWith(name,"_catch") ~"logCatch")) %>% 
  mutate(Species = case_when(
    startsWith(name, "logS") | endsWith(name, "s") ~"scallop",
    startsWith(name, "logR") | endsWith(name, "r") ~"rock",
    startsWith(name, "logJ") | endsWith(name, "j") ~"jonah"))

catchTidy <- catchTidy %>% mutate(Species = as.factor(Species),Season = as.factor(Season),Region = as.factor(Region), Stratum = as.factor(Stratum)) %>% 
          select(-name)

avgCatchFall <- catchTidy %>% filter(Type == "avgCatch") %>% filter(Season == "Fall")
avgWtFall <- catchTidy %>% filter(Type == "avgWt") %>% filter(Season == "Fall")
logCatchFall <- catchTidy %>% filter(Type == "logCatch") %>% filter(Season == "Fall")
logWtFall <- catchTidy %>% filter(Type == "logWt") %>% filter(Season == "Fall")

avgCatchSpring <- catchTidy %>% filter(Type == "avgCatch") %>% filter(Season == "Spring")
logCatchSpring <- catchTidy %>% filter(Type == "logCatch") %>% filter(Season == "Spring")
avgWtSpring <- catchTidy %>% filter(Type == "avgWt") %>% filter(Season == "Spring")
logWtSpring <- catchTidy %>% filter(Type == "logWt") %>% filter(Season == "Spring")

ggplot(logCatchSpring, aes(x=Year,y=value, color=Species))+geom_line()+theme_classic()+labs(x="Time", y="Log catch")+facet_grid(Region~Stratum)

ggplot(logCatchFall, aes(x=Year,y=value, color=Species))+geom_line()+theme_classic()+labs(x="Time", y="Log catch")+facet_grid(Region~Stratum)

ggplot(logWtSpring, aes(x=Year,y=value, color=Species))+geom_line()+theme_classic()+labs(x="Time", y="Log weight")+facet_grid(Region~Stratum)

ggplot(logWtFall, aes(x=Year,y=value, color=Species))+geom_line()+theme_classic()+labs(x="Time", y="Log weight")+facet_grid(Region~Stratum)

# Scallops ----------------------------------------------------------------

scallopsTidy <- catchTidy %>% filter(Species=="scallop")

# ggplot(scallopsTidy, aes(x=Year,y=value, color=Season))+geom_line()+theme_classic()+labs(x="Time", y="Log catch")+facet_grid(Region~Stratum)

#Set up functions
# numRegions = 5
# numStrata = 4



# log catch ---------------------------------------------------------------

############## Fall
scalLogCatchFall <- scallopsTidy %>% 
  filter(Type=="logCatch", Season=="Fall") %>% 
  select(-c('Species', 'Type'))

scalLogCatchFall_1.1 <- scalLogCatchFall %>% 
  filter(Region==1, Stratum==1) %>% 
  ungroup() %>% 
  select(Year, value)


E1.1<- EmbedDimension(dataFrame = scalLogCatchFall_1.1, lib = "1 23", pred = "1 23", columns = "value",target = "value")

findE_v <- function(v) {
  lib_vec <- paste(1, length(v))
  indices <- c(1:length(v))
  df <- data.frame(indices,v)
  colnames(df)<-c("index", "value")
  #print(df)
  rho_E<- EmbedDimension(dataFrame = df, lib = lib_vec, pred = lib_vec, columns = "value",target = "value", maxE = 6)
  E_out<-rho_E[which.max(rho_E$rho),"E"][1]
  return(E_out)
}

findErho_v <- function(v) {
  lib_vec <- paste(1, length(v))
  indices <- c(1:length(v))
  df <- data.frame(indices,v)
  colnames(df)<-c("index", "value")
  #print(df)
  rho_E<- EmbedDimension(dataFrame = df, lib = lib_vec, pred = lib_vec, columns = "value",target = "value", maxE = 6)
  rho_out<-rho_E[which.max(rho_E$rho),"rho"][1]
  return(rho_out)
}
findEtest_v<- findE_v(scalLogCatchFall_1.1$value)
findErhotest_v<- findErho_v(scalLogCatchFall_1.1$value)

findScallopE <- function(df, season, type) {
  df_out <- df %>% 
    filter(Type == type, Season == season) %>% 
    group_by(Region, Stratum) %>% 
    select(Year, value) %>%
    summarise(E_opt = findE_v(value)) %>%
    pivot_wider(names_from = Stratum, values_from = E_opt) %>% 
    ungroup() %>% 
    select(-Region)
  return(df_out)
}

findScallopErho_v <- function(df, season, type) {
  df_out <- df %>% 
    filter(Type == type, Season == season) %>% 
    group_by(Region, Stratum) %>% 
    select(Year, value) %>%
    summarise(E_opt_rho = findErho_v(value)) %>%
    pivot_wider(names_from = Stratum, values_from = E_opt_rho) %>% 
    ungroup() %>% 
    select(-Region)
  return(df_out)
}

par(mfrow=c(5,4), mar=c(0.6,1,0.4,0.4))
findScallopE(scallopsTidy, season="Fall", type="logCatch")


a<-findScallopE(scallopsTidy, season="Fall", type="logWt")


findScallopErho_v(scallopsTidy, season="Fall", type="logCatch")


findScallopErho_v(scallopsTidy, season="Fall", type="logWt")


############## Spring
par(mfrow=c(5,4), mar=c(0.6,1,0.4,0.5))
findScallopE(scallopsTidy, season="Spring", type="logCatch")

par(mfrow=c(5,4), mar=c(0.6,1,0.4,0.5))
findSpeciesErho(scallopsTidy, season="Spring", type="logCatch")



# log weight --------------------------------------------------------------

# ggplot(scalLogWtFall, aes(x=Year, y=logWeight, group = Region, color=Region))+geom_line()+facet_grid(~Stratum)
# ggplot(scalLogWtSpring, aes(x=Year, y=logWeight, group = Region, color=Region))+geom_line()+facet_grid(~Stratum)
# ggplot(scalLogCatchSpring, aes(x=Year, y=logCatch, group = Region, color=Region))+geom_line()+facet_grid(~Stratum)
# ggplot(scalLogCatchFall, aes(x=Year, y=logCatch, group = Region, color=Region))+geom_line()+facet_grid(~Stratum)






