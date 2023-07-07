#EDM help functions for analyzing the Maine inshore trawl survey data
#Ruby Krasnow
#Last modified: July 7, 2023

library(tidyverse)
library(lubridate)

########### cleanCatch --------------------------------------------------------------

df_tows<-read.csv("data/Maine_inshore_trawl/MEtows.csv") #tow data
df_s_cat<- read.csv("data/Maine_inshore_trawl/MEscallopCatch.csv") #catch data of interest

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

#implementation
s_catFull <- cleanCatch(df_s_cat) %>% 
  mutate(Common_Name = "Scallop")


########## summaryCatch ------------------------------------------------------------

summaryCatch <- function(df) {
  df %>% group_by(Season, Year, Region,Stratum) %>%
    summarise(avgCatch = mean(Expanded_Catch),
              avgWt = mean(Expanded_Weight_kg))
}

#implementation
s_cat <- summaryCatch(s_catFull)


########### findSpeciesMean ---------------------------------------------------------

#returns a tibble with the mean catch (avg across years) for each region/stratum combination

# prepare the dataframe by adding log catch and log weight columns, then making it tidy
s_cat$logS_catch <- log(s_cat$avgCatch+1)
s_cat$logS_wt <- log(s_cat$avgWt+1)

s_catchTidy <- pivot_longer(s_cat, 
                          cols = 5:ncol(s_cat)) %>% 
  mutate(Type = case_when(
    startsWith(name, "avgCatch") ~"avgCatch",
    startsWith(name,"avgWt") ~"avgWt",
    endsWith(name,"_wt") ~"logWt",
    endsWith(name,"_catch") ~"logCatch")) %>% 
  mutate(Species = "scallop")
  # mutate(Species = case_when(
  #   startsWith(name, "logS") | endsWith(name, "s") ~"scallop",
  #   startsWith(name, "logR") | endsWith(name, "r") ~"rock",
  #   startsWith(name, "logJ") | endsWith(name, "j") ~"jonah"))

s_catchTidy <- s_catchTidy %>% 
  mutate(Species = as.factor(Species),
    Season = as.factor(Season),
    Region = as.factor(Region), 
    Stratum = as.factor(Stratum)) %>% 
  select(-name)

# actual function
findSpeciesMean<- function(df, season, type) {
  df_out <- df %>% 
    filter(Type == type, Season == season) %>% 
    group_by(Region, Stratum) %>% 
    select(Year, value) %>%
    summarise(avg = mean(value)) %>%
    pivot_wider(names_from = Stratum, values_from = avg) %>% 
    ungroup() %>% 
    select(-Region)
  return(df_out)
}

#implementation
findSpeciesMean(s_catchTidy, season="Fall", type="logCatch")

############ Find E - df input -------------------------------------------------------

findE_df <- function(df) {
  lib_vec <- paste(1,nrow(df))
  rho_E<- EmbedDimension(dataFrame = df, lib = lib_vec, pred = lib_vec, columns = "value",target = "value")
  E_out<-rho_E[which.max(rho_E$rho),"E"][1]
  return(E_out)
}

#implementation example
scalLogCatchFall_1.1 <- s_catchTidy %>% 
  filter(Type=="logCatch", Season=="Fall") %>% 
  select(-c('Species', 'Type')) %>% 
  filter(Region==1, Stratum==1) %>% 
  ungroup() %>% 
  select(Year, value)

findE_df(scalLogCatchFall_1.1)

############ Find E - vector input -------------------------------------------------

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

#implementation examples
findE_v(scalLogCatchFall_1.1$value)
findErho_v(scalLogCatchFall_1.1$value)

############ findSpeciesE -------------------------------------------------------

#returns a tibble with the optimal embedding dimension for time series from each region/stratum combination
#Same idea as the findSpeciesMean function, but finding the optimal embedding dimension instead of the mean

findSpeciesE <- function(df, season, type) {
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

findSpeciesErho <- function(df, season, type) {
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

#Implementation examples
par(mfrow=c(5,4), mar=c(0.6,1,0.4,0.4))
findSpeciesE(s_catchTidy, season="Fall", type="logCatch")
findSpeciesErho(s_catchTidy, season="Fall", type="logCatch")
