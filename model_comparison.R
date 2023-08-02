# Comparing EDM models to linear models
# Example: Jonah crab abundance in the Gulf of Maine
# Ruby Krasnow
# Last modified: Aug 2, 2023

#Load packages
library(tidyverse)
library(lubridate) #date formatting
library(patchwork) #combining plots
library(rEDM) #EDM
library(tseries)
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
    select(-c("Stratum", "Subsample_Weight_kg", "Subsample_Weight_kg_2", "Male_Wt_kg", "Female_Wt_kg","Date", "Surface_WaterTemp_DegC", "Surface_Salinity", "End_Latitude","End_Longitude", "Air_Temp", "Tow_Time")) %>%
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
  select(-name) %>%
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

# First fit the EDM model

EmbedDimension(dataFrame=tsdf, columns="value", target="value", lib = "1 30", pred="31 44")

PredictNonlinear(dataFrame=tsdf, columns="value", target="value", lib = "1 30", pred="31 44", E=3) #ballpark
PredictNonlinear(dataFrame=tsdf, columns="value", target="value", lib = "1 30", pred="31 44", E=3, #slightly more specific
                 theta="0.85 0.9 0.95 1 1.05 1.1") %>% filter(rho == max(rho))
 
s_out <- SMap(dataFrame=tsdf, columns="value", target="value", lib = "1 30", pred="31 44", E=3, theta=1.05)

# Now ARIMA
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

(tsdf[31:44, "value"] == s_out$predictions$Observations[1:14]) #sanity check


# Now let's plot
edm_df <- data.frame(obs = s_out$predictions$Observations[1:14], 
                     preds = s_out$predictions$Predictions[2:15], 
                     pred_var = s_out$predictions$Pred_Variance[2:15])

edm_df <- edm_df %>% mutate(Lo.95 = preds - 1.96*sqrt(pred_var),
                            Hi.95 = preds + 1.96*sqrt(pred_var),
                            Lo.80 = preds - 1.28*sqrt(pred_var),
                            Hi.80 = preds + 1.28*sqrt(pred_var),
                            yrs = yrs)

edmPlot_manual <- ggplot()+
  geom_path(data = data.frame(ts_val), aes(x = index(ts_val), y = value)) +
  geom_path(data = edm_df, aes(x=yrs, y=preds,color="line"))+
  geom_ribbon(data = edm_df, aes(x = yrs, y =preds, ymin = Lo.95, ymax = Hi.95), fill = "blue", alpha = 0.2) +
  labs(x="Year", y="Avg. catch")+
  scale_color_manual(values=c("blue"))+ylim(c(-8, 25))+theme_classic()
edmPlot_manual

arimaPlot_manual <- ggplot()+
  geom_path(data = data.frame(ts_val), aes(x = index(ts_val), y = value)) +
  geom_path(data = arima_preds, aes(x=yrs, y=Point.Forecast,color="line"))+
  geom_ribbon(data = arima_preds, aes(x = yrs, y = Point.Forecast, ymin = Lo.95, ymax = Hi.95), fill = "blue", alpha = 0.2) +
  labs(x="Year", y="")+
  scale_color_manual(values=c("blue"))+ylim(c(-8, 25))+theme_classic()
arimaPlot_manual

edmPlot_manual+arimaPlot_manual +
   plot_layout(guides = 'collect')+
   plot_annotation(tag_levels = 'A')


both_noCI <-ggplot()+
  geom_path(data = data.frame(ts_val), aes(x = index(ts_val), y = value)) +
  geom_path(data = edm_df, aes(x=yrs, y=preds, color="EDM"))+
  geom_path(data=arima_preds, aes(x=yrs, y=Point.Forecast, color="ARIMA"))+
  labs(y="Avg. catch", x="Year")

both_noCI +theme_classic()