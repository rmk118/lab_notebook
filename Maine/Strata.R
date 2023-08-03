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


# Strata analysis ---------------------------------------------------------

# summaryStrat <- function(df) {
#   df %>% group_by(Season, Year, Stratum) %>%
#     summarise(avgCatch = mean(Expanded_Catch, na.rm=TRUE),
#               avgWt = mean(Expanded_Weight_kg, na.rm=TRUE),
#               temp = mean(Bottom_WaterTemp_DegC, na.rm=TRUE)) 
# }

# #computes averages for each stratum
# j_cat_strat <- summaryStrat(j_cat_clean_seasons)
# r_cat_strat <- summaryStrat(r_cat_clean_seasons)
# s_cat_strat <- summaryStrat(s_cat_clean_seasons)
# 
# catch_strat <- s_cat_strat %>% left_join(j_cat_strat, by=c("Season", "Stratum", "Year", "temp"), suffix = c("_s", "_j"))
# 
# catch_strat <- catch_strat %>% left_join(r_cat_strat, by=c("Season", "Stratum", "Year", "temp")) %>% 
#   mutate(avgCatch_r = avgCatch,avgWt_r = avgWt, .keep="unused")
# 
# catchTidy_strat <- pivot_longer(catch_strat, 
#                                   cols = starts_with("avg")) %>% 
#   mutate(Type = case_when(
#     startsWith(name, "avgCatch_") ~"catch",
#     startsWith(name,"avgWt_") ~"wt")) %>% 
#   mutate(Species = case_when(
#     endsWith(name, "s") ~"scallop",
#     endsWith(name, "r") ~"rock",
#     endsWith(name, "j") ~"jonah"))

# catchTidy_strat <- catchTidy_strat %>% 
#   mutate(Species = as.factor(Species),Season = as.factor(Season), Stratum = as.factor(Stratum)) %>% 
#   select(-name)
# 
# catch_strat_complete <- complete(data=catch_strat %>% ungroup(), Stratum, Season, Year) %>% 
#  mutate(date=paste(Year, case_when(Season== "Fall" ~ "-11-01", Season =="Spring" ~"-05-01"), sep = ""), .before=Stratum) %>%
#    filter(as.Date(date) > as.Date("2003-05-01"))
# 
# catchTidy_strat_complete<- complete(data = catchTidy_strat %>% ungroup(),Stratum, Season, Year) %>% 
#   mutate(date=paste(Year, case_when(Season== "Fall" ~ "-11-01", Season =="Spring" ~"-05-01"), sep = ""), .before=Stratum) %>%
#   filter(as.Date(date) > as.Date("2003-05-01"))

# ggplot(data = catchTidy_strat %>% filter(Type=="catch", Species=="jonah") %>% group_by(Year, Season) %>% summarise(value = mean(value, na.rm=TRUE)))+geom_line(aes(x=Year, y=value))+facet_wrap(~Season)+theme_classic()+labs(y="Abundance (catch/tow)")

# ggplot(data = catchTidy_strat_complete %>% 
#          filter(Type == "catch", Species != "scallop") %>% 
#          group_by(date, Species) %>% 
#          summarise(avg = mean(value, na.rm = TRUE)), aes(x=as.Date(date), y=avg))+
#   geom_line()+
#   facet_wrap(~Species)+
#   labs(y="catch")+
#   theme_classic()

# ggplot(data = catchTidy_strat_complete %>% 
#          filter(Type == "catch", Species != "scallop") %>% 
#          group_by(Year, Season, Species) %>% 
#          summarise(avg = mean(value, na.rm = TRUE)), aes(x=Year, y=avg))+
#   geom_line()+
#   facet_grid(Season~Species)+
#   labs(y="catch")+
#   theme_classic()

# regionsGrid_orig <- surveyGrid %>% group_by(Stratum) %>% summarise(num = n_distinct(GridID))
# regionsGrid <- left_join(regionsGrid_orig %>% mutate(Stratum = as.factor(Stratum)), catchTidy_strat_complete %>% filter(Type=="catch",) %>% group_by(Stratum, date, Species))
# 
# ggplot(data=regionsGrid %>% filter(Species != "scallop") %>% group_by(Species, Stratum) %>% summarise(avg = mean(value, na.rm=TRUE)))+
#   geom_sf(aes(fill=avg))+facet_wrap(~Species)#+scale_fill_viridis_c()
# 
# regionsGrid_seasons <- left_join(regionsGrid_orig %>% mutate(Stratum = as.factor(Stratum)), catchTidy_strat_complete %>% filter(Type=="catch"))
# 
# ggplot(data=regionsGrid_seasons %>% group_by(Stratum, Species, Season) %>% summarize(avg = mean(value, na.rm=TRUE)))+geom_sf(aes(fill=avg))+facet_grid(Season~Species)+scale_fill_viridis_c(option = "F", name="avg catch")
# 
# lag2 <- function(x) {
#   x_lagged <- (x - lag(x, 2))
#   return(x_lagged)
# } 
# catch_complete_diff <- catch_strat_complete %>% arrange(date) %>% group_by(Stratum) %>%
#   mutate(across(where(is.double) & !date, lag2)) %>% 
#   filter(date != "2003-11-01" & date != "2004-05-01") %>%  filter(as.Date(date) < as.Date("2020-05-01"))
# 
# complete_tidy_diff <- pivot_longer(catch_complete_diff,cols =starts_with("avg")) %>% 
#   mutate(Type = case_when(
#     startsWith(name, "avgCatch_") ~"catch",
#     startsWith(name,"avgWt_") ~"wt")) %>% 
#   mutate(Species = case_when(
#     endsWith(name, "s") ~"scallop",
#     endsWith(name, "r") ~"rock",
#     endsWith(name, "j") ~"jonah")) %>%
#   mutate(Species = as.factor(Species),
#          Type = as.factor(Type),
#          Stratum = as.factor(Stratum)) %>% 
#   select(-name)  

# #All areas on one graph, split by species
# ggplot(data = complete_tidy_diff %>% 
#          filter(Type == "catch", Species != "scallop"), aes(x=date, y=value, group=Stratum,color=Stratum))+geom_line()+
#   facet_wrap(~Species) +
#   labs(y="2nd-differenced catch", x="Year")
# 
# #Colored by species, split by area
# ggplot(data = complete_tidy_diff %>% filter(Type == "catch", Species != "scallop"), 
#        aes(x=date, y=value, group=Species,color=Species))+
#   geom_line()+
#   facet_wrap(~Stratum)+
#   labs(x="Depth stratum")+
#   theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))
# 
# params_ccm_combos <- data.frame(jonah=c("jonah", "jonah"), prey=c("scallop", "rock"))
# 
# catch_ccm <- catch_complete_diff %>% select(-c(Season, avgWt_s, avgWt_j, avgWt_r)) %>% rename("jonah" = "avgCatch_j", "rock"="avgCatch_r", "scallop"="avgCatch_s")
# 
# 
# v_keep_col <- c("E","Tp","num_pred", "rho", "mae","rmse","perc","p_val","rho_linear", "mae_linear",
#                 "rmse_linear","perc_linear","p_val_linear")
# 
# # Strata as replicates
# RESULTS_ccm_rep_strata <- pmap_dfr(params_ccm_combos,function(jonah,prey){
#   
#   out_1 <- do_xmap_ID(catch_ccm,predictor=jonah,target=prey,ID_col="Stratum",E=3, tp=1)  %>%
#     mutate(direction= paste("jonah","->",prey))
#   
#   out_2 <- do_xmap_ID(catch_ccm,predictor=prey,target=jonah,ID_col="Stratum",E=3, tp=1) %>% 
#     mutate(direction= paste(prey,"->","jonah"))
#   
#   bind_rows(out_1,out_2) %>% select(direction, all_of(v_keep_col))  %>% mutate(type="catch", replicate = "stratum")
# })
# 
# do_xmap_ID(catch_ccm,predictor="jonah",target="temp",ID_col="Stratum",E=3, tp=1) %>% select(rho)
# do_xmap_ID(catch_ccm,predictor="temp",target="jonah",ID_col="Stratum",E=3, tp=1) %>% select(rho)
# 
# 
# #WEIGHT
# strataDf_wt<- wtCCMdf %>% ungroup() %>% 
#   group_by(Stratum, date) %>% 
#   summarise(across(scallop:jonah, ~ mean(.x, na.rm = TRUE)))
# 
# # Strata as replicates - weight
# RESULTS_ccm_wt_combos_strata <- pmap_dfr(params_ccm_combos,function(jonah,prey){
#   
#   out_1 <- do_xmap_ID(strataDf_wt,predictor=jonah,target=prey,ID_col="Stratum",E=2, tp=1)  %>%
#     mutate(direction= paste("jonah","->",prey))
#   
#   out_2 <- do_xmap_ID(strataDf_wt,predictor=prey,target=jonah,ID_col="Stratum",E=2, tp=1) %>% 
#     mutate(direction= paste(prey,"->","jonah"))
#   
#   bind_rows(out_1,out_2) %>% select(direction, all_of(v_keep_col)) %>% mutate(type="wt", replicate = "stratum") })


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




# EDM vs. ARIMA (na.spline) -----------------------------------------------

#ts4<- na.spline(ts(catchTidy_agg_complete2 %>% filter(Species=="jonah", Type=="catch") %>% 
                  #   mutate(date=as.Date(date)) %>% arrange(date), frequency = 2, start=c(2001, 1)))

#ts_val4 <- ts4[,c("Year", "value", "date")]
#tsdf4 <- data.frame(ts_val4)
# ggplot(data=tsdf4)+geom_line(aes(x=date, y=value))
# 
# fit_train4 <- auto.arima(ts_val4[train,"value"])
# checkresiduals(fit_train4)
# 
# forecast(fit_train4, h=14)
# arima_preds4 <- data.frame(forecast(fit_train4, h=14)) %>% mutate(yrs = yrs)
# 
#EmbedDimension(dataFrame=tsdf4, columns="value", target="value", lib = "1 30", pred="31 44") #%>% filter(rho == max(rho))
#PredictNonlinear(dataFrame=tsdf4, columns="value", target="value", lib = "1 30", pred="31 44", E=8)
# PredictNonlinear(dataFrame=tsdf4, columns="value", target="value", lib = "1 30", pred="31 44", E=3, 
#                  theta="0.85 0.9 0.95 1 1.05 1.1") %>% filter(rho == max(rho))
# 
# s_out4 <- SMap(dataFrame=tsdf4, columns="value", target="value", lib = "1 30", pred="31 44", E=3, theta=1.05)
# 
# compute_stats(s_out4$predictions$Observations, s_out4$predictions$Predictions)
# plot(x=s_out4$predictions$Observations, y=s_out4$predictions$Predictions)
# 
# (tsdf4[31:44, "value"] == s_out4$predictions$Observations[1:14]) #sanity check
# 
# compute_stats(s_out4$predictions$Observations[1:14], arima_preds4$Point.Forecast)
# compute_stats(s_out4$predictions$Observations, s_out4$predictions$Predictions)
# 
# arimaPlot_train4 <- autoplot(forecast(fit_train4, h=14)) + 
#   geom_path(data = data.frame(ts_val), aes(x = c(1:44), y = value)) + 
#   ylab("Catch")
# arimaPlot_train4
# 
# edm_df4 <- data.frame(obs = s_out4$predictions$Observations[1:14], 
#                       preds = s_out4$predictions$Predictions[2:15], 
#                       pred_var = s_out4$predictions$Pred_Variance[2:15])
# 
# yrs <- seq(2016, 2022.5, 0.5)
# ind <- index(ts_val)
# 
# edm_df4 <- edm_df4 %>% mutate(Lo.95 = preds - 1.96*sqrt(pred_var),
#                             Hi.95 = preds + 1.96*sqrt(pred_var),
#                             Lo.80 = preds - 1.28*sqrt(pred_var),
#                             Hi.80 = preds + 1.28*sqrt(pred_var),
#                             yrs = yrs)
# 
# edmPlot_manual4 <- ggplot()+
#   geom_path(data = data.frame(ts_val4), aes(x = ind, y = value)) + 
#   geom_path(data = edm_df4, aes(x=yrs, y=preds,color="line"))+
#   geom_ribbon(data = edm_df4, aes(x = yrs, y =preds, ymin = Lo.95, ymax = Hi.95), fill = "blue", alpha = 0.2) +
#   labs(x="Year", y="Avg. catch")+
#   scale_color_manual(values=c("blue"))+ylim(c(-8, 25))+theme_classic()
# edmPlot_manual4
# 
# arimaPlot_manual4 <- ggplot()+
#   geom_path(data = data.frame(ts_val4), aes(x = ind, y = value)) + 
#   geom_path(data = arima_preds4, aes(x=yrs, y=Point.Forecast,color="line"))+
#   geom_ribbon(data = arima_preds4, aes(x = yrs, y = Point.Forecast, ymin = Lo.95, ymax = Hi.95), fill = "blue", alpha = 0.2) +
#   labs(x="Year", y="")+
#   scale_color_manual(values=c("blue"))+ylim(c(-8, 25))+theme_classic()
# arimaPlot_manual4
# 
# edmPlot_manual4+arimaPlot_manual4 +
#   edmPlot_manual4+arimaPlot_manual4 +
#   plot_layout(guides = 'collect')+
#   plot_annotation(tag_levels = 'A')
# 
# edm_ARIMAplot4 <-ggplot()+
#   geom_path(data = data.frame(ts_val4), aes(x = ind, y = value)) + 
#   geom_path(data = edm_df4, aes(x=yrs, y=preds, color="EDM"))+
#   geom_path(data=arima_preds4, aes(x=yrs, y=Point.Forecast, color="ARIMA"))+
#   labs(y="Avg. catch", x="Year") +theme_classic()

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

ts3 <- ts(catchTidy_agg_complete %>% filter(Species=="jonah", Type=="catch") %>% mutate(date=as.Date(date)) %>% arrange(date), frequency = 2, start=c(2001, 1))
class(ts3)
ts3
index(ts3)
ts3<- na.approx(ts3)
autoplot(ts3[,3:4])

# fit <- auto.arima(ts3[,"value"],xreg=ts3[,"temp"])

ts3[,4] %>% diff() %>% ggtsdisplay(main="")
ts3[,4] %>% ggtsdisplay(main="")

# ARIMA vs S-Map ----------------------------------------------------------
ts_val <- ts3[,c("Season", "Year", "value", "date")]

fit <- auto.arima(ts_val[,"value"])
summary(fit)
checkresiduals(fit)
arimaPlot <- autoplot(forecast(fit))

tsdf <- data.frame(ts_val)
ggplot(data=tsdf)+geom_line(aes(x=date, y=value))

theta_seq <- seq(0.01, 0.75, by=0.05)
theta_seq <- paste(theta_seq, collapse=" ")

EmbedDimension(dataFrame=tsdf, columns="value", target="value", lib = "1 44", pred="1 44")
PredictNonlinear(dataFrame=tsdf, columns="value", target="value", lib = "1 44", pred="1 44", E=3)
PredictNonlinear(dataFrame=tsdf, columns="value", target="value", lib = "1 44", pred="1 44", E=3, theta=theta_seq)
PredictNonlinear(dataFrame=tsdf, columns="value", target="value", lib = "1 44", pred="1 44", E=3, theta="0.16 0.17 0.18 0.19 0.2 0.21 0.22 0.23 0.24 0.25 0.26 0.27 0.28") %>% filter(rho == max(rho))

s_out <- SMap(dataFrame=tsdf, columns="value", target="value", lib = "1 44", pred="1 44", E=3, theta=0.23)
compute_stats(s_out$predictions$Observations, s_out$predictions$Predictions)
plot(x=s_out$predictions$Observations, y=s_out$predictions$Predictions)

train <- 1:30
test <- 31:44

fit_train <- auto.arima(ts_val[train,"value"])
summary(fit_train)
checkresiduals(fit_train)


EmbedDimension(dataFrame=tsdf, columns="value", target="value", lib = "1 30", pred="31 44") %>% filter(rho == max(rho))
PredictNonlinear(dataFrame=tsdf, columns="value", target="value", lib = "1 30", pred="31 44", E=4)
PredictNonlinear(dataFrame=tsdf, columns="value", target="value", lib = "1 30", pred="31 44", E=4, theta="0.3 0.4 0.5 0.55 0.6 0.65 0.7") %>% filter(rho == max(rho))

s_out <- SMap(dataFrame=tsdf, columns="value", target="value", lib = "1 30", pred="31 44", E=3, theta=0.6)

plot(x=s_out$predictions$Observations, y=s_out$predictions$Predictions)

(tsdf[31:44, "value"] == s_out$predictions$Observations[1:14]) #sanity check


yrs <- seq(2016, 2022.5, 0.5)
ind <- index(ts_val)

forecast(fit_train, h=14)
arima_preds <- data.frame(forecast(fit_train, h=14)) %>% mutate(yrs = yrs)

compute_stats(s_out$predictions$Observations[1:14], arima_preds$Point.Forecast)
compute_stats(s_out$predictions$Observations, s_out$predictions$Predictions)

arimaPlot_train <- autoplot(forecast(fit_train, h=14)) + 
  geom_path(data = data.frame(ts_val), aes(x = c(1:44), y = value)) + 
  ylab("Catch")
arimaPlot_train

edm_df <- data.frame(obs = s_out$predictions$Observations[1:14], preds = s_out$predictions$Predictions[2:15], pred_var = s_out$predictions$Pred_Variance[2:15])

arimaPlot_manual <- ggplot()+
  geom_path(data = data.frame(ts_val), aes(x = ind, y = value)) + 
  geom_path(data = arima_preds, aes(x=yrs, y=Point.Forecast,color="line"))+
  geom_ribbon(data = arima_preds, aes(x = yrs, y = Point.Forecast, ymin = Lo.95, ymax = Hi.95), fill = "blue", alpha = 0.2) +
  geom_ribbon(data = arima_preds, aes(x = yrs, y = Point.Forecast, ymin = Lo.80, ymax = Hi.80), fill = "blue", alpha = 0.4) +
  ylab("Catch")+scale_color_manual(values=c("blue"))
arimaPlot_manual