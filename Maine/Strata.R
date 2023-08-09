# Strata analysis and GAMs/linear models
# Last modified: Aug 4, 2023

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

# Regions -----------------------------------------------------------------
summaryReg <- function(df) {
  df %>% group_by(Season, Year, Region) %>%
    summarise(avgCatch = mean(Expanded_Catch, na.rm=TRUE),
              avgWt = mean(Expanded_Weight_kg, na.rm=TRUE),
              temp = mean(Bottom_WaterTemp_DegC, na.rm=TRUE))
}

#computes averages for each stratum
j_cat_reg <- summaryReg(j_cat_clean_seasons) %>% ungroup() %>% complete(Season, Year, Region)
r_cat_reg <- summaryReg(r_cat_clean_seasons) %>% ungroup() %>% complete(Season, Year, Region)
s_cat_reg <- summaryReg(s_cat_clean_seasons) %>% ungroup() %>% complete(Season, Year, Region)

catch_reg <- s_cat_reg %>% left_join(j_cat_reg, by=c("Season", "Region", "Year", "temp"), suffix = c("_s", "_j"))

catch_reg <- catch_reg %>% left_join(r_cat_reg, by=c("Season", "Region", "Year", "temp")) %>%
  mutate(avgCatch_r = avgCatch,avgWt_r = avgWt, .keep="unused")

catchTidy_reg <- pivot_longer(catch_reg,
                              cols = starts_with("avg")) %>%
  mutate(Type = case_when(
    startsWith(name, "avgCatch_") ~"catch",
    startsWith(name,"avgWt_") ~"wt")) %>%
  mutate(Species = case_when(
    endsWith(name, "s") ~"scallop",
    endsWith(name, "r") ~"rock",
    endsWith(name, "j") ~"jonah"))

catchTidy_reg <- catchTidy_reg %>%
  mutate(Species = as.factor(Species),Season = as.factor(Season), Region = as.factor(Region)) %>%
  select(-name)

catch_reg_complete <- complete(data=catch_reg %>% ungroup(), Region, Season, Year) %>% 
  mutate(date=paste(Year, case_when(Season== "Fall" ~ "-11-01", Season =="Spring" ~"-05-01"), sep = ""), .before=Region) %>%
  filter(as.Date(date) > as.Date("2003-05-01"))

catchTidy_reg_complete<- complete(data = catchTidy_reg %>% ungroup(),Region, Season, Year) %>% 
  mutate(date=paste(Year, case_when(Season== "Fall" ~ "-11-01", Season =="Spring" ~"-05-01"), sep = ""), .before=Region) %>%
  filter(as.Date(date) > as.Date("2003-05-01")) %>% ungroup()

catchTidy_reg_complete_j1 <- catchTidy_reg_complete %>% 
  filter(Species =="jonah") %>% 
  arrange(Region, date) %>% 
  group_by(Season, Region, Type) %>% 
  mutate(temp = na.spline(temp),value = na.spline(value)) %>% 
  ungroup() 

par(mfrow=c(2,3))
findSpeciesGroups_both(catchTidy_reg_complete_j1, type="catch", g="Region", species="jonah")


# Catch model testing -----------------------------------------------------

#j_cat_area is from model_comparison.R

lm1 <- lm(log(avgCatch+1) ~ Year + Region + Stratum + Season + temp, data=j_cat_area %>% na.omit())
summary(lm1)
par(mfrow=c(2,2))
plot(lm1)
AIC(lm1)

lm2 <-lm(log(avgCatch+1) ~ Year + Region * Stratum, data=j_cat_area %>% na.omit())
lm3 <-lm(log(avgCatch+1) ~ Year + Region + Stratum^2, data=j_cat_area %>% na.omit())
lme1 <- lme(log(avgCatch+1) ~ Year + Region + Stratum, random = ~1|Season, data=j_cat_area %>% na.omit())


gam1 <- gam(avgCatch ~ s(Year) + s(Region, k=5) + s(Stratum, k=4), data=j_cat_area %>% na.omit())
gam2 <- gam(avgCatch ~ s(Year) + s(Region, k=5) + Stratum, data=j_cat_area %>% na.omit())
gam3 <- gam(log(avgCatch+1) ~ s(Year) + s(Region, k=5) + s(Stratum, k=4), data=j_cat_area %>% na.omit())
gam4 <- gam(avgCatch ~ s(Year) + s(Region, k=5) + s(Stratum, k=4), family=tw(), data=j_cat_area %>% na.omit())
gam5 <- gam(log(avgCatch+1) ~ s(Year) + s(Region, k=5) + s(Stratum, k=4), family=tw(), data=j_cat_area %>% na.omit())
gam6 <- gam(log(avgCatch+1) ~ s(Year, k=23) + Region + poly(Stratum, 2, raw = TRUE),data=j_cat_area %>% na.omit())
gam7 <- gam(avgCatch ~ s(Year) + s(Region, k=5, bs="ordinal") + s(Stratum, k=4, bs="ordinal"), data=j_cat_area %>% na.omit())

summary(gam2)
AIC(gam2)
plot(gam1)
gam.check(gam6)



AIC(gam1, gam2, gam3, gam4, gam5, gam6)


summary(gam4)
AIC(gam4)
plot(gam3)
gam.check(gam4)
anova(gam1, gam2)
acf(residuals(gam4))

gam5 <- gam(log(avgCatch+1) ~ s(Year) + s(Region, k=5) + s(Stratum, k=4), family=tw(), data=j_cat_area %>% na.omit())
summary(gam5)
gam.check(gam5)

+ poly(Stratum, 2, raw = TRUE)

gamm1 <- gamm(avgCatch ~ s(Year) + s(Region, k=5) + s(Stratum, k=4), correlation=corAR1(form = ~Year|Region/Stratum/Season), data=j_cat_area %>% na.omit())
summary(gamm1$gam)

gamm2 <- gamm(log(avgCatch+1) ~ s(Year) + s(Region, k=5) + poly(Stratum, 2, raw = TRUE), correlation=corAR1(form = ~Year|Region/Stratum/Season), data=j_cat_area %>% na.omit())
summary(gamm2$gam)

acf(residuals(gamm2$lme))

anova(gamm1$lme, gamm2$lme)
gam.check(gamm2$gam)

gls1 <- gls(log(avgCatch+1) ~ Year + Region + poly(Stratum, 2, raw = TRUE), correlation=corAR1(form = ~Year|Region/Stratum/Season), data=j_cat_area %>% na.omit())
summary(gls3)
acf(resid(gls3, type="normalized"))

gls2 <- gls(log(avgCatch+1) ~ Year + Region + poly(Stratum, 2, raw = TRUE), correlation=corAR1(), data=j_cat_area %>% na.omit())


gls3 <- gls(log(avgCatch+1) ~ Year + Region + poly(Stratum, 2, raw = TRUE), correlation=corAR1(form = ~Year|Region/Stratum/as.factor(Season)), data=j_cat_area %>% na.omit())
plot(gls3)

AIC(gls1, gls2, gls3)
