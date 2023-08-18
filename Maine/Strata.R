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
library(mgcv)
## With year ---------------------------------------------------------------
# 
# lm1 <- lm(log(avgCatch+1) ~ Year + Region + Stratum + Season + temp, data=j_cat_area %>% na.omit())
# lm2 <-lm(log(avgCatch+1) ~ Year + Region * Stratum, data=j_cat_area %>% na.omit())
# lm3 <-lm(log(avgCatch+1) ~ Year + Region + Stratum^2, data=j_cat_area %>% na.omit())

#lme1 <- lme(log(avgCatch+1) ~ Year + Region + Stratum, random = ~1|Season, data=j_cat_area %>% na.omit())

#gam1 <- gam(avgCatch ~ s(Year) + s(Region, k=5) + s(Stratum, k=4), data=j_cat_area %>% na.omit())
#gam2 <- gam(avgCatch ~ s(Year) + s(Region, k=5) + Stratum, data=j_cat_area %>% na.omit())
#gam4 <- gam(avgCatch ~ s(Year) + s(Region, k=5) + s(Stratum, k=4), family=tw(), data=j_cat_area %>% na.omit())

gam1 <- gam(log(avgCatch+1) ~ s(Year) + s(Region, k=5) + s(Stratum, k=4), 
            data=j_cat_area %>% na.omit())
gam2 <- gam(log(avgCatch+1) ~ s(Year, k=23) + Region + poly(Stratum, 2, raw = TRUE),
            data=j_cat_area %>% na.omit())
gam3 <- gam(log(avgCatch+1) ~ s(Year) + Region + s(Stratum, k=4), 
            data=j_cat_area %>% na.omit())
gam4 <- gam(log(avgCatch+1) ~ s(Year) + ti(Region,k=5) + ti(Stratum,k=4) + ti(Region, Stratum,k=4), 
            data=j_cat_area %>% na.omit())

AIC(gam1, gam2, gam3,gam4)
par(mfrow=c(2,2))
plot(gam4)
acf(resid(gam4))

# gam6 <- gam(log(avgCatch+1) ~ s(Year,k=23, bs="gp") + te(Region, Stratum, bs="mrf"), data=j_cat_area %>% na.omit())

gamm1 <- gamm(avgCatch ~ s(Year) + s(Region, k=5) + s(Stratum, k=4), 
              correlation=corAR1(form = ~Year|Region/Stratum/Season), data=j_cat_area %>% na.omit())

gamm2 <- gamm(log(avgCatch+1) ~ s(Year) + s(Region, k=5) + poly(Stratum, 2, raw = TRUE), correlation=corAR1(form = ~Year|Region/Stratum/Season), data=j_cat_area %>% na.omit())

w <- 1/(j_cat_area %>% na.omit() %>% pull(Region))
w2 <- 1/(j_cat_area %>% na.omit() %>% pull(Stratum))
w3 <- 1/(j_cat_area %>% na.omit() %>% mutate(w = Region*Stratum) %>% pull(w))
w4 <- 1/(j_cat_area %>% na.omit() %>% mutate(w = (Region^2)) %>% pull(w))

gamm3 <- gamm(log(avgCatch+1) ~ s(Year) + s(Region, k=5) + poly(Stratum, 2, raw = TRUE),
              correlation=corAR1(), weights=w4, data=j_cat_area %>% na.omit())

gamm4 <- gamm(log(avgCatch+1) ~ s(Year) + s(Region, k=5) + poly(Stratum, 2, raw = TRUE),
              correlation=corAR1(), data=j_cat_area %>% filter(Year > 2002) %>% na.omit())

gls1 <- gls(log(avgCatch+1) ~ Year + Region + poly(Stratum, 2, raw = TRUE), correlation=corAR1(form = ~Year|Region/Stratum/Season), data=j_cat_area %>% na.omit())

gls2 <- gls(log(avgCatch+1) ~ Year + Region + poly(Stratum, 2, raw = TRUE), correlation=corAR1(), data=j_cat_area %>% na.omit())

gls3 <- gls(log(avgCatch+1) ~ Year + Region + poly(Stratum, 2, raw = TRUE), correlation=corARMA(p=3), data=j_cat_area %>% na.omit())

plot(gls3)
hist(resid(gls3, type="normalized"))
anova(gls1, gls2, gls3)

## No year ---------------------------------------------------------------
lm4 <- lm(log(avgCatch) ~ Region +Stratum, data=j_c_c_s)
lm5 <- lm(avgCatch ~ Region +  poly(Stratum, 2, raw = TRUE), data=j_c_c_s)
lm6 <- lm(log(avgCatch) ~ Region +  poly(Stratum, 2, raw = TRUE), data=j_c_c_s %>% arrange(Stratum, Region))

bptest(lm5)
durbinWatsonTest(lm5, max.lag = 10)
lm5_results <- coeftest(lm5, vcov = vcovHAC(lm5))
lm5_results

lm5b <- lm(avgCatch ~ Region +  poly(Stratum, 2, raw = TRUE), data=sample_frac(j_c_c_s %>% ungroup(), 1L))
lm5b_results <- coeftest(lm5b, vcov = vcovHAC(lm5b))
lm5b_results

lm6_results <- coeftest(lm6, vcov = vcovHAC(lm6))
lm6_results

lm6b <- lm(log(avgCatch) ~ Region +  poly(Stratum, 2, raw = TRUE), data=j_c_c_s %>% arrange(Region, Stratum))
lm6b_results <- coeftest(lm6b, vcov = vcovHAC(lm6b))
lm6b_results

lm6c <- lm(log(avgCatch) ~ Region +  poly(Stratum, 2, raw = TRUE), data=sample_frac(j_c_c_s, 1L))
plot(lm5)

summary(lm4)$adj.r.squared
summary(lm5)$adj.r.squared
summary(lm6)$adj.r.squared
AIC(lm4, lm5, lm6, gam4)

gam4 <- gam(log(avgCatch) ~ Region + s(Stratum, k=4), data=j_c_c_s)
gam5 <- gam(avgCatch ~ Region + s(Stratum, k=4), data=j_c_c_s)
gam6 <- gam(avgCatch ~ s(Region,k=5) + s(Stratum, k=4), data=j_c_c_s)
gam6b <- gam(avgCatch ~ s(Region,k=5) + s(Stratum, k=4), data=j_c_c_s)

# gls3<-gls(log(avgCatch) ~ Region +  poly(Stratum, 2, raw = TRUE), data=j_c_c_s) #autocorrelated residuals
# gls4<-gls(log(avgCatch) ~ Region +  poly(Stratum, 2, raw = TRUE), correlation = corARMA(p=2), data=j_c_c_s)
# gls5<-gls(log(avgCatch) ~ Region +  I(Stratum^2), correlation = corARMA(p=2), data=j_c_c_s)
# 
# gls6<-gls(log(avgCatch) ~ Region +   poly(Stratum, 2, raw = TRUE), correlation = corAR1(form = ~Region|Stratum), data=j_c_c_s)
# summary(gls6)

ggplot(data=j_c_c_s)+geom_boxplot(aes(x=Region, y=avgCatch, group=Region))
ggplot(data=j_c_c_s)+geom_boxplot(aes(x=Stratum, y=avgCatch, group=Stratum))
ggplot(data=j_cat_area)+geom_boxplot(aes(x=Stratum, y=avgCatch, group=Stratum))+facet_wrap(~Region)

gamm5 <- gamm(log(avgCatch) ~ Region + s(Stratum, k=4) , correlation=corARMA(p=2),data=j_c_c_s)
gamm6 <- gamm(avgCatch ~ Region + s(Stratum, k=4) , correlation=corARMA(p=2),data=j_c_c_s)
gamm7 <- gamm(avgCatch ~ Region + s(Stratum, k=4) , correlation=corGaus(form=~ Region + Stratum),
              data=j_c_c_s %>% ungroup())





### Poisson -----------------------------------------------------------------

j <- j_cat_clean_seasons %>%
  filter(!(Region==4 & Stratum==2 & Year==2017 & Tow_Number==90)) %>% #errant february
  mutate(Season = as.factor(Season), Date = as.numeric(Date),
                                    lat = Start_Latitude, lon=Start_Longitude) %>% 
  filter(abs(Start_Depth_fathoms - End_Depth_fathoms) < 100) %>% 
  filter(Start_Depth_fathoms < 100)

100*sum(j$Number_Caught == 0)/nrow(j)
100*sum(j$Expanded_Catch == 0)/nrow(j)
100*sum(j$Expanded_Weight_kg == 0)/nrow(j)

glm1 <- glm(Number_Caught ~ Date + Region + Stratum, data=j, family="poisson")

 
# gamm(Number_Caught ~ s(Date) + s(Start_Latitude, Start_Longitude, bs = 'gp', k = 100, m = 2), family="poisson",data = j)

gam5 <- gam(Number_Caught ~ s(Date) + Region + s(Stratum, k=4), data=j, family=ziP())

gam6 <- gam(Number_Caught ~ s(Date) + Region + s(Stratum, k=4), data=j, family="poisson")
summary(gam6)
gam.check(gam6)
plot(gam6$fitted.values, resid(gam6))

# NW_VCOV <- NeweyWest(lm(log(avgCatch) ~ Region + Stratum, data=j_c_c_s), 
#                      prewhite = F, 
#                      adjust = T)
# acf(lm(log(avgCatch) ~ Region +Stratum, data=j_c_c_s)$residuals)
# shapiro.test(lm1$residuals)
# durbinWatsonTest(lm1)
# coeftest(lm1, vcov = NW_VCOV)

gamm_spat = gam(
   Expanded_Catch ~ s(Date, k=150) + Region + s(Start_Depth_fathoms), 
   data = j, family=tw())

gamm_spat2 = gamm(
  log(Expanded_Catch+1) ~ s(Date) + s(lat) + s(Start_Depth_fathoms), 
  correlation = corAR1(),
  family=Tweedie(p=1.01),
  data = j)

gamm_spat3 = gamm(
  log(Expanded_Weight_kg+1) ~ s(Date) + s(lat) + s(Start_Depth_fathoms), 
  correlation = corAR1(),
  family=Tweedie(p=1.01),
  data = j)

j_cat_area <- j_cat_area %>% mutate(Area = as.factor(paste(Region, Stratum, sep=" ")))

gam_mrf = gamm(
  # define MRF smooth
  log(avgCatch+1) ~ s(Year) + s(temp) + s(Area, bs = 'mrf', xt = list(nb = nb)),
  data   = j_cat_area %>% na.omit() %>% filter(Year > 2003),
 correlation = corAR1(form = ~Year|Season/Area),
 method="REML",
family=Tweedie(p=1.01)
)

gam_mrf2 = gamm(
  # define MRF smooth
  log(avgCatch+1) ~ s(Year, bs="cr") + s(temp, bs="cr") + s(Area, bs = 'mrf', xt = list(nb = nb)),
  data   = j_cat_area %>% na.omit() %>% filter(Year > 2003),
 correlation = corAR1(form = ~Year|Season/Area),
  method="REML",
 family=Tweedie(p=1.01)
)

gam_tw = gamm(
  log(avgCatch+1) ~ s(Year) + s(temp) + Region + s(Stratum,k=4, bs="cr"),
  data   = j_cat_area %>% na.omit() %>% filter(Year > 2003),
  correlation = corAR1(form = ~Year|Season/Area),
  method="REML",
  family=Tweedie(p=1.01)
)

gam.check(gam_tw$gam)
summary(gam_tw$lme)
AIC(gam_tw$lme)
summary(gam_tw$gam)
plot(gam_mrf2$gam)
acf(resid(gam_mrf2$lme, type="normalized"))

# 
# j_c_c_s2 <-j_cat_clean_seasons %>%
#   group_by(Stratum, Region, Season) %>%
#   summarise(avgCatch = mean(Expanded_Catch, na.rm=TRUE),
#             avgWt = mean(Expanded_Weight_kg, na.rm=TRUE))
# 
# lm7 <- lm(log(avgCatch) ~ Region +Stratum, data=j_c_c_s2)
# lm8 <- lm(log(avgCatch) ~ Region + Season + Stratum, data=j_c_c_s2)
# lme1 <- lmer(log(avgCatch) ~ Region + (1|Season) +Stratum, data=j_c_c_s2)
# summary(lme1)
# r2(lme1)

library(MuMIn)
library(spdep)

# no temporal
# gamm8 for simple confirmation of spatial differences, minimal representation of nonlinearity with smooth term, account for spatial autocorrelation, still satisfy regression assumptions (shapiro-wilk, bptest)

#here's proof of spatial autocorrelation
j_c_c_s_geom <- j_c_c_s_geom %>% mutate(Area = as.factor(paste(Region, Stratum, sep=" ")))

nb <- poly2nb(j_c_c_s_geom, row.names = j_c_c_s_geom$Area) # queen shares point or border
attr(nb, "region.id") <- j_c_c_s_geom$Area
names(nb) = attr(nb, "region.id")
names(nb)
nbw <- nb2listw(nb, style = "W")

library(gratia)
# Global Moran's I
gmoran <- moran.test(j_c_c_s_geom$avgCatch, nbw,
                     alternative = "greater")
gmoran


gamm8 <- gamm(avgCatch ~ Region + s(Stratum, k=4, bs="cr"),
              correlation=corGaus(form=~ Region + Stratum),
              data=j_c_c_s %>% ungroup())

gamm8plot1 <- draw(gamm8$gam)+theme_bw()
plot.para <- termplot(gamm8$gam, se = TRUE, plot = FALSE)

gamm8plot2 <- ggplot(data=plot.para$Region)+
  geom_line(aes(x=x,y=y))+
 # ylim(-1,6)+
geom_ribbon(data = plot.para$Region, aes(x = x, y =y, ymin = y-se, ymax = y+se), alpha = 0.2) +
 labs(x="Region", y="Partial of region")+theme_bw()+ggtitle("Region")

gamm8plot1+gamm8plot2 + plot_layout(ncol=2)

summary(gamm8$gam)
AIC(gamm8$lme)
observed_fitted_plot(gamm8$gam)

shapiro.test(resid(gamm8$lme, type="normalized"))
Box.test(resid(gamm8$lme, type="normalized"), type="L")
bptest(gamm8$gam)

acf(resid(gamm8$lme, type="normalized"))
pacf(resid(gamm8$lme, type="normalized"))


# gls for highly linear sex differences, can still account for autocorrelation
len4 <- gls(Diff ~ Stratum + Year + Region, data = int2,
            correlation = corExp(form = ~ Region + Stratum|Year))

summary(len4)
r.squaredLR(len4)
plot(len4)
AIC(len4)

shapiro.test(residuals(len4, type="normalized"))
Box.test(residuals(len4, type="normalized"), type="L")

acf(resid(len4, type="normalized"))
pacf(residuals(len4, type="normalized"))


# EDM for forecasting/treating it as a dynamic system, insights into complexity and nonlinearity of the 
# system and spatial heterogeneity with regards to these characteristics
# aggregate EDM outperforming ARIMA (and gam?)
# low-dimensional nonlinear dynamics at several spatial scales
# "dewdrop regression" by adding other regions or strata into the library can improve predictive skill

# all guided by fishermen's knowledge and priorities


