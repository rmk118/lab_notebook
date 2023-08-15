#Jonah crab seasonal sex ratio differences
#Ruby Krasnow
#Last modified: Aug 8, 2023

# Load packages
library(tidyverse)
library(lubridate) #working with dates
library(sf) #for spatial data analysis/visualization

# Time series analysis
library(tseries)
library(forecast)

#Data visualization
library(patchwork)
library(ggpubr)
library(ggpmisc)

#Stats/modeling

library(car)
library(rstatix)
library(nlme)
library(broom.mixed)
library(RLRsim)
library(MASS)

# Import data
df_j_len<- read.csv("data/Maine_inshore_trawl/MEjonahLength.csv") #jonah crab length and sex data
df_tows<-read.csv("data/Maine_inshore_trawl/MEtows.csv") #tow data
surveyGrid <-st_read("~/Downloads/lab_notebook/Maine/MaineDMR_-_Inshore_Trawl_Survey_Grid") #CRS: WGS 84/EPSG 4326

surveyGrid <- surveyGrid %>% 
  mutate(Region = region_id,
         Stratum = depth_stra,
         GridID = grid_id, .keep="unused", .before=last_surve)

regionsGrid_orig <- surveyGrid %>% group_by(Stratum) %>% summarise(num = n_distinct(GridID))

df_j_len <- df_j_len %>%
  arrange(Season, Year, Stratum, Region, Tow_Number)

df_sex <- df_j_len %>% 
  filter(Year > 2003) %>% 
  dplyr::select(c("Season", "Year", "Tow_Number", "Region", "Stratum", "Frequency", "Sex")) %>% 
  arrange(Season, Year, Stratum, Region, Tow_Number)

data_complete <- df_sex %>% 
  group_by(Season, Year, Region, Stratum) %>% 
  complete(Sex, Tow_Number) #makes implicit NAs explicit by filling in all combinations of tow number and sex

data_complete <- data_complete %>% 
  group_by(Season, Year, Region, Stratum, Tow_Number, Sex) %>% 
  mutate(Num_Sex = cumsum(Frequency)) %>% #add up the number of crabs caught per tow of each sex
  slice_tail()  %>% #retrieve the total
  replace(is.na(.),0)

data_complete <- data_complete %>% filter(Sex != "Unknown") %>% 
  group_by(Season, Year, Region, Stratum, Tow_Number) %>% 
  mutate(Total = cumsum(Num_Sex)) %>% #add both sexes to find the total number of crabs caught per tow
  slice_tail() %>% 
  mutate(perc_f = ifelse(Sex=="Female", Num_Sex/Total, (1-(Num_Sex/Total))), #calculate percent female
         perc_m = ifelse(Sex == "Male", Num_Sex/Total, (1-(Num_Sex/Total)))) #calculate percent male

#find the mean proportion of female crabs for each stratum by season and year
data_strat <- data_complete %>%
  group_by(Season, Year, Stratum) %>% 
  summarise(perc_f = mean(perc_f, na.rm=TRUE)) 

data_complete_geom <- left_join(regionsGrid_orig, data_strat) #join to sf object for visualization


sex_diff <- pivot_wider(data_strat, names_from="Season", values_from = "perc_f") %>% 
  group_by(Year, Stratum) %>% 
  summarise(Fall = mean(Fall, na.rm=TRUE),
            Spring = mean(Spring, na.rm=TRUE)) %>% 
  mutate(Diff = Fall-Spring, .keep="unused") %>% #calculate the seasonal difference in perc. female from Fall to Spring
  na.omit() %>% 
  filter(Year > 2004) %>% ungroup() %>% 
  mutate(Stratum = as.numeric(Stratum))

sex_diff_geom <- left_join(regionsGrid_orig, sex_diff) #join to sf for visualization

sex_diff %>% 
  group_by(Stratum) %>% 
  identify_outliers(Diff) #check for extreme outliers, none found

sex_diff %>%
  group_by(Stratum) %>%
  shapiro_test(Diff) #no significant deviations from normality

tibble(sex_diff) #preview data

# Ordinary least squares linear regression
lm <- lm(Diff ~ Stratum, data=sex_diff)
summary(lm)
par(mfrow = c(2, 2))
plot(lm)
shapiro.test(lm$residuals) #good
durbinWatsonTest(lm, max.lag=15) #good

ggplot(data=sex_diff, aes(x=Stratum, y=Diff, group=Stratum))+geom_violin()+theme_bw()

# Figures -----------------------------------------------------------------

ggplot(data=sex_diff)+geom_line(aes(x=Year, y=Diff, group=Stratum, color=Stratum))

#across all years
#find the mean proportion of female crabs for each stratum by season
data_agg <- data_complete %>%
  group_by(Season, Stratum) %>% 
  summarise(perc_f = mean(perc_f, na.rm=TRUE)) 

sex_diff_agg <- pivot_wider(data_agg, names_from="Season", values_from = "perc_f") %>% 
  group_by(Stratum) %>% 
  summarise(Fall = mean(Fall, na.rm=TRUE),
            Spring = mean(Spring, na.rm=TRUE)) %>% 
  mutate(Diff = Fall-Spring, .keep="unused") %>% #calculate the seasonal difference in perc. female from Fall to Spring
  na.omit() %>% 
  ungroup() %>% 
  mutate(Stratum = as.numeric(Stratum))

#Straight line with r^2 = 0.97
r_squared_j <- ggplot(data=sex_diff_agg, aes(x=Stratum, y=Diff))+
  geom_point()+
  theme_bw()+
  stat_smooth(method="lm", se=FALSE)+
  stat_regline_equation(label.y = 0, aes(label = after_stat(eq.label))) +
  stat_regline_equation(label.y = 0.05, aes(label = after_stat(rr.label)))+
  ylim(-0.1, 0.3)

sex_diff_geom_agg <- left_join(regionsGrid_orig, sex_diff_agg) #join to sf for visualization

p1 <- ggplot(data=sex_diff_geom_agg)+geom_sf(aes(fill=Diff))

p2 <- ggplot(data=sex_diff_geom %>% group_by(Stratum) %>% summarise(diff = mean(Diff)))+geom_sf(aes(fill=diff))
p1+p2

# Regions -----------------------------------------------------------------

#find the mean proportion of female crabs for each stratum and region by season and year
data_area <- data_complete %>%
  group_by(Season, Year, Stratum, Region) %>% 
  summarise(perc_f = mean(perc_f, na.rm=TRUE)) 

sex_diff_reg <- pivot_wider(data_area, names_from="Season", values_from = "perc_f") %>% 
  group_by(Year, Region, Stratum) %>% 
  summarise(Fall = mean(Fall, na.rm=TRUE),
            Spring = mean(Spring, na.rm=TRUE)) %>% 
  mutate(Diff = Fall-Spring, .keep="unused") %>% #calculate the seasonal diff in perc. female, Fall-Spring
  na.omit() %>% 
  filter(Year > 2004) %>% ungroup() %>% 
  mutate(Stratum = as.numeric(Stratum))

regionsGrid_reg <- surveyGrid %>% group_by(Region, Stratum) %>% summarise(num = n_distinct(GridID))
sex_diff_geom_reg <- left_join(regionsGrid_reg, sex_diff_reg) #join to sf for visualization
ggplot(data=sex_diff_geom_reg)+geom_sf(aes(fill=Diff))

# Many outliers, one extreme
sex_diff_reg %>% 
  group_by(Region, Stratum) %>% 
  identify_outliers(Diff)

# Areas 23 and 51 are not normal
sex_diff_reg %>%
  group_by(Region, Stratum) %>%
  shapiro_test(Diff)

min_date <- df_tows %>% 
  filter(!(Region==4 & Depth_Stratum==2 & Year==2017 & Tow_Number==90))  %>% 
  group_by(Year, Season) %>% slice_min(Start_Date) %>% 
  mutate(date = ymd_hms(Start_Date), .keep="unused", .before=Season) %>% 
  mutate(stdYear = `year<-`(date, 2000), .before=Season) %>% 
  mutate(stdYear = date(stdYear))

max_date <- df_tows %>% 
  filter(!(Region==4 & Depth_Stratum==2 & Year==2017 & Tow_Number==90))  %>% 
group_by(Year, Season) %>% slice_max(Start_Date) %>% 
  mutate(date = ymd_hms(Start_Date), .keep="unused", .before=Season) %>% 
  mutate(stdYear = `year<-`(date, 2000), .before=Season) %>% 
  mutate(stdYear = date(stdYear))

ggplot(data=min_date %>% filter(Season=="Fall"))+geom_line(aes(x=Year, y=stdYear))
ggplot(data=min_date %>% filter(Season=="Spring"))+geom_line(aes(x=Year, y=stdYear))
ggplot(data=min_date)+geom_line(aes(x=Year, y=stdYear, color=Season))
ggplot(data=max_date %>% filter(Season=="Fall"))+geom_line(aes(x=Year, y=stdYear))
ggplot(data=max_date %>% filter(Season=="Spring"))+geom_line(aes(x=Year, y=stdYear))
ggplot(data=max_date)+geom_line(aes(x=Year, y=stdYear, color=Season))

min(min_date %>% filter(Season == "Fall") %>% pull(stdYear))
median(min_date %>% filter(Season == "Fall") %>% pull(stdYear))

med_date <- df_tows  %>% 
  filter(!(Region==4 & Depth_Stratum==2 & Year==2017 & Tow_Number==90)) %>% 
  mutate(date = ymd_hms(Start_Date), .keep="unused", .before=Season) %>% 
  mutate(stdYear = `year<-`(date, 2000), .before=Season) %>% 
  mutate(stdYear = date(stdYear)) %>% 
  group_by(Year, Season, Region, Depth_Stratum) %>% 
  summarise(med = median(stdYear)) %>% 
  mutate(Stratum = Depth_Stratum, .keep="unused") %>%
  ungroup() 

med_date %>% 
  group_by(Season, Year) %>% 
  summarise(med = max(med) - min(med))

int <- df_tows  %>% 
  mutate(date = ymd_hms(Start_Date), .keep="unused", .before=Season) %>% 
  mutate(stdYear = `year<-`(date, 2000), .before=Season) %>% 
  mutate(stdYear = date(stdYear)) %>% 
  group_by(Year, Region, Depth_Stratum) %>% 
  summarise(med = median(stdYear)) %>% 
  mutate(Stratum = Depth_Stratum, .keep="unused") %>%
  ungroup() #not grouped by season

int <- left_join(int, sex_diff_reg) %>% na.omit()

tot_date <- df_tows %>% 
  filter(!(Region==4 & Depth_Stratum==2 & Year==2017 & Tow_Number==90)) %>% #errant February
  mutate(date = ymd_hms(Start_Date), .keep="unused", .before=Season) %>% 
  mutate(stdYear = `year<-`(date, 2000), .before=Season) %>% 
  mutate(stdYear = date(stdYear)) %>% 
  group_by(Year, Region, Depth_Stratum) %>% 
  summarise(len = max(stdYear) - min(stdYear)) %>% 
  mutate(Stratum = Depth_Stratum, .keep="unused") %>%
  ungroup() %>% filter(Year > 2004 & Year != 2020) %>% 
  mutate(len = ifelse(len < 10, 151, len))

int2 <- left_join(tot_date, sex_diff_reg) %>% na.omit()

gls1 <- gls(Diff ~ Stratum + Year + Region, data=int2, corr=corAR1(form =  ~Year|Stratum/Region))
gls2 <- gls(Diff ~ Stratum + Region + Year, correlation = corAR1(), data = int2)
len1 <- lme(Diff ~ Stratum + Year + Region, random = ~ 1|len, data = int2, correlation = corAR1())


AIC(gls1, gls2, len1)
summary(len1)
plot(gls1)

shapiro.test(residuals(gls1, type="normalized"))
shapiro.test(residuals(len2, type="normalized"))
shapiro.test(residuals(len1, type="normalized"))

plot(gls1, resid(.) ~ Year, abline = 0, cex = 0.3)
plot(gls1, resid(.) ~ Year | Region, abline = 0, cex = 0.3)
plot(gls1, resid(.) ~ Year | Stratum, abline = 0, cex = 0.3)
plot(gls1, resid(.) ~ fitted(.) | Region, abline = 0, cex = 0.3)

Box.test(residuals(gls1), type="L")
Box.test(residuals(len1, type="normalized"), type="L")

tidy(gls1)
tidy(gls2)
tidy(len1)

par(mfrow=c(3,1), mar=c(1,1,1,1))
acf(residuals(gls1, type="normalized"))
acf(residuals(len2, type="normalized"))
acf(residuals(len1, type="normalized"))
acf(residuals(len1))

anova(gls1, gls2)
anova(gls1, gls2, len1)

plot(len1, resid(.) ~ Stratum | Region, abline = 0, cex = 0.3)
plot(len1, resid(.) ~ fitted(.) | Region, abline = 0, cex = 0.3)
plot(len1, resid(.) ~ Year, abline = 0, cex = 0.3)
plot(len1, resid(.) ~ Year | Region, abline = 0, cex = 0.3)
plot(len1, resid(.) ~ Year | Stratum, abline = 0, cex = 0.3)
plot(len1, resid(.) ~ fitted(.) | Region, abline = 0, cex = 0.3)

summary(len1)
plot(len1)

ggplot(med_date, aes(x=Year, y=med, group=Season,color=Season))+geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  facet_grid(Stratum~Region)

fig1a <- ggplot(int2, aes(x=Year-2005, y=len))+
 geom_point(size=1)+
 geom_smooth(method="gam", aes(color="GAM"))+
  stat_poly_line(method="rlm",  aes(color="RLM"))+
  stat_poly_eq(method="rlm",aes(label = paste(after_stat(eq.label))), label.y=0.3)+
  theme_classic()+
 ylim(110,170)+
  labs(x="Years since 2005", y="Time elapsed (days)")+
  scale_colour_manual(name="legend", values=c("blue", "red"))
fig1a

fig1b <- ggplot(int2 %>% group_by(Year) %>% summarise(len=mean(len)), aes(x=Year-2005, y=len))+
  geom_point(size=1)+
 geom_smooth(method="gam",  aes(color="GAM"))+
  stat_poly_line(method="rlm",  aes(color="RLM"))+
  stat_poly_eq(method="rlm",aes(label = paste(after_stat(eq.label))), label.y=0.3)+
  theme_classic()+
  ylim(110,170)+
  labs(x="Years since 2005",y="")+
  scale_colour_manual(name="legend", values=c("blue", "red"))
fig1b

(fig1a + fig1b) + 
  plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 12)) &
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)))

rlm(data = int2, len ~ Year)
rlm(data = int2 %>% mutate(yr = Year-2005), len ~ yr)

library(RLRsim)
exactRLRT(len1)


# Rejected models ---------------------------------------------------------

# library(lmtest)
# library(sandwich)
# bptest(lm) #p<0.05, will use robust standard errors
# lm_results <- coeftest(lm, vcov = vcovHC(lm, type = 'HC0'))
# lm_results
# 
# # Year is not significant when added as an explanatory variable
# lm_year <- lm(Diff ~ Stratum + Year, data=sex_diff)
# coeftest(lm_year, vcov = vcovHC(lm_year, type = 'HC0'))

### With median survey date
# i1 <- lm(Diff ~ Stratum + Year + Region + med, data = int)  #autocorr, med not sig, fails ncvTest
# i2 <- lme(Diff ~ Stratum + Region + Year, random = ~Year|med, data = int) #unneeded complexity, high AIC
# i5 <- lme(Diff ~ Stratum + Region + Year, random = ~1|med, data = int, correlation = corAR1(form =  ~1|med)) #exactly the same as corAR1()

### With length
# i1 <- lm(Diff ~ Stratum + Year + Region + len, data = int2)  #autocorr, len not sig
#d5 <- lme(Diff ~ Stratum + Region + Year, random = ~Year|len, data = int2, correlation = corAR1()) # singular
# d1 <- lme(Diff ~ Stratum + Year + Region, random = ~ 1|len, data = int2) #autocorrelation

# incompatible formulas for groups in 'random' and 'correlation'
#lme(Diff ~ Stratum + Year + Region, random = ~ 1|len, data = int2, 
#          correlation = corAR1(form =  ~Year|Stratum/Region))

##### same corr form another
# Same as form =  ~Year|Stratum/Region
# gls2b <- gls(Diff ~ Stratum + Year + Region, data=sex_diff_reg, corr=corAR1(form =  ~Year|Region/Stratum))
# Same as corAR1()
#d2 <- lme(Diff ~ Stratum + Year + Region, random = ~ 1|len, data = int2, 
         # correlation = corAR1(form =  ~1|len))

#len1_ml2 <- lme(Diff ~ Stratum + Year + Region, random = ~ 1|len, data = int2, correlation = corAR1(form = ~1|len), method = "ML")

#### Autocorrelation
#reg1<-lm(Diff ~ Stratum + Region, data = sex_diff_reg)
#durbinWatsonTest(reg1) #significant autocorrelation
#ncvTest(reg1) #also non-constant variance

#reg2<-lm(Diff ~ Stratum + Region + Year, data = sex_diff_reg)
#shapiro.test(reg2$residuals) #good
#durbinWatsonTest(reg2) #significant autocorrelation, but less than 1
#ncvTest(reg2) #also non-constant variance

#d2 <- lme(Diff ~ Stratum + Region + Year, random = ~Year|len,  data = int2) #autocorrelation
#identical(d1$fitted, d2$fitted)
#identical(d1$residuals, d2$residuals)

#i1 <- lme(Diff ~ Stratum + Region + Year, random = ~1|med, data = int)

##### Region should be a fixed effect
# reg6 <- lme(Diff ~ Stratum + Year, random = ~ 1|Region, data = sex_diff_reg, 
#             correlation = corARMA(form= ~ Year | Region/Stratum, p=1, q=1))
# gls1 <- gls(Diff ~ Stratum + Year, data=sex_diff_reg, corr=corAR1(form =  ~Year|Stratum/Region))

# reg4 <- lme(Diff ~ Stratum + Year, random = ~ 1|Region, data = sex_diff_reg)
# summary(reg4)
# plot(reg4)
# plot(reg4, resid(., type = "normalized") ~ fitted(.) | Region, abline = 0)
# plot(ACF(reg4, resType = "normalized"))
# Box.test(residuals(reg4))
# acf(residuals(reg4, type="normalized"))
# Box.test(residuals(reg4, type="normalized"))
# shapiro.test(residuals(reg4, type="normalized"))
# aug <- broom.mixed::augment(reg4)
# aug
# ggplot(aug, aes(Year, Diff)) +
#   geom_point(alpha = 0.3) +
#   facet_wrap(~Region)
# 
# plot(reg4, resid(.) ~ Year | Region, abline = 0, cex = 0.3)
# plot(reg4, resid(.) ~ fitted(.) | Region, abline = 0, cex = 0.3)
# 
# anova(reg4, reg5)


# reg5 <- lme(Diff ~ Stratum + Year, random = ~ 1|Region, data = sex_diff_reg, 
#             correlation = corAR1(form =  ~Year|Region/Stratum))
# summary(reg5)
# summary(reg5)$tTable
# fixef(reg5)
# ranef(reg5)
# plot(reg5)
# vcov(reg5)
# intervals(reg5, which="fixed")
# coef(reg5)
# plot(reg5, resid(.) ~ Stratum | Region, abline = 0, cex = 0.3)
# plot(reg5, resid(.) ~ fitted(.) | Region, abline = 0, cex = 0.3)
# AIC(reg5)
# acf(residuals(reg5, type="normalized"))
# shapiro.test(resid(reg5))
# anova(reg5)
# getCall(reg5)$correlation
# intervals(reg5, which="var-cov")$corStruct

# reg3 <- lmer(Diff ~ Stratum + (1|Region) + Year, data = sex_diff_reg)
# summary(reg3)
# shapiro.test(residuals(reg3)) #good
# acf(residuals(reg3))
# ncvTest(reg3) #also non-constant variance
# Box.test(residuals(reg3)) #
# par(mfrow = c(2, 2))
# plot(reg3)
# arm::display(reg3)

#### Duration - worse version of len
# int <- med_date %>% filter(Year > 2004 & Year != 2020) %>% 
#   group_by(Year, Region, Stratum) %>% 
#   mutate(dur = max(med) - min(med)) %>% 
#   mutate(dur = as.integer(dur)) %>% 
#   filter(Season == "Fall")
#ggplot(data=int)+geom_line(aes(x=Year,y=dur))

#Median date - length between first and last sampling days per year is better
#med1 <- lme(Diff ~ Stratum + Region + Year, random = ~1|med, data = int, correlation = corAR1())

# Fit using ML rather than REML
# gls1_ml <- gls(Diff ~ Stratum + Year + Region, data=int2, corr=corAR1(form =  ~Year|Stratum/Region), method = "ML")
# gls2_ml <- gls(Diff ~ Stratum + Region + Year, correlation = corAR1(), data = int2, method="ML")
# len1_ml <- lme(Diff ~ Stratum + Year + Region, random = ~ 1|len, data = int2, correlation = corAR1(), method = "ML")
# 
# anova(gls1_ml, gls2_ml, len1_ml)
# anova(gls1_ml)
# acf(residuals(len1_ml, type="normalized"))
# 
# tidy(gls1_ml)
# tidy(len1_ml)

# len1 <- lme(Diff ~ Stratum + Year + Region, random = ~ 1|len, data = int2, correlation = corAR1())
# summary(len1)
# plot(len1)
# shapiro.test(residuals(len1, type="normalized"))
# Box.test(residuals(len1, type="normalized"), type="L")
# acf(resid(len1, type="normalized"))
# pacf(residuals(len1, type="normalized"))
# 
# plot(len1, resid(.) ~ Stratum | Region, abline = 0, cex = 0.3)
# plot(len1, resid(.) ~ fitted(.) | Region, abline = 0, cex = 0.3)
# plot(len1, resid(.) ~ Year, abline = 0, cex = 0.3)
# plot(len1, resid(.) ~ Year | Region, abline = 0, cex = 0.3)
# plot(len1, resid(.) ~ Year | Stratum, abline = 0, cex = 0.3)
# plot(len1, resid(.) ~ fitted(.) | Region, abline = 0, cex = 0.3)

#len2 <- gam(Diff ~ Stratum + s(Year, bs="cr") + Region, data = int2)

# summary(len4)
# acf(resid(len4, type="normalized"))
# 
# len3 <- gamm(Diff ~ Stratum + s(Year, bs="cr") + Region, data = int2,
#              correlation = corLin(form = ~ Region + Stratum|Year))
# 
# len3 <- gamm(Diff ~ s(Year, bs="cr") + Stratum + Region, data = int2,
#              correlation = corLin(form = ~ Region + Stratum|Year))

# summary(len3$g)
# hist(int2$Diff)
# library(gratia)
# draw(len2)
# appraise(len2)
# plot(len2)
# gam.check(len2$g)
# acf(resid(len2$lme, type="normalized"))

len4 <- gls(Diff ~ Stratum + Year + Region, data = int2,
             correlation = corExp(form = ~ Region + Stratum|Year))
summary(len4)
r.squaredLR(len4)
acf(resid(len4, type="normalized"))
