#Jonah crab seasonal sex ratio differences
#Ruby Krasnow
#Last modified: July 30, 2023

library(tidyverse)
library(rstatix)
library(ggpubr)
library(car)
 library(lme4)
library(lmerTest)
library(sf)
detach("package:lmerTest", unload = TRUE)
detach("package:lme4", unload = TRUE)
library(nlme)
 detach("package:nlme", unload = TRUE)

df_j_len<- read.csv("data/Maine_inshore_trawl/MEjonahLength.csv") #jonah crab length
df_tows<-read.csv("data/Maine_inshore_trawl/MEtows.csv") #tow data

surveyGrid <-st_read("~/Downloads/lab_notebook/Maine/MaineDMR_-_Inshore_Trawl_Survey_Grid") #CRS: WGS 84/EPSG 4326

surveyGrid <- surveyGrid %>% 
  mutate(Region = region_id,
         Stratum = depth_stra,
         GridID = grid_id, .keep="unused", .before=last_surve)

surveyGrid$area <- as.integer(paste0(surveyGrid$Region, surveyGrid$Stratum))
regionsGrid_orig <- surveyGrid %>% group_by(Region, Stratum, area) %>% summarise(num = n_distinct(GridID))


df_j_len <- df_j_len %>% mutate(Common_Name = "Jonah crab") #note shell height is in cm

df_j_len <- df_j_len %>%
  arrange(Season, Year, Stratum, Region, Tow_Number)

df_sex <- df_j_len %>% filter(Year > 2003) %>% select(Season, Year, Tow_Number, Region, Stratum, Frequency, Sex) %>% mutate(Area = as.numeric(paste0(Region, Stratum))) %>% 
  arrange(Season, Year, Stratum, Region, Tow_Number)

data <- df_sex %>% group_by(Season, Year, Area, Tow_Number, Sex) %>% mutate(Num_Sex = cumsum(Frequency)) %>% slice_tail()

data_complete <- df_sex %>% 
  group_by(Season, Year, Region, Stratum) %>% 
  complete(Sex, Tow_Number) 

data_complete <- data_complete %>% 
  group_by(Season, Year, Region, Stratum, Tow_Number, Sex) %>% 
  mutate(Num_Sex = cumsum(Frequency)) %>% 
  slice_tail()  %>% 
  replace(is.na(.),0)

data_complete <- data_complete %>% filter(Sex != "Unknown") %>% 
  group_by(Season, Year, Region, Stratum, Tow_Number) %>% 
  mutate(Total = cumsum(Num_Sex)) %>% 
  slice_tail() %>% 
  mutate(perc_f = ifelse(Sex=="Female", Num_Sex/Total, (1-(Num_Sex/Total))),
         perc_m = ifelse(Sex == "Male", Num_Sex/Total, (1-(Num_Sex/Total))))

data_complete <- data_complete %>% group_by(Season, Year, Region, Stratum) %>% summarise(perc_m = mean(perc_m)) %>% 
  mutate(Area = as.numeric(paste0(Region, Stratum)))

data_complete_geom <- left_join(regionsGrid_orig %>% mutate(Area = area, .keep="unused"), data_complete)
ggplot(data=data_complete_geom %>% filter(Season=="Fall"))+geom_sf(aes(fill=perc_m))+facet_wrap(~Year)

sex_diff <- pivot_wider(data_complete, names_from="Season", values_from = "perc_m") %>% 
  mutate(Diff = Fall-Spring) %>% na.omit() %>% 
  group_by(Year, Stratum) %>% 
  summarise(Diff = mean(Diff)) %>% 
  filter(Year > 2004)

sex_diff_reg <- pivot_wider(data_complete, names_from="Season", values_from = "perc_m") %>% 
  mutate(Diff = Fall-Spring) %>% na.omit() %>% 
  group_by(Year, Region, Stratum) %>% 
  summarise(Diff = mean(Diff)) %>% 
  filter(Year > 2004)

sex_diff_geom <- left_join(regionsGrid_orig, sex_diff %>% mutate(Stratum = as.numeric(Stratum)))

sex_diff <- sex_diff %>% filter(!(Year==2019 & Stratum==3))  %>% arrange(Stratum)#remove extreme outlier

sex_diff %>% group_by(Stratum) %>% identify_outliers(Diff) #no extreme outliers


sex_diff_reg <- sex_diff_reg %>% filter(!(Year==2010 & Stratum==4 & Region == 5))
sex_diff_reg %>% group_by(Region, Stratum) %>% identify_outliers(Diff)

ggplot(data=sex_diff_geom)+geom_sf(aes(fill=Diff))+facet_wrap(~Year)



mod0 <- lm(Diff ~ Stratum, data=(sex_diff %>% group_by(Stratum) %>% summarise(Diff=mean(Diff))))
summary(mod0)
#across all years
ggplot(data=sex_diff_geom %>% group_by(Stratum) %>% summarize(Diff = mean(Diff)))+geom_sf(aes(fill=Diff))

ggplot(data=sex_diff, aes(x=Stratum, y=Diff, group=Stratum))+geom_boxplot()+theme_bw()

ggplot(data=sex_diff %>% group_by(Stratum) %>% summarise(Diff=mean(Diff)), aes(x=Stratum, y=Diff))+geom_point()+theme_bw()+ stat_smooth(method="lm", se=FALSE)+
  stat_regline_equation(label.y = 0, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = -0.05, aes(label = ..rr.label..))
#test with lobster abundance, Jonah crab landings

#### Basic linear model - only stratum
# Basic ANOVA - only strat
summary(aov(Diff ~ Stratum, data=sex_diff))
summary(aov(Diff ~ Year, data=sex_diff))
summary(aov(Diff ~ Year + Stratum, data=sex_diff))
summary(aov(Diff ~ Year + Stratum + Region, data=sex_diff_reg))

#Basic linear models (same as ANOVA calls above)
mod1<- lm(Diff ~ Stratum, data = sex_diff %>% mutate(Stratum = as.numeric(Stratum)))
mod1
summary(mod1)
shapiro.test(residuals(mod1))

mod1b<- lm(Diff ~ as.factor(Stratum), data = sex_diff)
mod1b
summary(mod1b)
extractAIC(mod1)
# par(mfrow = c(2, 2))
# plot(mod1)
# plot(model,5)
# ggqqplot(sex_diff, "Diff", facet.by = "Stratum")

mod2<- lm(Diff ~ Stratum + Region, data = sex_diff_reg)
mod2
summary(mod2)
shapiro.test(residuals(mod2))
plot(mod2)
extractAIC(mod2)

mod3<- lm(Diff ~ Year + Stratum, data = sex_diff)
mod3
summary(mod3)
shapiro.test(residuals(mod3))
extractAIC(mod3)
par(mfrow = c(2, 2))
plot(mod3)

mod3b<- lm(Diff ~ as.factor(Year) + Stratum, data = sex_diff)
mod3b
summary(mod3b)
extractAIC(mod3b)

mod4 <- lm(Diff ~ Year + Stratum + Region, data=sex_diff_reg)
mod4
summary(mod4)
extractAIC(mod4)
shapiro.test(residuals(mod4))
par(mfrow = c(2, 2))
plot(mod1)

sex_diff %>%
  group_by(Stratum) %>%
  shapiro_test(Diff) 

sex_diff_reg %>%
  group_by(Stratum, Region) %>%
  shapiro_test(Diff) 


durbinWatsonTest(mod1)
durbinWatsonTest(mod2)
durbinWatsonTest(mod3)

#homogeneity of residual variance not met
ncvTest(mod1) 
ncvTest(mod2)
ncvTest(mod3)
sex_diff %>% ungroup() %>% levene_test(Diff ~ Stratum)
leveneTest(Diff ~ Stratum, data=sex_diff)
leveneTest(Diff ~ Stratum, data=sex_diff)

plot(fitted(mod3), resid(mod3), xlab='Fitted Values', ylab='Residuals')


sked.dat = sex_diff %>% ungroup() %>% 
  mutate("abs.resids" = abs(residuals(mod3)))

sked.resid.mod = lm(abs.resids~Stratum, data=sked.dat)

ggplot(sked.dat) +
  geom_point(aes(x=Stratum, y=abs.resids)) +
  geom_abline(aes(intercept=coef(sked.resid.mod)[1],
                  slope=coef(sked.resid.mod)[2]))

###### Welch & Kruskal-Wallis one-way ANOVAs
#robust to violations of equal variance
welch_anova_test(data=sex_diff %>% ungroup(), Diff ~ Stratum)
games_howell_test(data=sex_diff %>% ungroup(), Diff ~ Stratum) #post-hoc for Welch
kruskal.test(data=sex_diff %>% ungroup(), Diff ~ Stratum)


## LMER models with package lme4 and lmerTest
lmer1<- lmer(Diff ~ (1|Year) + Stratum, data=sex_diff)
summary(lmer1)
shapiro.test(residuals(lmer1)) #good
ranova(lmer1)


#aov(Diff~(factor(Stratum)*factor(Year)),sex_diff )
#summary(aov(Diff~(factor(Stratum)*factor(Year)),sex_diff ))

with (sex_diff, {interaction.plot(Year, factor(Stratum), Diff, ylab="mean of Diff", xlab="time", trace.label="Stratum") })

#fit.ar1 <- gls(Diff ~ factor(Stratum), data=sex_diff, corr=corAR1())

lmer2 <- lmer(Diff ~ (1|Year) + as.factor(Stratum) + as.factor(Region), data=sex_diff_reg)
summary(lmer2)
shapiro.test(residuals(lmer2))
extractAIC(lmer2)
ranova(lmer2)

lmer2a <- lmer(Diff ~ (1|Year) + Stratum + Region, data=sex_diff_reg)
lmer2b <- lmer(Diff ~ (1|Year) + as.factor(Stratum) + Region, data=sex_diff_reg)

lmer3 <- lmer(Diff ~ (1|Year) + Stratum + (1|Region), data=sex_diff_reg)
ranova(lmer3)
summary(lmer3)
shapiro.test(residuals(lmer3))
extractAIC(lmer3)
anova(lmer2, lmer2a, lmer3)

ranova(lmer2)

# lmer4 <- lmer(Diff ~ Stratum + (1|Region), data=sex_diff_reg)
# summary(lmer4)
# shapiro.test(residuals(lmer4))
# extractAIC(lmer4)

anova(lmer2, lmer2a, lmer2b, lmer3)

ranova(lmer3)

M.lm <-gls(Diff~Stratum, data=sked.dat)

summary(gls(Diff~Stratum,weights=varPower(), data=sked.dat))

mod3gls <- gls(Diff~Stratum,weights=varPower(), data=sked.dat)
summary(mod3gls)
shapiro.test(residuals(mod3gls))



vf3 <- varPower(form =~ Stratum)
M.gls3 <- gls(Diff ~ Stratum,
              weights = vf3,data=sked.dat)
shapiro.test(residuals(M.gls3))
summary(M.gls3)


vf5 <- varExp(form =~ Stratum)
M.gls5 <- gls(Diff ~ Stratum,
              weights = vf5, data = sex_diff)


vf6<-varConstPower(form =~ Stratum)
M.gls6<-gls(Diff ~ Stratum,
            weights = vf6, data = sex_diff)


# anova(M.lm, mod3gls,M.gls3,M.gls5,M.gls6)
# acf(residuals(mod3gls))
# durbinWatsonTest(mod3gls)
# 
# 
# w <- 1/(sked.dat$Stratum)
# wls<- lm(Diff ~ Stratum, weights=w, data=sked.dat)
# summary(wls)
# plot(wls)
# shapiro.test(wls$residuals)
# ncvTest(wls)
