#Jonah crab seasonal sex ratio differences
#Ruby Krasnow
#Last modified: Aug 18, 2023

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
library(MuMIn)
library(lmtest)

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


# ggplot(data=sex_diff, aes(x=Stratum, y=Diff, group=Stratum))+geom_violin()+theme_bw()

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
ggplot(data=sex_diff_agg, aes(x=Stratum, y=Diff))+
  geom_point()+
  theme_bw()+
  stat_smooth(method="lm", se=FALSE)+
  stat_regline_equation(label.y = 0, aes(label = after_stat(eq.label))) +
  stat_regline_equation(label.y = 0.05, aes(label = after_stat(rr.label)))+
  ylim(-0.1, 0.3)

sex_diff_geom_agg <- left_join(regionsGrid_orig, sex_diff_agg) #join to sf for visualization

p1 <- ggplot(data=sex_diff_geom_agg)+geom_sf(aes(fill=Diff))+scale_fill_continuous(name="")

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


tot_date <- df_tows %>% 
  filter(!(Region==4 & Depth_Stratum==2 & Year==2017 & Tow_Number==90)) %>% #errant February
  mutate(date = ymd_hms(Start_Date), .keep="unused", .before=Season) %>% 
  mutate(stdYear = `year<-`(date, 2000), .before=Season) %>% 
  mutate(stdYear = date(stdYear)) %>% 
  group_by(Year, Region, Depth_Stratum) %>% 
  summarise(len = max(stdYear) - min(stdYear)) %>% 
  mutate(Stratum = Depth_Stratum, .keep="unused") %>%
  ungroup() %>% filter(Year > 2004 & Year != 2020) %>% mutate(len = ifelse(len < 10, 151, len))

int2 <- left_join(tot_date, sex_diff_reg) %>% na.omit()

len4 <- gls(Diff ~ Stratum + Year + Region, data = int2,
            correlation = corExp(form = ~ (Region + Stratum)|Year))
summary(len4)
tidy(len4)

r.squaredLR(len4)
r.squaredLR(len1)
acf(resid(len4, type="normalized"))
Box.test(residuals(len4, type="normalized"), type="L")
shapiro.test(resid(len4, type="normalized"))


plot(len4)
plot(len4, resid(.) ~ Stratum | Region, abline = 0, cex = 0.3)
plot(len4, resid(.) ~ Year, abline = 0, cex = 0.3)
plot(len4, resid(.) ~ Year | Region, abline = 0, cex = 0.3)
plot(len1, resid(.) ~ Year | Stratum, abline = 0, cex = 0.3)


len1 <- lme(Diff ~ Stratum + Year + Region, random = ~ 1|len, data = int2, correlation = corAR1())
summary(len1)
plot(len1)
shapiro.test(residuals(len1, type="normalized"))
Box.test(residuals(len1, type="normalized"), type="L")
acf(resid(len1, type="normalized"))
pacf(residuals(len1, type="normalized"))

# plot(len1, resid(.) ~ Stratum | Region, abline = 0, cex = 0.3)
# plot(len1, resid(.) ~ fitted(.) | Region, abline = 0, cex = 0.3)
# plot(len1, resid(.) ~ Year, abline = 0, cex = 0.3)
# plot(len1, resid(.) ~ Year | Region, abline = 0, cex = 0.3)
# plot(len1, resid(.) ~ Year | Stratum, abline = 0, cex = 0.3)
# plot(len1, resid(.) ~ fitted(.) | Region, abline = 0, cex = 0.3)


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


