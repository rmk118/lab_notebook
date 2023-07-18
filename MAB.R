#Mid-Atlantic Bight testing
#Ruby Krasnow
#Last modified: July 18, 2023

library(tidyverse)
library(lubridate)
library(patchwork)
library(rEDM)
library(tseries)
library(zoo)

#Import data
df_mab<-read.csv("data/mabmeans.csv")
df_scal <- df_mab %>% select(year,ScalNum, ScalBms)
plot(df_mab$ScalNum)
mab_ts<-as.ts(read.zoo(df_mab)) 

plot(mab_ts)


ggplot(data=df_scal, aes(x=year, y=ScalNum))+geom_line()
ggplot(data=df_scal, aes(x=year, y=bmsLag))+geom_line()


kpss.test(df_scal$ScalNum, null="Trend")
kpss.test(df_scal$bmsLag, null="Level")
?kpss.test

df_scal$numLag = diff(df_scal$ScalNum, 1)
df_scal <- df_scal %>% mutate(numLag = c(diff(ScalNum, 1), NA),
                              bmsLag = c(diff(ScalBms, 1), NA))


findE_v(df_scal$numLag)
EmbedDimension(dataFrame = df_scal, lib = "1 22", pred = "1 22", columns = "ScalNum",target = "ScalNum", maxE = 7)

PredictNonlinear(dataFrame = df_scal, lib = "1 23", pred = "1 23", columns = "ScalNum",target = "ScalNum", E = 3)
