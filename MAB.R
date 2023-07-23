#Mid-Atlantic Bight testing
#Ruby Krasnow
#Last modified: July 23, 2023

library(tidyverse)
library(lubridate)
library(patchwork)
library(rEDM)
library(tseries)
library(zoo)

# #Import data
# df_mab<-read.csv("data/mabmeans.csv")
# df_scal <- df_mab %>% select(year,ScalNum, ScalBms)
# plot(df_mab$ScalNum)
# mab_ts<-as.ts(read.zoo(df_mab)) 
# 
# plot(mab_ts)
# 
# ggplot(data=df_scal, aes(x=year, y=ScalNum))+geom_line()
# ggplot(data=df_scal, aes(x=year, y=bmsLag))+geom_line()
# 
# kpss.test(df_scal$ScalNum, null="Trend")
# kpss.test(df_scal$bmsLag, null="Level")
# ?kpss.test
# 
# df_scal$numLag = diff(df_scal$ScalNum, 1)
# df_scal <- df_scal %>% mutate(numLag = c(diff(ScalNum, 1), NA),
#                               bmsLag = c(diff(ScalBms, 1), NA))
# 
# 
# findE_v(df_scal$numLag)
# EmbedDimension(dataFrame = df_scal, lib = "1 22", pred = "1 22", columns = "ScalNum",target = "ScalNum", maxE = 7)
# PredictNonlinear(dataFrame = df_scal, lib = "1 23", pred = "1 23", columns = "ScalNum",target = "ScalNum", E = 3)

#Import data
df_mab<-read.csv("data/mabsubareas.csv") 

df_scal <- df_mab %>% select(subarea, year, ScalNum, ScalBms) %>% head(n=-1)

df_mab <- df_mab %>% mutate(AsteriasNum = AsteriasnNum + LeptasteriasNum, 
                            AsteriasBms = AsteriasBms + LeptastiasBms,.keep = "unused") %>% 
  rename(Astropecten_Num = AstropectenNum,
         Astropecten_Bms = AstropectenBms,
         Asterias_Num = AsteriasNum,
         Asterias_Bms = AsteriasBms,
         Cancer_Num = CancerNum,
         Cancer_Bms = CancerBms,
         Scal_Num = ScalNum,
         Scal_Bms = ScalBms) %>% filter(year < 2014)

df_tidy <- df_mab %>% pivot_longer(cols = where(is.double), names_to = c("Species", "Type"), 
                                   names_sep = "_", values_to = "value") %>% 
  mutate(Species = as.factor(Species), Type = as.factor(Type), subarea = as.factor(subarea), .keep="unused")

ggplot(data = df_tidy %>% filter(Type == "Num"))+geom_line(aes(x=year, y=value,color=Species ))+facet_wrap(~subarea)
ggplot(data = df_tidy %>% filter(Type == "Bms"))+geom_line(aes(x=year, y=value,color=Species ))+facet_wrap(~subarea)
ggplot(data = df_tidy %>% filter(Type == "Bms"))+geom_line(aes(x=year, y=value,color=subarea ))+facet_wrap(~Species, scales = "free_y")

findE_v(df_tidy %>% filter(Type == "Bms", Species == "Scal", subarea=="DMV") %>% pull(value))

findSpeciesE_mab <- function(df, type, species, maxE = 7) {
    df_out <- df %>% 
      filter(Type == type, Species==species) %>% 
      group_by(subarea) %>% 
      select(year, value) %>%
      summarise(E_opt = findE_v(value, maxE)) %>%
      pivot_wider(names_from = subarea, values_from = E_opt)
  
  return(df_out)
}

findSpeciesE_mab(df_tidy, species="Scal", type="Bms", maxE = 6)

E_results_num <- df_tidy %>% group_by(Species) %>% summarise(E = findSpeciesE_mab(df = df_tidy, type="Num", species = Species, maxE = 6))
E_results_num_agg <- df_tidy %>% ungroup() %>% group_by(Species) %>% summarise(E = findSpeciesE_mab(df = ., type="Num", species = Species, maxE = 6))


findSpeciesErho <- function(df, season=NULL, type) {

    df_out <- df %>% 
      filter(Type == type) %>% 
      group_by(Region, Stratum) %>% 
      select(Year, value) %>%
      summarise(E_opt_rho = findErho_v(value)) %>%
      pivot_wider(names_from = Stratum, values_from = E_opt_rho) %>% 
      ungroup() %>% 
      select(-Region) 
  return(df_out)
}
