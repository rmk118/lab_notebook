#Code chunks I commented out of other documents
# Moved here to improve readability of actual code

#ggplot(scallopsNew, aes(x = CRUISE)) + geom_bar()
#ggplot(scallopsNew, aes(x = CRUISE6)) + geom_bar()
#ggplot(scallopsNew, aes(x = STRATUM)) + geom_bar()

# diffMonths <- df_stationsNew %>% 
#   filter(EST_MONTH != GMT_MONTH)


# trues <- species %>%  filter(!is.na(species$avg)==TRUE)

#this gets rid of some weird strata in 1984 with letters, which don't seem to 
# correspond to any of the NOAA (shellfish, dredge, or trawl survey) strata
scallopsNew <-  scallopsNew %>%
  mutate(STRATUM = as.integer(STRATUM)) %>% 
  filter(!is.na(STRATUM))



# df_stations_wrangled <- df_stations %>%
#   # filter(EST_YEAR %in% unlist(target_chunks)) %>%
#   mutate(DATE = parse_date_time(BEGIN_EST_TOWDATE,orders="mdYHMS",truncated = 3)) %>%
#   select(STATION,STRATUM,DATE) %>%
#   mutate(DATE=year(DATE)) %>%
#   distinct()
# 
# #shows how many occurrences of each station are in the data (i.e. how many years that station was sampled)
data.frame(table(df_stations$STATION))

# data.frame(table(df_stations_wrangled$DATE)) #only goes up to 2006, then a few in 2015 and 2021. Missing HabCam data?

# Find strata with all 45 years of data -----------------------------------------------------

#This will show you which ones were removed
# scallopsTest <-  scallopsNew %>%
#   mutate(strat = as.integer(STRATUM)) %>% 
#   filter(is.na(strat))

#shows how many occurrences of each stratum are in the data (i.e. how many years that stratum was sampled)
data.frame(table(cat$STRATUM))
data.frame(table(len$STRATUM))

scallop_E <- EmbedDimension(dataFrame = formatScal, lib = "1 21", pred = "1 21", columns = "avg",target = "avg")
scallop_theta <- PredictNonlinear(dataFrame = formatScal, lib = "1 21", pred = "1 21", columns = "avg",target = "avg", E = 8)
asterias_E <- EmbedDimension(dataFrame = formatStar, lib = "1 19", pred = "1 19", columns = "avg",target = "avg")
asterias_theta <- PredictNonlinear(dataFrame = formatStar, lib = "1 19", pred = "1 19", columns = "avg",target = "avg", E = 6)



#Scallop biomass exploration
#Ruby Krasnow
#Last modified: June 27, 2023

#NEFSC scallop survey data pulled from Northeast Ocean Data - 1975 through 2015, plus 1966
st_layers("~/Downloads/Fish/Fish.gdb")
fish <- st_read("~/Downloads/Fish/Fish.gdb", layer="ScallopBiomass")

attr(fish, "sf_column")
st_geometry(fish)
class(fish)

par(mar = c(0,0,1,0))
plot(fish)


fish <- fish %>% 
  filter(year_ > 1966) %>% 
  mutate(YEAR = year_, .keep = "unused")

onlyData <- fish %>% 
  na.omit()

# stations30 <- onlyData %>% 
#   group_by(station) %>% 
#   summarise(years=n_distinct(YEAR)) %>% 
#   filter(years >= 30) %>% 
#   ungroup()
# 
# df_stations_wrangled <- onlyData %>%
#   filter(YEAR %in% 1985:2015) %>% 
#   group_by(station) %>% 
#   summarise(years=n_distinct(YEAR))
# 
# stations <- onlyData %>% 
#   filter(station %in% stations30$station)


#Not very many observations
# testingStars<- read.csv("data/Maine_inshore_trawl/starCatch.csv")
# testingStars %>%  distinct(Common_Name)
# testingStars <- testingStars %>% filter(Common_Name == "Northern Sea Star" | Common_Name == "Star Common")



#ggplot(j_cat, aes(x=Year, y=avgCatch, group=Region, color=Region))+geom_line()+facet_wrap(~Stratum)
# ggplot(r_cat, aes(x=Survey, y=avgCatch, group=Region, color=Region))+geom_line()+facet_wrap(~Stratum)
# ggplot(s_cat, aes(x=Survey, y=avgCatch, group=Region, color=Region))+geom_line()+facet_wrap(~Stratum)
#
# ggplot(j_cat, aes(x=Survey, y=avgCatch, group=Stratum, color=Stratum))+geom_line()+facet_wrap(~Region)
# ggplot(r_cat, aes(x=Survey, y=avgCatch, group=Stratum, color=Stratum))+geom_line()+facet_wrap(~Region)
# ggplot(s_cat, aes(x=Survey, y=avgCatch, group=Region, color=Region))+geom_line()+facet_wrap(~Stratum)