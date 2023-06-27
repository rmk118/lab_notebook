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

stations30 <- onlyData %>% 
  group_by(station) %>% 
  summarise(years=n_distinct(YEAR)) %>% 
  filter(years >= 30) %>% 
  ungroup()

df_stations_wrangled <- onlyData %>%
  filter(YEAR %in% 1985:2015) %>% 
  group_by(station) %>% 
  summarise(years=n_distinct(YEAR))

stations <- onlyData %>% 
  filter(station %in% stations30$station)
  
  
plot(stations)
