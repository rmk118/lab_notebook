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

# scalLogCatchFall_1.2 <- scalLogCatchFall %>% 
#   filter(Region==1, Stratum==2) %>% 
#   ungroup() %>% 
#   select(Year, logCatch)
# 
# scalLogCatchFall_1ab <- left_join(scalLogCatchFall_1a, scalLogCatchFall_1b, by="Year", suffix=c("_a","_b"))

#EmatScalFall <- data.frame(matrix(ncol = numStrata, nrow = numRegions))
#colnames(EmatScalFall) <- c("Stratum 1", "Stratum 2", "Stratum 3", "Stratum 4")

# smplx<- Simplex(dataFrame = scalLogCatchFall_1ab, lib = "1 23", pred = "1 23", columns = "logCatch_a", target = "logCatch_b", E = 2, showPlot = TRUE)
# err <- ComputeError( smplx$Observations, smplx$Predictions )


########### The "last surve and surveys" columns have not been updated since 2019, so they cannot be used to filter which grids have actually been surveyed and when

#crs=4326
#crs=4269
#crs=4267

# newMatrix<- data.frame(matrix(nrow=5, ncol=4))
# 
# for (i in 1:5) {
#   for (j in 1:4) {
# tempArea <- neighbors_df_test %>% 
#   group_by(objectID) %>% 
#   filter(neighborRegion == paste(i,j)) %>% 
#   filter(objectRegion != paste(i,j))
# 
# newMatrix[i,j]<-nrow(tempArea)
# print(paste(i,j))
# print(nrow(tempArea))
# }
# }
# 
# tempMat <- neighbors_df_test %>% 
#   group_by(objectID) %>% 
#   filter(neighborRegion == "3 4")


# stuff with the surveyed grid and nearest neighboring grids --------------

st_queen <- function(a, b = a) st_relate(a, b, pattern = "F***T****")
sf.sgbp.surveyed <- st_queen(surveyed)

as.nb.sgbp <- function(x, ...) {
  attrs <- attributes(x)
  x <- lapply(x, function(i) { if(length(i) == 0L) 0L else i } )
  attributes(x) <- attrs
  class(x) <- "nb"
  x
}
sf.nb.surveyed <- as.nb.sgbp(sf.sgbp.surveyed)
summary(sf.nb.surveyed)

test <- st_centroid(surveyed)
head(test) #CRS: WGS 84/EPSG 4326

longitude <- map_dbl(surveyed$geometry, ~st_centroid(.x)[[1]])
latitude <- map_dbl(surveyed$geometry, ~st_centroid(.x)[[2]])
coords <- cbind(longitude, latitude)
head(coords)
plot(sf.nb.surveyed, coords, lwd=.2, col="blue", cex = .5)

surveyed.card <- card(sf.nb.surveyed)
max(surveyed.card)
ggplot() +
  geom_histogram(aes(x=surveyed.card), breaks = seq(0,9, by = 1)) +
  xlab("number of neighbors")

surveyedNoGeom <- st_drop_geometry(surveyed) %>% select(c("OBJECTID", "region_stratum"))

neighbors_df<-data.frame(sf.sgbp.surveyed)
neighbors_df<- neighbors_df %>% 
  mutate(OBJECTID = surveyedNoGeom[row.id, "OBJECTID"], .keep="unused") %>% 
  mutate(neighborID = surveyedNoGeom[col.id, "OBJECTID"], .keep="unused") 

neighbors_df_test<- left_join(neighbors_df, surveyedNoGeom) %>% 
  rename(objectRegion = region_stratum, objectID = OBJECTID)
neighbors_df_test <- left_join(neighbors_df_test, surveyedNoGeom, by = c("neighborID"="OBJECTID"))

#grid_id starts again at 1 for each region
#st_layers("~/Downloads/lab_notebook/Maine/MaineDMR_-_Inshore_Trawl_Survey_Grid")

# This plot adds a 1000m buffer around each point
#ggplot()+ geom_sf(data=region1.1grid)+geom_sf(data = st_buffer(region1.1points, 1000)) 

Box.test(s_catchTidy %>% filter(Region==1, Stratum==3, Season=="Fall", Type=="avgLogCatch") %>% pull(value), lag=5, type="Ljung-Box")
acf(s_catchTidy %>% filter(Region==2, Stratum==3, Season=="Fall", Type=="avgLogCatch") %>% pull(value))

logCatchFall <- catchTidy %>% filter(Type == "logCatch") %>% filter(Season == "Fall")

