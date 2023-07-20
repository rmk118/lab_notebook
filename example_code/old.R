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


# Second highest E --------------------------------------------------------

findE_v_new <- function(v) {
  lib_vec <- paste(1, length(v))
  indices <- c(1:length(v))
  df <- data.frame(indices,v)
  colnames(df)<-c("index", "value")
  rho_E<- EmbedDimension(dataFrame = df, lib = lib_vec, pred = lib_vec, columns = "value",target = "value", maxE = 7)
  r<- rho_E$rho
  E_out<-rho_E[which.max(r),"E"][1]
  
  if (E_out == 1) {
    rho2<- max(r[-E_out])
    print(rho2)
    E_out_2 <- which(r==rho2)
    return(E_out_2)
    
  } else {
    return(E_out)
  }
}

findSpeciesE_new <- function(df, season, type) {
  df_out <- df %>% 
    filter(Type == type, Season == season) %>% 
    group_by(Region, Stratum) %>% 
    select(Year, value) %>%
    summarise(E_opt = findE_v_new(value)) %>%
    pivot_wider(names_from = Stratum, values_from = E_opt) %>% 
    ungroup() %>% 
    select(-Region)
  return(df_out)
}

findE_v_new(logCatchFall %>% filter(Species=="scallop", area=="1 1") %>% pull(value))
findE_v_new(logCatchFall %>% filter(Species=="scallop", area=="1 4") %>% pull(value))
findSpeciesTheta(df=(catchTidy %>% filter(Species=="scallop")), df_Es=scalCatchEs, season="Fall", type="logCatch")
findSpeciesTheta(df=(catchTidy %>% filter(Species=="scallop")), df_Es=scalCatchEs_new, season="Fall", type="logCatch")

second<-EmbedDimension(dataFrame = (logCatchFall %>% filter(Species=="scallop", area=="1 4") %>% ungroup() %>% select(Year, value)), lib = "1 20", pred = "1 20", columns = "value",target = "value", maxE = 7)
secondE<- max(second$rho[-which.max(second$rho)])
which(second$rho == secondE)

scalCatchEs_new<-findSpeciesE_new((catchTidy %>% filter(Species=="scallop")), season="Fall", type="logCatch")



# CCM for copred ----------------------------------------------------------

# emptyXmap <- data.frame(matrix(ncol=43, nrow=1))
# colnames(emptyXmap)<- c(xmapCols, "area1")
# emptyXmap[,] <- NA
# for (k in 1:5) {
#   for (j in 1:4) {
#     areaTemp <- paste(k, j)
df_temp_xmap <- catch %>% filter(Season =="Fall", area=="1 1")%>% ungroup() %>% select(Year, all_of(vars))
do_xmap_noID(df_temp_xmap, predictor = "avgLogCatch_s", target="avgLogCatch_r", E_max=5, tp=1,keep_preds = FALSE)
#emptyXmap<- bind_rows(emptyXmap, outputDf)

# xmapCols<-colnames(do_xmap_noID(df_temp_xmap, predictor = "avgLogCatch_s", target="avgLogCatch_r", E_max=5, tp=1,keep_preds = FALSE))
#View(make_xmap_block_noID(df = df_temp_xmap, predictor = avgLogCatch_s, target=avgLogCatch_r, E_max=5, cause_lag=1))




# Concantenation for copred - manual -----------------------------------------------


# intBlock<- make_xmap_block_ID(df=(logCatchFallInt %>% filter(Species=="scallop") %>% ungroup() %>% select(areaInt, Year, value)), predictor=value, target=value, ID_col=areaInt, E_max=6, cause_lag=1) %>%
#   filter(complete.cases(.))

# lib_1 <- paste(1,nrow(intBlock))
# 
# out_1 <- map_df(1:6,function(E_i){
#   columns_i <- names(intBlock)[4:(E_i+3)]
#   out_i <- Simplex(dataFrame=intBlock,
#                    lib="1 200",pred="201 314",Tp=0,
#                    target="target",
#                    columns=columns_i,
#                    embedded=TRUE,
#                    parameterList = TRUE,
#                    E=E_i, showPlot = TRUE)
#   params_i <- out_i$parameters
#   out_i <- out_i$predictions %>% filter(complete.cases(.))
#   
#   stats_i <- compute_stats(out_i$Observations,out_i$Predictions)
#   print(bind_cols(data.frame(E=E_i),stats_i))
#   return(bind_cols(data.frame(E=E_i),stats_i))
#   
# })

# make_xmap_block_ID(logWtFallInt %>% filter(Species=="scallop") %>% ungroup() %>% select(areaInt, Year, value), predictor=value, target=value, ID_col=areaInt, E_max=6,cause_lag=1)
# 
# intBlockWt<- make_xmap_block_ID(df=(logWtFallInt %>% filter(Species=="scallop") %>% ungroup() %>% select(areaInt, Year, value)), predictor=value, target=value, ID_col=areaInt, E_max=6, cause_lag=1) %>%
#   filter(complete.cases(.))

# lib_2 <- paste(1,nrow(intBlockWt))
# 
# out_2 <- map_df(1:6,function(E_i){
#   columns_i <- names(intBlockWt)[4:(E_i+3)]
#   out_i <- Simplex(dataFrame=intBlockWt,
#                    lib=lib_2,pred=lib_2,Tp=0,
#                    target="target",
#                    columns=columns_i,
#                    embedded=TRUE,
#                    parameterList = TRUE,
#                    E=E_i, showPlot = TRUE)
#   params_i <- out_i$parameters
#   out_i <- out_i$predictions %>% filter(complete.cases(.))
#   
#   stats_i <- compute_stats(out_i$Observations,out_i$Predictions)
#   
#   return(bind_cols(data.frame(E=E_i),stats_i))
#   
# })




# Graphics testing --------------------------------------------------------

M = cor(mtcars[1:3])
patchObj <- ggcorrplot(M)
N = cor(mtcars[4:6])
patchObj <- wrap_elements(panel=ggcorrplot(M)) + wrap_elements(panel=ggcorrplot(N))
O = cor(mtcars[7:9])
patchObj <- patchObj + wrap_elements(panel=ggcorrplot(O))

 # NOTE TO SELF: ALWAYS CHECK THAT YOU AREN'T REUSING VARIABLE NAMES
for (i in 1:5) {
  for (j in 1:4) {
    # cat("i=",i, ",")
    # cat("j=",j, "\n")
    print(paste(i, j))
  }}


# from both seasons.R -----------------------------------------------------

# logCatch <- catchTidy_seasons %>% filter(Type == "logCatch")
# logWt <- catchTidy_seasons %>% filter(Type == "logWt") %>% 
#   mutate(date=paste(Year, case_when(Season== "Fall" ~ "-11-01",
#                                     Season =="Spring" ~"-05-01"), sep = ""))
# logWt %>% group_by(area, Species) %>% arrange(Year) %>% select(value) %>% 
#   group_map(~ts(., frequency = 2, start=c(2000, 2)))

# logWt_complete <- complete(data=logWt %>% ungroup(), date, Species, Region, Stratum, explicit = TRUE) %>% mutate(area = paste(Region, Stratum), Type="logWt") %>% select(date, Species, area, value)

# logWt_areas <- logWt_complete %>% group_by(area, Species) %>% arrange(date) %>% select(value) %>% 
#   group_map(~ts(., frequency = 2, start=c(2000, 2)))
# 
# logWt_areas2 <- logWt_complete %>% group_by(area) %>% arrange(date) %>% select(Species, value) %>% 
#   group_map(~ts(., frequency = 2, start=c(2000, 2)))


# logWt_ts <- ts(logWt, frequency = 2)
# logCatch_ts <- ts(logCatch, frequency = 2)
# catch_seasons_ts <- ts(catch_seasons %>% ungroup() %>% filter(area== "1 1") %>% select(Season, Year,avgLogCatch_s) %>% arrange(Year), frequency = 2)
# 
# autoplot(catch_seasons_ts)
# monthplot(catch_seasons_ts, phase = "Season")
# 
# s_cat_seasons <- ts(s_cat_clean %>% filter(area== "1 1") %>% select(Date,Tow_Number, Region, Stratum, logWt) %>% group_by(Date, Region, Stratum) %>% mutate(date = as.POSIXct(Date)) %>% summarise(lWt = mean(logWt)))

# plot(catch_ts[[1]])
# Box.test(catch_ts[[1]], type = "Ljung-Box")
# #apply(catch_ts[[1]], 2, kpss.test, null = "Trend")                      
# 
# decompose(catch_ts2)
# plot(decompose(catch_ts2))
# ggAcf(catch_ts[[1]])
# 
# catch_ts2 <- na.interp(catch_ts[[1]])
# catch_ts2[[1]] %>%
#   stl() %>%
#   autoplot()
# 
#catch_ts2diff <- map(catch_ts, diff, lag=2)
# 
# plot(catch_ts2)
# plot(catch_ts2diff)
# 
#fried(catch_ts[[1]])
#map(catch_ts2diff, fried, freq=2)
#map(catch_ts2diff, isSeasonal, test="qs",freq=2)
# summary(seastests::wo(catch_ts[[1]]))
# # plot(decompose(catch_ts2diff))

# catch_complete_tidy <-pivot_longer(catch_complete, 
#                                    cols = 7:ncol(catch_complete)) %>% 
#   mutate(Type = case_when(
#     startsWith(name, "avgCatch_") ~"catch",
#     startsWith(name,"avgWt_") ~"wt",
#     startsWith(name,"avgLogWt") ~"logWt",
#     startsWith(name,"avgLogCatch") ~"logCatch")) %>% 
#   mutate(Species = case_when(
#     endsWith(name, "s") ~"scallop",
#     endsWith(name, "r") ~"rock",
#     endsWith(name, "j") ~"jonah")) %>%
#   mutate(area = as.factor(area), Species = as.factor(Species),
#          Region = as.factor(Region), Type = as.factor(Type),
#          Stratum = as.factor(Stratum)) %>% 
#   select(-name)

# logCatchComplete <- catch_complete_tidy %>% filter(Type == "logCatch")
# logWtComplete <- catch_complete_tidy %>% filter(Type == "logWt")



do_xmap_ID(df=complete_tidy_diff %>% filter(Species=="scallop", Type=="catch") %>% group_by(date) %>% 
             summarise(avg = mean(value, na.rm = TRUE)) %>% 
             ungroup() %>% select(date, avg) %>% mutate(ID=1), ID_col = "ID",
           predictor="avg", target="avg", E_max=7, tp=1) #better than linear!

do_xmap_ID(df=complete_tidy_diff %>% filter(Species=="scallop", Type=="wt") %>% group_by(date) %>% 
             summarise(avg = mean(value, na.rm = TRUE)) %>% 
             ungroup() %>% select(date, avg) %>% mutate(ID=1), ID_col = "ID",
           predictor="avg", target="avg", E_max=7, tp=1) #also better than linear

make_xmap_block_noID(df=catchCCMdf %>% filter(area==11), predictor = scallop, target = jonah, E_max = 7, 
                     cause_lag = 1)

do_xmap_noID(df=catchCCMdf %>% filter(area==14) %>% select(-c(Region, Stratum, area)), predictor = "scallop", target = "jonah", E_max = 7,tp = 1)

#out1<- catchCCMdf %>% filter(area == areaInput) %>% mutate(predator=predator,
#                                                                         prey=prey,
#                                                                         direction= paste("prey","->","predator")

summary(lm(as.formula(scallop ~ jonah),data=data.frame(catchCCMdf %>% filter(area == 11))))

library(correlation)
cor_to_p(cor=0.2444023, n=35)
library(psych)
fisherz(rho=0.2444023)
r.con(rho=0.2444023, n=35, p=0.95)

# print(ccf(catchCCMdf %>% filter(area == 11) %>% pull(jonah), catchCCMdf %>% filter(area == 11) %>% 
#       pull(scallop), type="correlation"))

#ggplot(data=logCatch, aes(x=Year, y=value, color=Species)) +geom_line()+facet_grid(Region~Stratum)