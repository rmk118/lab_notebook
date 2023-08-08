#Code chunks I commented out of other documents
# Moved here to improve readability of actual code

# library(ggfortify)
# library(xts)
# library(tseries) #for KPSS test for stationarity
#library(forecast)
#library(seastests)



# NOAA stuff --------------------------------------------------------------

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


# Maine starts ------------------------------------------------------------

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


# surveyed grid (not accurate) --------------

st_queen <- function(a, b = a) st_relate(a, b, pattern = "F***T****")
sf.sgbp.surveyed <- st_queen(surveyed)

# as.nb.sgbp <- function(x, ...) {
#   attrs <- attributes(x)
#   x <- lapply(x, function(i) { if(length(i) == 0L) 0L else i } )
#   attributes(x) <- attrs
#   class(x) <- "nb"
#   x
# }
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

# Graphics testing (corr)--------------------------------------------------------

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

# jonah_catchE_rho <- findSpeciesErho(complete_tidy_diff %>% filter(Species=="jonah") %>% na.omit(), type="catch")%>% round(digits=3) %>% rowid_to_column(var="region")
# 
# jonah_wtE <- findSpeciesE(complete_tidy_diff %>% filter(Species=="jonah") %>% na.omit(), type="wt") 
# jonah_wtE_rho <- findSpeciesErho(complete_tidy_diff %>% filter(Species=="jonah") %>% na.omit(), type="wt")%>% round(digits=3)
# 
# c(as.matrix(jonah_catchE))
# c(as.matrix(jonah_wtE))
# c(as.matrix(jonah_wtE))
# 
# #predictability for simplex weight vs catch
# boxplot(c(as.matrix(jonah_catchE_rho)), c(as.matrix(jonah_wtE_rho)))
# 
# jonah_theta<- findSpeciesTheta(complete_tidy_diff %>% filter(Species=="jonah") %>% na.omit(), type="catch") %>% rowid_to_column(var="region")
# jonah_theta_rho <- findSpeciesTheta_rho(complete_tidy_diff %>% filter(Species=="jonah") %>% na.omit(), type="catch") %>% rowid_to_column(var="region")
# 
# jonah_theta_long <- jonah_theta %>% pivot_longer(cols=2:5, names_to = "stratum", values_to = "theta")
# jonah_E_long <- jonah_catchE %>% pivot_longer(cols=2:5, names_to = "stratum", values_to = "E")
# jonah_theta_rho_long <- jonah_theta_rho %>% pivot_longer(cols=2:5, names_to = "stratum", values_to = "rho")
# jonah_E_rho_long <- jonah_catchE_rho %>% pivot_longer(cols=2:5, names_to = "stratum", values_to = "rho")
# 
# jonahGeom <- left_join(regionsGrid_orig, catchCCMdf) %>% select(-c("scallop", "rock"))
# 
# ggplot(data = complete_tidy_diff %>% 
#          filter(Type == "wt", Species=="jonah"), aes(x=date, y=value, color=area))+geom_line()
# 
# ggplot(data = complete_tidy_diff %>% 
#          filter(Type == "wt", Species != "scallop"), aes(x=date, y=value, color=area))+geom_line()+facet_wrap(~Species) 
# #a lot more variation in the jonah crabs
# 
# ggplot(data = complete_tidy_diff %>% filter(Type == "catch", Species != "scallop") %>% group_by(date, Species) %>% 
#          summarise(avg = mean(value, na.rm = TRUE)), aes(x=date, y=avg))+geom_line()+facet_wrap(~Species)+labs(y="2nd-differenced catch")+theme_classic()
# #a lot more variation in the jonah crabs
# 
# 
# ggplot(data = catchTidy_seasons %>% filter(Type == "catch", Species != "scallop") %>% group_by(Year, Season, Species) %>% summarise(avg = mean(value, na.rm = TRUE)), aes(x=Year, y=avg))+geom_line()+facet_grid(Season~Species)+labs(y="catch")+theme_classic()
# #a lot more variation in the jonah crabs
# 
# catchTidy_seasons_complete<- complete(data = catchTidy_seasons %>% ungroup() %>%  filter(Type=="catch", Species == "jonah"),  Region, Stratum, Season, Year) %>% 
#         mutate(area = as.numeric(paste0(Region, Stratum))) %>% 
#         mutate(date=paste(Year, case_when(Season== "Fall" ~ "-11-01", Season =="Spring" ~"-05-01"), sep = ""), .before=Region) %>% filter(date != "2000-05-01")
# #%>% select(-c("Species", "Type", "Year", "Season", "Region", "Stratum")) %>% arrange(date) %>% group_by(area)
#  
# cor(c(as.matrix(sq_miles)), c(as.matrix(jonah_catchE)))
# 
# sq_miles_v <- c(as.matrix(sq_miles))
# tows_v <- c(as.matrix(tows_per_area))
# 
# jonah_catchE_v <- c(as.matrix(jonah_catchE))
# jonah_catchE_rho_v <- c(as.matrix(jonah_catchE_rho))
# 
# jonah_catch_theta_v <- c(as.matrix(jonah_catch_theta))
# jonah_catch_theta_rho_v <- c(as.matrix(jonah_catch_theta_rho))
# 
# jonah_wtE_v <- c(as.matrix(jonah_wtE))
# jonah_wtE_rho_v <- c(as.matrix(jonah_wtE_rho))
# 
# jonah_wt_theta_v <- c(as.matrix(jonah_wt_theta))
# jonah_wt_theta_rho_v <- c(as.matrix(jonah_wt_theta_rho))
# 
# corDf <- data.frame(sq_miles_v, tows_v, jonah_catchE_v, jonah_catchE_rho_v, jonah_catch_theta_v, jonah_catch_theta_rho_v, jonah_wtE_v, jonah_wtE_rho_v, jonah_wt_theta_v, jonah_wt_theta_rho_v)
# areaList <- c(11, 12, 13, 14, 21, 22, 23, 24, 31, 32, 33, 34, 41, 42, 43, 44, 51, 52, 53, 54)
# library(corrplot)
# corrplot(cor(corDf), method = "circle", order = 'hclust', type="lower")
# testRes = cor.mtest(corDf, conf.level = 0.95)
# corrplot(cor(corDf), method = "circle", order = 'hclust', type="lower", diag=FALSE, p.mat = testRes$p, sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.9,
#          insig = 'label_sig', pch.col = 'grey20')

# reg_cols <- pivot_wider(RESULTS_ccm_by_reg_max, names_from = name, values_from = value) %>%
#        group_by(region) %>% select(-c(xmap, E, LibSize)) %>% rename(Region = region) %>% 
#        fill(everything(), .direction = "downup") %>% slice(1)


# both_seasons.R scatch work-----------------------------------------------------

# logCatch <- catchTidy_seasons %>% filter(Type == "logCatch")
# logWt <- catchTidy_seasons %>% filter(Type == "logWt") %>% 
#   mutate(date=paste(Year, case_when(Season== "Fall" ~ "-11-01",
#                                     Season =="Spring" ~"-05-01"), sep = ""))
# logWt %>% group_by(area, Species) %>% arrange(Year) %>% select(value) %>% 
#   group_map(~ts(., frequency = 2, start=c(2000, 2)))

catch_ts <- read.zoo(catchTidy_seasons_complete, index.column = "date")

# logWt_complete <- complete(data=logWt %>% ungroup(), date, Species, Region, Stratum, explicit = TRUE) %>% mutate(area = paste(Region, Stratum), Type="logWt") %>% select(date, Species, area, value)

# logWt_areas <- logWt_complete %>% group_by(area, Species) %>% arrange(date) %>% select(value) %>% 
#   group_map(~ts(., frequency = 2, start=c(2000, 2)))
# 
# logWt_areas2 <- logWt_complete %>% group_by(area) %>% arrange(date) %>% select(Species, value) %>% 
#   group_map(~ts(., frequency = 2, start=c(2000, 2)))

# 
# jonah_ts <- catch_complete_diff %>% group_by(area) %>% arrange(date) %>% select(avgLogWt_s) %>%
#   group_map(~ts(., frequency = 2, start=c(2000, 2)))# .keep = FALSE))


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

#out1<- catchCCMdf %>% filter(area == areaInput) %>% mutate(predator=predator,prey=prey,direction= paste("prey","->","predator")

# print(ccf(catchCCMdf %>% filter(area == 11) %>% pull(jonah), catchCCMdf %>% filter(area == 11) %>% 
#       pull(scallop), type="correlation"))

#ggplot(data=logCatch, aes(x=Year, y=value, color=Species)) +geom_line()+facet_grid(Region~Stratum)

test<-CCM(dataFrame=data.frame(catchCCMdf %>% filter(area == 11)), columns="jonah", target="scallop", E = 2, Tp=0, libSizes = "36 36 10", sample=100, verbose = FALSE, includeData = TRUE)

ggplot(data=test[["CCM1_Predictions"]][[1]] %>% mutate(index = row_number()), aes(x=index, y=Predictions))+geom_line() +geom_line(aes(x=index, y=Observations, color="red"))

##### CCM by area - tp0

RESULTS_ccm_combos_by_area_tp0 <- pmap_dfr(params_areas_ccm_combos,function(predator,prey, areaInput){
  
  df_temp <- catchCCMdf %>% filter(area == areaInput)
  lib_vec <- paste(1, nrow(df_temp))
  
  rho_E_1<- EmbedDimension(dataFrame = df_temp, lib = lib_vec, pred = lib_vec,
                           columns = predator,target = prey, maxE = 7, showPlot = FALSE)
  E_out_1<-rho_E_1[which.max(rho_E_1$rho),"E"][1]
  out_1 <- CCM(dataFrame= df_temp, columns=predator, target=prey, E = E_out_1, Tp=0,
               libSizes = paste(E_out_1+2, nrow(df_temp) - E_out_1, "1",sep=" "), sample=100, verbose=TRUE, showPlot = TRUE)  %>%
    mutate(predator=predator,
           prey=prey,
           direction= paste("predator","->","prey"),
           area = areaInput,
           E = E_out_1)
  
  rho_E_2<- EmbedDimension(dataFrame = df_temp, lib = lib_vec, pred = lib_vec,
                           columns = prey,target = predator, maxE = 7, showPlot = FALSE)
  E_out_2<-rho_E_2[which.max(rho_E_1$rho),"E"][1]
  out_2 <- CCM(dataFrame= df_temp, columns=prey, target=predator, E = E_out_2, Tp=0,
               libSizes = paste(E_out_2+2, nrow(df_temp)-E_out_2, "1",sep=" "), sample=100, verbose=TRUE, showPlot = TRUE)  %>%
    mutate(predator=predator,
           prey=prey,
           direction= paste("prey","->","predator"),
           area = areaInput,
           E= E_out_2)
  
  bind_rows(out_1,out_2)
})

regionsGrid_tp0<- left_join(regionsGrid, max_rho_by_area_tp0)

CCM(dataFrame=data.frame(catchCCMdf %>% filter(area == 11)), columns="jonah", target="scallop", E = 2, Tp=1, libSizes = "4 36 1", sample=100, verbose = TRUE, showPlot = TRUE)


summary(lm(as.formula(scallop ~ jonah),data=data.frame(catchCCMdf %>% filter(area == 11))))

test2<-Simplex(dataFrame=data.frame(catchCCMdf %>% filter(area == 11)), columns="jonah", target="scallop", E = 2, Tp=1, lib = "1 36", pred="1 36", showPlot = TRUE, parameterList = TRUE)
compute_stats(test2$predictions$Observations, test2$predictions$Predictions)

library(correlation)
cor_to_p(cor=0.2444023, n=35) #0.157
library(psych)
fisherz(rho=0.2444023) #z-score = 0.249?
r.con(rho=0.2444023, n=35, p=0.95) #95% CI for rho: -0.097 to 0.534

max_rho_by_area_tp0<- RESULTS_ccm_combos_by_area_tp0 %>% group_by(area) %>% summarise(
  max_J_S = max(`jonah:scallop`, na.rm = TRUE),
  max_S_J = max(`scallop:jonah`, na.rm = TRUE),
  max_R_S = max(`rock:scallop`, na.rm = TRUE),
  max_S_R = max(`scallop:rock`, na.rm = TRUE),
  max_J_R = max(`jonah:rock`, na.rm = TRUE),
  max_R_J = max(`rock:jonah`, na.rm = TRUE),
  max_lib = max(LibSize),
  min_lib = min(LibSize))

tibble(as.matrix(max_rho_by_area) - as.matrix(max_rho_by_area_tp0))

library(mgcv)
jonahGAM <- gam(rho ~ region + stratum, data=jonah_E_rho_long)
summary(jonahGAM)

ggplot(data=regionsGrid %>% filter(Species != "scallop"))+geom_sf(aes(fill=avg))+facet_wrap(~Species)+scale_fill_gradientn(colors = viridis_pal(option="F")(9),limits=c(0, 5))

ggplot(data=regionsGrid_seasons %>% filter(Species != "scallop"))+geom_sf(aes(fill=log(avg+1)))+facet_grid(Season~Species)+ scale_fill_gradientn(colors = viridis_pal(option="F")(9),limits=c(0, 2.5))

ggplot(data=regionsGrid_seasons %>% filter(Species == "jonah"))+geom_sf(aes(fill=log(avg+1)))+facet_wrap(~Season)+ scale_fill_gradientn(colors = viridis_pal(option="D")(9),limits=c(0, 2.1))

#And another approach to looking at differences between regions and between strata:
  
 # 8. Multispatial CCM by region, with strata within each region as replicates = run CCM on 5 different ~115-point time series
# 9. Multispatial CCM by stratum, with regional divisions within each depth stratum as replicates = run CCM on 4 different ~150-point time series

# Function 3: do_xmap with ID col------------------------------------------------

do_xmap_ID_failed <- function(df,predictor,target,ID_col,E_max=2 ,tp=1,keep_preds=FALSE){
  
  df_1 <- make_xmap_block_ID(df,!!sym(predictor),!!sym(target),!!sym(ID_col),
                             E_max,cause_lag=tp) %>% ungroup() %>% filter(complete.cases(.))
  #df_1 should have the following columns: time index, ID column, target, predictor, lagged predictor
  
  lib_vec <- paste(1,nrow(df_1))
  
  columns_star <- names(df_1)[4:(E_max+3)] 
  #return(columns_star)
  
  out <- Simplex(dataFrame=df_1,
                 lib = lib_vec, pred=lib_vec,
                 Tp=0, # The target has already been "manually" lagged
                 target="target",
                 columns=columns_star,
                 embedded=TRUE,
                 parameterList = TRUE,
                 E=E_max)
  
  fit_linear <- lm(as.formula(paste0("target ~ ",columns_star[1])),data=df_1)
  out_linear_predictions <- predict(fit_linear)
  
  params <- out$parameters
  out <- out$predictions %>% filter(complete.cases(.))
  
  stats <- compute_stats(out$Observations,out$Predictions)
  
  stats_linear <- compute_stats(out$Observations,out_linear_predictions)
  names(stats_linear) <- paste0(names(stats_linear),"_linear")
  
  return(bind_cols(Filter(function(x) length(x)==1,params),stats,stats_linear))
  
}

# The two calls will produce similar results, and the combined
# result will give us a better picture of strong interspecies
# relationships that are robust to slight misspecifications of the
# function.

#testing surrogate function
# library(tseries)
# x <- 1:10  # Simple example
# surrogate(x, ns=10, fft=TRUE, amplitude = TRUE)
# 
# 
# n <- 500  # Generate AR(1) process
# e <- rnorm(n)  
# x <- double(n)
# x[1] <- rnorm(1)
# for(i in 2:n) {
#   x[i] <- 0.4 * x[i-1] + e[i]
# }
# x <- ts(x)
# 
# theta_fun <- function(x) { # Autocorrelations up to lag 10
#   return(acf(x, plot=FALSE)$acf[2:11])
# }
# 
# y<-surrogate(x, ns=50, fft=TRUE, statistic=theta_fun) 

# , which will create surrogate data with the same spectrum as the input time series (x) by randomizing the phases of the Fourier coefficients of x, and we'll set "amplitude=TRUE" to preserve the amplitude distribution of the original series

ccm_agg_surr <- cbind(catchCCMdf_agg, surrogate(catchCCMdf_agg$jonah, ns=100)) %>% select(-jonah)

ccm_agg_surr <- rename_with(ccm_agg_surr,.cols=!any_of(c("date", "scallop", "rock")),~ paste0("jonah_", .x, recycle0 = TRUE))

params_ccm_combos_surr <- map_dfr(seq_len(100), ~params_ccm_combos)
params_ccm_combos_surr <- params_ccm_combos_surr %>% mutate(surr_trial = rep(1:100, each=2))

paste("jonah", 2, sep="_")



# find_max <- function(df) {
#   df_out <- df %>% summarise(
#     max_J_S = max(`jonah:scallop`, na.rm = TRUE),
#     max_S_J = max(`scallop:jonah`, na.rm = TRUE),
#     max_J_R = max(`jonah:rock`, na.rm = TRUE),
#     max_R_J = max(`rock:jonah`, na.rm = TRUE))
#   return(df_out)
# }
# 1-ecdf(null_dist(df = RESULTS_ccm_by_reg_surr %>% filter(region==1), s="jonah:scallop"))(0.166819876)
# 0.01*(sum(null_dist(df = RESULTS_ccm_by_reg_surr %>% filter(region==1), s="jonah:scallop") >= 0.166819876))

# max_rho_by_area<- RESULTS_ccm_by_area %>% group_by(area) %>% find_max()
# max_rho_wt_by_area<- RESULTS_ccm_wt_by_area %>% group_by(area) %>% find_max()
# 
# colSums(max_rho_by_area >0)
# colSums(max_rho_wt_by_area>0)

# We can see that for catchm max rho for J-\>S was \>0 for all area and
# max rho for J-\>R was \>0 for 18/20 areas. For weight, max rho for J-\>S
# was \>0 for 18/20 areas and max rho for J-\>R was \>0 for 19/20 areas.
# S-\>J was 6 and 7 and R-\>J was 10 and 8 for catch and weight,
# respectively.

# max_rho_by_reg<- RESULTS_ccm_by_reg %>% group_by(region) %>% find_max()
# max_rho_wt_by_reg<- RESULTS_ccm_wt_by_reg %>% group_by(region) %>% find_max()
# 
# max_rho_by_strat<- RESULTS_ccm_by_strat %>% group_by(stratum) %>% find_max()
# max_rho_wt_by_strat<- RESULTS_ccm_wt_by_strat %>% group_by(stratum) %>% find_max()

# Rho at max lib size - surr regions
RESULTS_ccm_by_reg_surr %>% 
  group_by(xmap, region) %>% 
  pivot_longer(cols=all_of(combos)) %>% 
  na.omit() %>% 
  filter(substr(xmap, 1, 1)==substr(name, 1, 1))

# If we look back at the max rho by region, we can see that we only really need to test jonah:scallop (all regions), jonah:rock (all regions), and rock:jonah for region 1 only.
# #find_p(1,"jonah:scallop")

#test %>% rowwise() %>% mutate(p = find_p(Var1, Var2))

quantile(null_dist(df = RESULTS_ccm_wt_by_reg_surr %>% filter(region==2), s="scallop:jonah"), .95)

1-ecdf(null_dist(df = RESULTS_ccm_wt_by_reg_surr %>% filter(region==1), s="scallop:jonah"))(0.219) #p=0


map(1:4, function(x) {
  ndist = null_dist(df = RESULTS_ccm_by_strat_surr %>% filter(stratum==x), s="jonah:rock")
  prob = 1-ecdf(ndist)(max_rho_by_strat[x,"max_J_R"])
  return(prob)
}) #strat 4 is sig, p=0.03

map(1:4, function(x) {
  ndist = null_dist(df = RESULTS_ccm_by_strat_surr %>% filter(stratum==x), s="scallop:jonah")
  prob = 1-ecdf(ndist)(max_rho_by_strat[x,"max_S_J"])
  return(prob)
}) #strats 3 and 4 are sig, p=0 and p=0.01

map(1:4, function(x) {
  ndist = null_dist(df = RESULTS_ccm_by_strat_surr %>% filter(stratum==x), s="rock:jonah")
  prob = 1-ecdf(ndist)(max_rho_by_strat[x,"max_R_J"])
  return(prob)
}) #strat 1 is sig, p=0.02


# Sex ratio testing old-------------------------------------------------------
# library(lme4)
# library(lmerTest)
 #detach("package:lmerTest", unload = TRUE)
#detach("package:lme4", unload = TRUE)
#  detach("package:nlme", unload = TRUE)
# library(nlme)

sex_diff_old <- pivot_wider(data_complete, names_from="Season", values_from = "perc_m") %>% 
  mutate(Diff = Fall-Spring) %>% na.omit() %>% 
  group_by(Year, Stratum) %>% 
  summarise(Diff = mean(Diff)) %>% 
  filter(Year > 2004)

mod2<- lm(Diff ~ Stratum + Region, data = sex_diff_reg)
mod2
summary(mod2)
shapiro.test(residuals(mod2))

sex_diff_reg %>%
  group_by(Stratum, Region) %>%
  shapiro_test(Diff) 

sex_diff_reg %>% 
  group_by(Stratum, Region) %>% 
  identify_outliers(Diff) 

plot(fitted(mod3), resid(mod3), xlab='Fitted Values', ylab='Residuals')

sked.dat = sex_diff %>% ungroup() %>% 
  mutate("abs.resids" = abs(residuals(mod3)))

sked.resid.mod = lm(abs.resids~Stratum, data=sked.dat)

ggplot(sked.dat) +
  geom_point(aes(x=Stratum, y=abs.resids)) +
  geom_abline(aes(intercept=coef(sked.resid.mod)[1],
                  slope=coef(sked.resid.mod)[2]))

bartlett.test(Diff ~ Stratum, data=sex_diff)
# sex_diff %>% ungroup() %>% levene_test(Diff ~ Stratum)
# leveneTest(Diff ~ Stratum, data=sex_diff)
# leveneTest(Diff ~ Stratum, data=sex_diff)

# Basic ANOVA - only strat
summary(aov(Diff ~ Stratum, data=sex_diff))
summary(aov(Diff ~ Year, data=sex_diff))
summary(aov(Diff ~ Year + Stratum, data=sex_diff))
summary(aov(Diff ~ Year + Stratum + Region, data=sex_diff_reg))

mod3gls <- gls(Diff~Stratum,weights=varPower(), data=sex_diff)
summary(mod3gls)
shapiro.test(residuals(mod3gls))

# vf3 <- varPower(form =~ Stratum)
# M.gls3 <- gls(Diff ~ Stratum,
#               weights = vf3,data=sked.dat)
# shapiro.test(residuals(M.gls3))
# summary(M.gls3)
# vf5 <- varExp(form =~ Stratum)
# M.gls5 <- gls(Diff ~ Stratum,
#               weights = vf5, data = sex_diff)
# vf6<-varConstPower(form =~ Stratum)
# M.gls6<-gls(Diff ~ Stratum,
#             weights = vf6, data = sex_diff)
anova(M.lm, mod3gls,M.gls3,M.gls5,M.gls6)
# acf(residuals(mod3gls))
# durbinWatsonTest(mod3gls)

w <- sked.dat$Stratum
wls<- lm(Diff ~ Stratum, weights=w, data=sked.dat)
summary(wls)

shapiro.test(wls$residuals)
ncvTest(wls)
## LMER models with package lme4 and lmerTest
lmer1<- lmer(Diff ~ (1|Year) + Stratum, data=sex_diff)
summary(lmer1)
shapiro.test(residuals(lmer1)) #good
ranova(lmer1)
#lmer_w <- lmer(Diff ~ (1|Year) + Stratum, data=sex_diff, weights=w)

anova(lmer1, wls2, lmer_w)
anova(lmer2, lmer2a, lmer2b, lmer3)
ranova(lmer3)

M.lm <-gls(Diff~Stratum, data=sked.dat)

summary(gls(Diff~Stratum,weights=varPower(), data=sked.dat))

fit.ar1 <- gls(Diff ~ factor(Stratum), data=sex_diff, corr=corAR1())

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

ggplot(data=data_complete_geom %>% filter(Season=="Fall"))+geom_sf(aes(fill=perc_m))+facet_wrap(~Year)
ggplot(data=data_complete_geom %>% filter(Season=="Spring"))+geom_sf(aes(fill=perc_m))+facet_wrap(~Year)
ggplot(data=sex_diff_geom)+geom_sf(aes(fill=Diff))+facet_wrap(~Year)

with (sex_diff, {interaction.plot(Year, factor(Stratum), Diff, ylab="mean of Diff", xlab="time", trace.label="Stratum") })

# Unweighted linear models / Stratum analysis ------------------------------------------------

# regionsGrid_seasons <- left_join(regionsGrid_orig %>% mutate(Stratum = as.factor(Stratum)), catchTidy_strat_complete %>% filter(Type=="catch"))
# 
# ggplot(data=regionsGrid_seasons %>% group_by(Stratum, Species, Season) %>% summarize(avg = mean(value, na.rm=TRUE)))+geom_sf(aes(fill=avg))+facet_grid(Season~Species)+scale_fill_viridis_c(option = "F", name="avg catch")
# 
# lag2 <- function(x) {
#   x_lagged <- (x - lag(x, 2))
#   return(x_lagged)
# } 
# catch_complete_diff <- catch_strat_complete %>% arrange(date) %>% group_by(Stratum) %>%
#   mutate(across(where(is.double) & !date, lag2)) %>% 
#   filter(date != "2003-11-01" & date != "2004-05-01") %>%  filter(as.Date(date) < as.Date("2020-05-01"))
# 
# complete_tidy_diff <- pivot_longer(catch_complete_diff,cols =starts_with("avg")) %>% 
#   mutate(Type = case_when(
#     startsWith(name, "avgCatch_") ~"catch",
#     startsWith(name,"avgWt_") ~"wt")) %>% 
#   mutate(Species = case_when(
#     endsWith(name, "s") ~"scallop",
#     endsWith(name, "r") ~"rock",
#     endsWith(name, "j") ~"jonah")) %>%
#   mutate(Species = as.factor(Species),
#          Type = as.factor(Type),
#          Stratum = as.factor(Stratum)) %>% 
#   select(-name)  
# 
# params_ccm_combos <- data.frame(jonah=c("jonah", "jonah"), prey=c("scallop", "rock"))
# 
# catch_ccm <- catch_complete_diff %>% select(-c(Season, avgWt_s, avgWt_j, avgWt_r)) %>% rename("jonah" = "avgCatch_j", "rock"="avgCatch_r", "scallop"="avgCatch_s")
# 
# v_keep_col <- c("E","Tp","num_pred", "rho", "mae","rmse","perc","p_val","rho_linear", "mae_linear",
#                 "rmse_linear","perc_linear","p_val_linear")
# 
# # Strata as replicates
# RESULTS_ccm_rep_strata <- pmap_dfr(params_ccm_combos,function(jonah,prey){
#   
#   out_1 <- do_xmap_ID(catch_ccm,predictor=jonah,target=prey,ID_col="Stratum",E=3, tp=1)  %>%
#     mutate(direction= paste("jonah","->",prey))
#   
#   out_2 <- do_xmap_ID(catch_ccm,predictor=prey,target=jonah,ID_col="Stratum",E=3, tp=1) %>% 
#     mutate(direction= paste(prey,"->","jonah"))
#   
#   bind_rows(out_1,out_2) %>% select(direction, all_of(v_keep_col))  %>% mutate(type="catch", replicate = "stratum")




# ets1 <- ets(window(ts1[,"value"], 2001.0, 2015.5))
# #    lamba="auto")
# summary(ets1)
# plot(ets1)
# accuracy(ets1)
# 
# ets1 %>% forecast(h=14) %>% autoplot()
# obs <- tsdf %>% slice(31:44) %>% pull(value)
# compute_stats(ets1$mean, obs)

mod1b<- lm(Diff ~ Stratum, data = sex_diff %>% mutate(Stratum = as.factor(Stratum)))
mod1b
summary(mod1b)
durbinWatsonTest(mod1b, max.lag=10) #OK
ncvTest(mod2) #homogeneity of residual variance not met

mod3<- lm(Diff ~ Year + Stratum, data = sex_diff)
summary(mod3)
shapiro.test(residuals(mod3))
durbinWatsonTest(mod3, max.lag = 10) #OK
ncvTest(mod3) #homogeneity of residual variance not met


###### Welch & Kruskal-Wallis one-way ANOVAs
#robust to violations of equal variance
welch<- welch_anova_test(data=sex_diff, Diff ~ Stratum)
games_howell_test(data=sex_diff, Diff ~ Stratum) #post-hoc for Welch
kruskal.test(data=sex_diff, Diff ~ Stratum)

data_complete <- data_complete %>%
  group_by(Season, Year, Region, Stratum) %>% 
  summarise(perc_m = mean(perc_m, na.rm=TRUE)) %>% 
  mutate(Area = as.numeric(paste0(Region, Stratum)))

#remove outlier
sex_diff <- sex_diff %>%
  filter(!(Year==2019 & Stratum==3))%>%
  arrange(Stratum)

wls2<- lm(Diff ~ Stratum, weights=w, data=sex_diff)
summary(wls2)
shapiro.test(wls2$residuals) #Good
extractAIC(wls2)
acf(wls2$residuals) #Good
durbinWatsonTest(wls2, max.lag=10) #Good
ncvTest(wls2) #Good

w <- sex_diff$Stratum
wls<- lm(Diff ~ Stratum + Year, weights=w, data=sex_diff)
summary(wls)
shapiro.test(wls$residuals) #Good
extractAIC(wls)
acf(wls$residuals) #Good
durbinWatsonTest(wls, max.lag=10) #Good
ncvTest(wls) #Good

surveyGrid$area <- as.integer(paste0(surveyGrid$Region, surveyGrid$Stratum))

df_j_len <- df_j_len %>% mutate(Common_Name = "Jonah crab") #note shell height is in cm

# Weighted least-squares linear regression --------------------------------

# Construct weighting vector equivalent to stratum number
w <- sex_diff$Stratum 
# Weighted least squares linear regression
wls<- lm(Diff ~ Stratum, weights=w, data=sex_diff)
summary(wls)
# Visualize and test normality of residuals
plot(wls, 2)
shapiro.test(wls$residuals)
# Visualize and test autocorrelation of residuals
acf(wls$residuals)
durbinWatsonTest(wls, max.lag=15) #none have p<0.05
# Visualize and test homogeneity of residual variance
plot(wls, 1)
ncvTest(wls)
bptest(wls, varformula = ~ fitted.values(wls), studentize=F)
coeftest(wls, vcov = vcovHC(wls, type = 'HC0'))
# Test with Year as explanatory variable - non-significant
wls_year<- lm(Diff ~ Stratum + Year, weights=w, data=sex_diff)
summary(wls_year)

# Regions -----------------------------------------------------------------


ggplot(data=sex_diff_reg, aes(x=Year, y=Diff))+geom_line()+facet_grid(Region~Stratum)+geom_smooth(method = "lm", se=FALSE)

ggplot(data=sex_diff_reg, aes(x=Stratum, y=Diff, group=Stratum))+geom_boxplot()+facet_wrap(~Region)+
  geom_jitter(size=0.5) +
  theme_bw()

ggplot(data=sex_diff_reg, aes(x=Year, y=Diff, group=Stratum, color=Stratum))+geom_point()+geom_smooth(method = "lm", se=FALSE)+facet_wrap(~Region)

mod4 <- lm(Diff ~ Stratum + Region, data=sex_diff_reg)
mod4
summary(mod4)
extractAIC(mod4)
shapiro.test(residuals(mod4))
durbinWatsonTest(mod4, max.lag=15) #not good
acf(mod4$residuals)
par(mfrow = c(2, 2))
plot(mod4)

library(lme4)

mod5 <- gls(data=sex_diff_reg, Diff~ Region + Stratum + Year, 
            correlation= corAR1(form = ~ Year | (Stratum/Region)))
ranova(mod5)
summary(mod5)

acf(residuals(mod5))


durbinWatsonTest(mod5)
Box.test(residuals(mod5), type="L")

summary(p1)
coef(p1,round=TRUE)

# reg_ts <- sex_diff_reg %>% complete(Region, Stratum, Year) #%>% pivot_wider(names_from=c(Region, Stratum), values_from = Diff)
# 
# reg_ts <- as_tsibble(reg_ts, index=Year, key=c(Region, Stratum)) %>% fill_gaps()
# reg_ts %>% autoplot()
# library(tsibble)
# library(fable)
# library(feasts)
# reg_ts %>% model(TSLM(Diff ~ Stratum))
# 
# # summary(mod5)
# 
# fit <- reg_ts %>%
#   model(
#     arima = ARIMA(Diff)
#   )
# fit
# fit %>%
#   select(Region, Stratum, arima) %>%
#   coef()
w2 <- sex_diff_reg$Stratum*sex_diff_reg$Region
mod6 <- lm(Diff ~ Stratum + Year + Region, data=sex_diff_reg,weights=w2)
mod6
summary(mod6)


durbinWatsonTest(m1, max.lag=15) #not good
acf(residuals(m1))
par(mfrow = c(2, 2))
plot(m1)

p1 <- powerTransform(data=sex_diff_reg, Diff~ (1|Region) + Stratum + Year, family="bcnPower")
sex_diff_reg$tDiff <- bcnPower(sex_diff_reg$Diff, lambda=p1$lambda, gamma=p1$gamma)
m1 <- lmer(tDiff ~ (1|Region) + Stratum + Year, data=sex_diff_reg)
summary(m1)
shapiro.test(residuals(m1))
Box.test(residuals(m1))
acf(residuals(m1))


# p2 <- powerTransform(data=sex_diff_reg, Diff~ Region + Stratum + Year, family="bcnPower")
# sex_diff_reg$tDiff2 <- bcnPower(sex_diff_reg$Diff, lambda=p2$lambda, gamma=p2$gamma)
# m2 <- lm(tDiff2 ~ Region + Stratum + Year, data=sex_diff_reg)
# summary(m2)
# shapiro.test(residuals(m2))
# Box.test(residuals(m2))
# acf(residuals(m3))

# p2 <- powerTransform(data=sex_diff_reg, Diff~ (1|Region) + Stratum, family="bcnPower")
# sex_diff_reg$tDiff2 <- bcnPower(sex_diff_reg$Diff, lambda=p2$lambda, gamma=p2$gamma)
# m2 <- lmer(tDiff2 ~ (1|Region) + Stratum, data=sex_diff_reg)
# summary(m2)
# shapiro.test(residuals(m2))
# Box.test(residuals(m2))

#  m3 <- gls(data=sex_diff_reg, tDiff~ Region + Stratum + Year, 
#      correlation= corAR1(form = ~ Year | (Stratum/Region)))
# # summary(m3)
# # shapiro.test(residuals(m3))
#  Box.test(residuals(m3))


# All figures -------------------------------------------------------------
map_reg_rho <- left_join(surveyGrid %>% group_by(Region) %>% summarise(num = n_distinct(GridID)), RESULTS_ccm_by_reg_max %>% rename(Region=region))

(ggplot(data=map_reg_rho)+geom_sf(aes(fill=value))+scale_fill_viridis_c(option="F"))+facet_wrap(~name)

(ggplot(data=left_join(surveyGrid %>% group_by(Region) %>% summarise(num = n_distinct(GridID)), RESULTS_ccm_wt_by_reg_max %>% rename(Region=region)))+geom_sf(aes(fill=value))+scale_fill_viridis_c(option="F"))+facet_wrap(~name)

ggplot(data=left_join(surveyGrid %>% group_by(Stratum) %>% summarise(num = n_distinct(GridID)), RESULTS_ccm_by_strat_max %>% rename(Stratum=stratum)))+geom_sf(aes(fill=value))+facet_wrap(~name)

ggplot(data=left_join(surveyGrid %>% group_by(Stratum) %>% summarise(num = n_distinct(GridID)), RESULTS_ccm_wt_by_strat_max %>% rename(Stratum=stratum)))+geom_sf(aes(fill=value))+facet_wrap(~name)


ggplot(data=left_join(surveyGrid %>% group_by(area) %>% summarise(num = n_distinct(GridID)), RESULTS_ccm_by_area_max %>% rename(area=areaInput)))+geom_sf(aes(fill=value))+facet_wrap(~name) #+scale_fill_viridis_c(option="F")

ggplot(data=left_join(surveyGrid %>% group_by(area) %>% summarise(num = n_distinct(GridID)), RESULTS_ccm_wt_by_area_max %>% rename(area=areaInput)))+geom_sf(aes(fill=value))+facet_wrap(~name)


ggplot(data = catchTidy_strat %>% filter(Type=="catch", Species=="jonah") %>% group_by(Year, Season) %>% summarise(value = mean(value, na.rm=TRUE)))+geom_line(aes(x=Year, y=value))+facet_wrap(~Season)+theme_classic()+labs(y="Abundance (catch/tow)")

ggplot(data = catchTidy_strat_complete %>%
         filter(Type == "catch", Species != "scallop") %>%
         group_by(date, Species) %>%
         summarise(avg = mean(value, na.rm = TRUE)), aes(x=as.Date(date), y=avg))+
  geom_line()+
  facet_wrap(~Species)+
  labs(y="catch")+
  theme_classic()


ggplot(data = catchTidy_strat_complete %>%
         filter(Type == "catch", Species != "scallop") %>%
         group_by(Year, Season, Species) %>%
         summarise(avg = mean(value, na.rm = TRUE)), aes(x=Year, y=avg))+
  geom_line()+
  facet_grid(Season~Species)+
  labs(y="catch")+
  theme_classic()

#All areas on one graph, split by species
ggplot(data = complete_tidy_diff %>%
         filter(Type == "catch", Species != "scallop"), aes(x=date, y=value, group=Stratum,color=Stratum))+geom_line()+
  facet_wrap(~Species) +
  labs(y="2nd-differenced catch", x="Year")

#Colored by species, split by area
ggplot(data = complete_tidy_diff %>% filter(Type == "catch", Species != "scallop"),
       aes(x=date, y=value, group=Species,color=Species))+
  geom_line()+
  facet_wrap(~Stratum)+
  labs(x="Depth stratum")+
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))


# catchTidy_strat_complete_j2 <- catchTidy_strat_complete_j %>% 
#   mutate(temp = na.spline(temp),value = na.spline(value)) %>% 
#   ungroup()#%>% 
# mutate(temp = round(temp, digits=7),value = round(value, digits=7))
# 
# all.equal(catchTidy_strat_complete_j1, catchTidy_strat_complete_j2)
# all.equal(catchTidy_strat_complete_j, catchTidy_strat_complete_j1)
# dplyr::setdiff(catchTidy_strat_complete_j1[,1:8], catchTidy_strat_complete_j2[,1:8])
# dplyr::setdiff(catchTidy_strat_complete_j2, catchTidy_strat_complete_j1)
# anti_join(catchTidy_strat_complete_j1, catchTidy_strat_complete_j2)
# anti_join(catchTidy_strat_complete_j1, catchTidy_strat_complete_j2, by="temp")
# 
# catchTidy_strat_complete_j1$included_a1 <- TRUE
# catchTidy_strat_complete_j2$included_a2 <- TRUE
# res <- merge(catchTidy_strat_complete_j1, catchTidy_strat_complete_j2, all=TRUE)

# # Testing imputed data ----------------------------------------------------
# fixCOVID <- function(df) {
#   df_out <- df %>% mutate(across(starts_with("avg"), na.approx))
# }
# 
# catch_complete_imp <- catch_complete %>% arrange(across(c("area", "Season", "Year"))) %>% 
#   fixCOVID()
# 
# catch_complete_diff_imp <- catch_complete_imp %>% arrange(date) %>% group_by(area) %>% 
#   mutate(across(where(is.double) & !date, lag2)) %>% 
#   arrange(area) %>% 
#   filter(date != "2000-11-01" & date != "2001-05-01")
# 
# #Tidy it up
# complete_tidy_diff_imp <- pivot_longer(catch_complete_diff_imp,cols = starts_with("avg")) %>% 
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
# 
# #All areas on one graph, split by species
# ggplot(data = complete_tidy_diff_imp %>% 
#          filter(Type == "catch", Species != "scallop"), aes(x=date, y=value, color=area))+geom_line()+facet_wrap(~Species) +labs(y="2nd-differenced catch", x="Year")
# 
# #Averaged across areas, split by species
# ggplot(data = complete_tidy_diff_imp %>% filter(Type == "catch", Species != "scallop") %>% group_by(date, Species) %>% 
#          summarise(avg = mean(value, na.rm = TRUE)), aes(x=date, y=avg))+geom_line()+facet_wrap(~Species)+theme_classic()+labs(y="2nd-differenced catch", x="Year")
# 
# #Colored by species, split by area
# ggplot(data = complete_tidy_diff_imp %>% filter(Type == "catch", Species != "scallop"), aes(x=date, y=value, color=Species))+geom_line()+facet_grid(Region~Stratum)+labs(x="Depth stratum", y="Region")+theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))
# 
# EmbedDimension(dataFrame=complete_tidy_diff_imp %>% filter(Species=="jonah", Type=="wt") %>% group_by(date) %>% 
#                  summarise(avg = mean(value, na.rm = TRUE)) %>% 
#                  ungroup() %>% select(date, avg),  columns ="avg", target="avg", lib = "1 43", pred="1 43")

# rm(complete_tidy_diff_imp)
# rm(catch_complete_diff_imp)
# rm(catch_complete_imp)


# EDM vs. ARIMA (na.spline) -----------------------------------------------

#ts4<- na.spline(ts(catchTidy_agg_complete2 %>% filter(Species=="jonah", Type=="catch") %>% 
#   mutate(date=as.Date(date)) %>% arrange(date), frequency = 2, start=c(2001, 1)))

#ts_val4 <- ts4[,c("Year", "value", "date")]
#tsdf4 <- data.frame(ts_val4)
# ggplot(data=tsdf4)+geom_line(aes(x=date, y=value))
# 
# fit_train4 <- auto.arima(ts_val4[train,"value"])
# checkresiduals(fit_train4)
# 
# forecast(fit_train4, h=14)
# arima_preds4 <- data.frame(forecast(fit_train4, h=14)) %>% mutate(yrs = yrs)
# 
#EmbedDimension(dataFrame=tsdf4, columns="value", target="value", lib = "1 30", pred="31 44") #%>% filter(rho == max(rho))
#PredictNonlinear(dataFrame=tsdf4, columns="value", target="value", lib = "1 30", pred="31 44", E=8)
# PredictNonlinear(dataFrame=tsdf4, columns="value", target="value", lib = "1 30", pred="31 44", E=3, 
#                  theta="0.85 0.9 0.95 1 1.05 1.1") %>% filter(rho == max(rho))
# 
# s_out4 <- SMap(dataFrame=tsdf4, columns="value", target="value", lib = "1 30", pred="31 44", E=3, theta=1.05)
# 
# compute_stats(s_out4$predictions$Observations, s_out4$predictions$Predictions)
# plot(x=s_out4$predictions$Observations, y=s_out4$predictions$Predictions)
# 
# (tsdf4[31:44, "value"] == s_out4$predictions$Observations[1:14]) #sanity check
# 
# compute_stats(s_out4$predictions$Observations[1:14], arima_preds4$Point.Forecast)
# compute_stats(s_out4$predictions$Observations, s_out4$predictions$Predictions)
# 
# arimaPlot_train4 <- autoplot(forecast(fit_train4, h=14)) + 
#   geom_path(data = data.frame(ts_val), aes(x = c(1:44), y = value)) + 
#   ylab("Catch")
# arimaPlot_train4
# 
# edm_df4 <- data.frame(obs = s_out4$predictions$Observations[1:14], 
#                       preds = s_out4$predictions$Predictions[2:15], 
#                       pred_var = s_out4$predictions$Pred_Variance[2:15])
# 
# yrs <- seq(2016, 2022.5, 0.5)
# ind <- index(ts_val)
# 
# edm_df4 <- edm_df4 %>% mutate(Lo.95 = preds - 1.96*sqrt(pred_var),
#                             Hi.95 = preds + 1.96*sqrt(pred_var),
#                             Lo.80 = preds - 1.28*sqrt(pred_var),
#                             Hi.80 = preds + 1.28*sqrt(pred_var),
#                             yrs = yrs)
# 
# edmPlot_manual4 <- ggplot()+
#   geom_path(data = data.frame(ts_val4), aes(x = ind, y = value)) + 
#   geom_path(data = edm_df4, aes(x=yrs, y=preds,color="line"))+
#   geom_ribbon(data = edm_df4, aes(x = yrs, y =preds, ymin = Lo.95, ymax = Hi.95), fill = "blue", alpha = 0.2) +
#   labs(x="Year", y="Avg. catch")+
#   scale_color_manual(values=c("blue"))+ylim(c(-8, 25))+theme_classic()
# edmPlot_manual4
# 
# arimaPlot_manual4 <- ggplot()+
#   geom_path(data = data.frame(ts_val4), aes(x = ind, y = value)) + 
#   geom_path(data = arima_preds4, aes(x=yrs, y=Point.Forecast,color="line"))+
#   geom_ribbon(data = arima_preds4, aes(x = yrs, y = Point.Forecast, ymin = Lo.95, ymax = Hi.95), fill = "blue", alpha = 0.2) +
#   labs(x="Year", y="")+
#   scale_color_manual(values=c("blue"))+ylim(c(-8, 25))+theme_classic()
# arimaPlot_manual4
# 
# edmPlot_manual4+arimaPlot_manual4 +
#   edmPlot_manual4+arimaPlot_manual4 +
#   plot_layout(guides = 'collect')+
#   plot_annotation(tag_levels = 'A')
# 
# edm_ARIMAplot4 <-ggplot()+
#   geom_path(data = data.frame(ts_val4), aes(x = ind, y = value)) + 
#   geom_path(data = edm_df4, aes(x=yrs, y=preds, color="EDM"))+
#   geom_path(data=arima_preds4, aes(x=yrs, y=Point.Forecast, color="ARIMA"))+
#   labs(y="Avg. catch", x="Year") +theme_classic()

ts3 <- ts(catchTidy_agg_complete %>% filter(Species=="jonah", Type=="catch") %>% mutate(date=as.Date(date)) %>% arrange(date), frequency = 2, start=c(2001, 1))
class(ts3)
ts3
index(ts3)
ts3<- na.spline(ts3)
autoplot(ts3[,3:4])

# fit <- auto.arima(ts3[,"value"],xreg=ts3[,"temp"])

ts3[,4] %>% diff() %>% ggtsdisplay(main="")
ts3[,4] %>% ggtsdisplay(main="")

ts_val <- ts3[,c("Season", "Year", "value", "date")]

fit <- auto.arima(ts_val[,"value"])
summary(fit)
checkresiduals(fit)
arimaPlot <- autoplot(forecast(fit))

tsdf <- data.frame(ts_val)
ggplot(data=tsdf)+geom_line(aes(x=date, y=value))

theta_seq <- seq(0.01, 0.75, by=0.05)
theta_seq <- paste(theta_seq, collapse=" ")

PredictNonlinear(dataFrame=tsdf, columns="value", target="value", lib = "1 44", pred="1 44", E=3)
PredictNonlinear(dataFrame=tsdf, columns="value", target="value", lib = "1 44", pred="1 44", E=3, theta=theta_seq)
PredictNonlinear(dataFrame=tsdf, columns="value", target="value", lib = "1 44", pred="1 44", E=3, theta="0.16 0.17 0.18 0.19 0.2 0.21 0.22 0.23 0.24 0.25 0.26 0.27 0.28") %>% filter(rho == max(rho))

s_out <- SMap(dataFrame=tsdf, columns="value", target="value", lib = "1 44", pred="1 44", E=3, theta=0.23)
compute_stats(s_out$predictions$Observations, s_out$predictions$Predictions)
plot(x=s_out$predictions$Observations, y=s_out$predictions$Predictions)

train <- 1:30
test <- 31:44

fit_train <- auto.arima(ts_val[train,"value"])
summary(fit_train)
checkresiduals(fit_train)

EmbedDimension(dataFrame=tsdf, columns="value", target="value", lib = "1 30", pred="31 44") %>% filter(rho == max(rho))
PredictNonlinear(dataFrame=tsdf, columns="value", target="value", lib = "1 30", pred="31 44", E=4)
PredictNonlinear(dataFrame=tsdf, columns="value", target="value", lib = "1 30", pred="31 44", E=4, theta="0.3 0.4 0.5 0.55 0.6 0.65 0.7") %>% filter(rho == max(rho))

s_out <- SMap(dataFrame=tsdf, columns="value", target="value", lib = "1 30", pred="31 44", E=3, theta=0.6)

plot(x=s_out$predictions$Observations, y=s_out$predictions$Predictions)

(tsdf[31:44, "value"] == s_out$predictions$Observations[1:14]) #sanity check

yrs <- seq(2016, 2022.5, 0.5)
ind <- index(ts_val)

forecast(fit_train, h=14)
arima_preds <- data.frame(forecast(fit_train, h=14)) %>% mutate(yrs = yrs)

compute_stats(s_out$predictions$Observations[1:14], arima_preds$Point.Forecast)
compute_stats(s_out$predictions$Observations, s_out$predictions$Predictions)

arimaPlot_train <- autoplot(forecast(fit_train, h=14)) + 
  geom_path(data = data.frame(ts_val), aes(x = c(1:44), y = value)) + 
  ylab("Catch")
arimaPlot_train

edm_df <- data.frame(obs = s_out$predictions$Observations[1:14], preds = s_out$predictions$Predictions[2:15], pred_var = s_out$predictions$Pred_Variance[2:15])

arimaPlot_manual <- ggplot()+
  geom_path(data = data.frame(ts_val), aes(x = ind, y = value)) + 
  geom_path(data = arima_preds, aes(x=yrs, y=Point.Forecast,color="line"))+
  geom_ribbon(data = arima_preds, aes(x = yrs, y = Point.Forecast, ymin = Lo.95, ymax = Hi.95), fill = "blue", alpha = 0.2) +
  geom_ribbon(data = arima_preds, aes(x = yrs, y = Point.Forecast, ymin = Lo.80, ymax = Hi.80), fill = "blue", alpha = 0.4) +
  ylab("Catch")+scale_color_manual(values=c("blue"))
arimaPlot_manual