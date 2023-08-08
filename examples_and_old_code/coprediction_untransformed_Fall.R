# #Coprediction Analysis - NO LOG TRANSFORM
# #Ruby Krasnow
# #Last modified: July 17, 2023
# 
# #Load packages
# library(tidyverse)
# library(lubridate) #date formatting
# library(patchwork) #combining plots
# # library(tseries) #for KPSS test for stationarity
# library(rEDM) #EDM
# 
# #spatial packages
# # library(sf)
# # library(sfheaders)
# # library(spdep)
# 
# #import help functions and data
# source('~/Downloads/lab_notebook/Maine/helpFunctionsMaine.R')
# df_tows<-read.csv("data/Maine_inshore_trawl/MEtows.csv") #tow data
# df_s_cat<- read.csv("data/Maine_inshore_trawl/MEscallopCatch.csv") #scallop catch
# df_r_cat<- read.csv("data/Maine_inshore_trawl/MErockCatch.csv") #rock crab catch
# df_j_cat<- read.csv("data/Maine_inshore_trawl/MEjonahCatch.csv") #jonah crab catch
# 
# s_cat_clean <- cleanCatch(df_s_cat) %>% 
#   mutate(Common_Name = "Scallop") %>%
#   filter(Season == "Fall")
# 
# r_cat_clean <- cleanCatch(df_r_cat) %>% 
#   mutate(Common_Name = "Rock") %>% 
#   filter(Season == "Fall")
# 
# j_cat_clean <- cleanCatch(df_j_cat) %>% 
#   mutate(Common_Name = "Jonah") %>% 
#   filter(Season == "Fall")
# 
# #Reorder columns
# colOrder<-c("area", "Survey", "Tow_Number", "Region", "Stratum", "logCatch", "logWt", "Expanded_Catch", 
#             "Expanded_Weight_kg", "Date", "Common_Name", "Number_Caught", "Weight_kg",
#             "Start_Latitude", "Start_Longitude","Season",
#             "Year","Grid", "Start_Depth_fathoms", "End_Depth_fathoms",
#             "Bottom_WaterTemp_DegC", "Bottom_Salinity")
# 
# j_cat_clean <- j_cat_clean %>% select(all_of(colOrder))
# r_cat_clean <- r_cat_clean %>% select(all_of(colOrder))
# s_cat_clean <- s_cat_clean %>% select(all_of(colOrder))
# 
# #computes averages for each study area (area = region-stratum combination)
# j_cat_sum <- summaryCatch(j_cat_clean)
# r_cat_sum <- summaryCatch(r_cat_clean)
# s_cat_sum <- summaryCatch(s_cat_clean)
# 
# catch <- s_cat_sum %>% left_join(j_cat_sum, by=c("area", "Season", "Region", "Stratum", "Year"), suffix = c("_s", "_j"))
# catch <- catch %>% left_join(r_cat_sum, by=c("area", "Season", "Region", "Stratum", "Year")) %>% 
#   mutate(avgCatch_r = avgCatch,avgWt_r = avgWt,avgCatch_r = avgCatch,avgWt_r = avgWt, .keep="unused")
# 
# catchTidy <- pivot_longer(catch, 
#                           cols = 6:ncol(catch)) %>% 
#   mutate(Type = case_when(
#     startsWith(name, "avgCatch_") ~"catch",
#     startsWith(name,"avgWt_") ~"wt",
#     startsWith(name,"avgLogWt") ~"logWt",
#     startsWith(name,"avgLogCatch") ~"logCatch")) %>% 
#   mutate(Species = case_when(
#     endsWith(name, "s") ~"scallop",
#     endsWith(name, "r") ~"rock",
#     endsWith(name, "j") ~"jonah"))
# 
# catchTidy <- catchTidy %>% mutate(area = as.factor(area), Species = as.factor(Species),Season = as.factor(Season),Region = as.factor(Region), Stratum = as.factor(Stratum)) %>% 
#   select(-name)
# 
# catchFall <- catchTidy %>% filter(Type == "catch") %>% filter(Season == "Fall")
# wtFall <- catchTidy %>% filter(Type == "wt") %>% filter(Season == "Fall")
# 
# #averaged over everything
# do_xmap_noID(df=catchFall %>% filter(Species=="scallop") %>% group_by(Year) %>% 
#                summarise(avg = mean(value)) %>% 
#                ungroup() %>% select(Year, avg),
#              predictor="avg", target="avg", E_max=7, tp=1)
# 
# do_xmap_noID(df=wtFall %>% filter(Species=="scallop") %>% group_by(Year) %>% 
#                summarise(avg = mean(value)) %>% 
#                ungroup() %>% select(Year, avg),
#              predictor="avg", target="avg", E_max=7, tp=1) #rho higher than rho_linear
# 
# 
# # Areas -------------------------------------------------------------------
# 
# #Scallops
# #Catch
# findSpeciesE((catchTidy %>% filter(Species=="scallop")), season="Fall", type="catch")
# findSpeciesErho((catchTidy %>% filter(Species=="scallop")), season="Fall", type="catch")
# findSpeciesTheta((catchTidy %>% filter(Species=="scallop")), season="Fall", type="catch",
#                  df_Es = findSpeciesE((catchTidy %>% filter(Species=="scallop")), season="Fall", type="catch"))
# findSpeciesTheta_rho((catchTidy %>% filter(Species=="scallop")), season="Fall", type="catch",
#                  df_Es = findSpeciesE((catchTidy %>% filter(Species=="scallop")), season="Fall", type="catch"))
# findSpeciesKPSS(catchTidy %>% filter(Species=="scallop"), season="Fall", type="catch")
# 
# #univariate dewdrop with areas as replicates
# catchFallInt <- catchFall %>% mutate(areaInt = as.integer(paste0(Region, Stratum)))
# 
# do_xmap_ID(df=(catchFallInt %>% filter(Species=="scallop") %>% ungroup() %>% select(areaInt, Year, value)), predictor="value", target="value", ID_col="areaInt", E_max=7, tp=1)
# 
# #Weight
# findSpeciesE((catchTidy %>% filter(Species=="scallop")), season="Fall", type="wt")
# findSpeciesErho((catchTidy %>% filter(Species=="scallop")), season="Fall", type="wt")
# findSpeciesTheta((catchTidy %>% filter(Species=="scallop")), season="Fall", type="wt",
#                  df_Es = findSpeciesE((catchTidy %>% filter(Species=="scallop")), season="Fall", type="wt"))
# 
# findSpeciesTheta_rho((catchTidy %>% filter(Species=="scallop")), season="Fall", type="wt",
#                      df_Es = findSpeciesE((catchTidy %>% filter(Species=="scallop")), season="Fall", type="wt"))
# findSpeciesKPSS(catchTidy %>% filter(Species=="scallop"), season="Fall", type="wt")
# 
# #univariate dewdrop with areas as replicates
# wtFallInt <- wtFall %>% mutate(areaInt = as.integer(paste0(Region, Stratum)))
# 
# do_xmap_ID(df=(wtFallInt %>% filter(Species=="scallop") %>% ungroup() %>% select(areaInt, Year, value)), predictor="value", target="value", ID_col="areaInt", E_max=7, tp=1)
# 
# ############ findAreasCorr -------------------------------------------------------
# 
# corrOut <- data.frame(matrix(ncol=5))
# colnames(corrOut) <- c("area", "type","s:r", "s:j", "r:j")
# 
# findAreasCorr <- function(df, season, areaInput) {
#   df_filtered <- df %>% filter(Season == season, area == areaInput) 
#   
#   s.r_catch<-max(abs(ccf(df_filtered$avgCatch_s, df_filtered$avgCatch_r, type = "correlation",lag.max = 6, plot = FALSE)$acf))
#   s.j_catch<-max(abs(ccf(df_filtered$avgCatch_s, df_filtered$avgCatch_j, type = "correlation",lag.max = 6, plot = FALSE)$acf))
#   r.j_catch<-max(abs(ccf(df_filtered$avgCatch_r, df_filtered$avgCatch_j, type = "correlation",lag.max = 6, plot = FALSE)$acf))
#   
#   absmax <- function(x) { x[which.max( abs(x) )]}
#   
#   s.r_wt<-absmax(ccf(df_filtered$avgWt_s, df_filtered$avgWt_r, type = "correlation", plot = FALSE)$acf)
#   s.j_wt<-absmax(ccf(df_filtered$avgWt_s, df_filtered$avgWt_j, type = "correlation", plot = FALSE)$acf)
#   r.j_wt<-absmax(ccf(df_filtered$avgWt_r, df_filtered$avgWt_j, type = "correlation", plot = FALSE)$acf)
#   
#   catchV<- c(df_filtered$area[1], "catch", s.r_catch, s.j_catch, r.j_catch)
#   wtV <- c(df_filtered$area[1], "wt", s.r_wt, s.j_wt, r.j_wt)
#   df_area<- data.frame(rbind(catchV, wtV))
#   colnames(df_area)<- c("area", "type","s:r", "s:j", "r:j")
#   
#   return(df_area)
# }
# 
# for (i in 1:5) {
#   for (j in 1:4) {
#     areaTemp <- paste(i, j)
#     corrOut<- bind_rows(corrOut,findAreasCorr(df=catch,"Fall", areaTemp))
#   }
# }
# #Catch
# for (i in 1:5) {
#   for (j in 1:4) {
#     areaTemp <- paste(i, j)
#     myvecCatch <- as.numeric(findAreasCorr(df=catch,"Fall", areaTemp ) %>% filter(type=="catch") %>% select("s:r", "s:j", "r:j") %>% remove_rownames() %>% slice(1))
#     class(myvecCatch) <- 'dist'
#     attr(myvecCatch,'Size') <- 3
#     corrplot(as.matrix(myvecCatch), is.corr = TRUE, tl.pos = 'n', title = areaTemp, mar=c(0.6,0,1,0))
#   }
# }
# #Weight
# for (i in 1:5) {
#   for (j in 1:4) {
#     areaTemp <- paste(i, j)
#     myvecCatch <- as.numeric(findAreasCorr(df=catch,"Fall", areaTemp ) %>% filter(type=="wt") %>% select("s:r", "s:j", "r:j") %>% remove_rownames() %>% slice(1))
#     class(myvecCatch) <- 'dist'
#     attr(myvecCatch,'Size') <- 3
#     corrplot(as.matrix(myvecCatch), is.corr = TRUE, tl.pos = 'n', title = areaTemp, mar=c(0.6,0,1,0))
#   }
# }
# 
# corrOut <- corrOut %>% slice(-1) %>% remove_rownames()
# 
# ###### CCM
# 
# varsWt <- c("avgWt_s", "avgWt_r", "avgWt_j")
# vars <- c("avgCatch_s", "avgCatch_r", "avgCatch_j")
# var_pairs = combn(vars, 2)
# 
# scalEs <- findSpeciesE((catchTidy %>% filter(Species=="scallop")), season="Fall", type="catch")
# 
# par(mfrow=c(5,4), mar=c(0.6,0.01,1,0.01))
# 
# for (k in 1:5) {
#   for (j in 1:4) {
#     areaTemp <- paste(k, j)
#     
#     E = scalEs %>% slice(k) %>% pull(j)
#     ccm_matrix = array(NA, dim = c(length(vars), length(vars)), dimnames = list(vars,vars))
#     df_temp <- catch %>% filter(area == areaTemp) %>% ungroup() %>% select(Year, all_of(vars))
#     lib_size <- nrow(df_temp)-E
# 
#     for (i in 1:ncol(var_pairs)) {
#        ccm_out = CCM(dataFrame = df_temp, columns = var_pairs[1, i],
#                      target = var_pairs[2, i], libSizes = paste(lib_size, lib_size, 10, collapse = " "),
#                      Tp = 0, E = E, sample = 100)
#              outVars = names(ccm_out)
#             var_out = unlist(strsplit(outVars[2], ":"))
#             ccm_matrix[var_out[2], var_out[1]] = ccm_out[1, 2]
#             var_out = unlist(strsplit(outVars[3], ":"))
#             ccm_matrix[var_out[2], var_out[1]] = ccm_out[1, 3]
#     }
#     corrplot(ccm_matrix, is.corr = FALSE, tl.pos = 'n', title = areaTemp, mar=c(0.6,0,1,0))
#   }
# }
# 
# df_temp_xmap <- catch %>% filter(Season =="Fall", area=="1 1")%>% ungroup() %>% select(Year, all_of(vars))
# do_xmap_noID(df_temp_xmap, predictor = "avgCatch_s", target="avgCatch_r", E_max=7, tp=1,keep_preds = FALSE)
# 
# # Regions -----------------------------------------------------------------
# 
# findSpeciesGroups<- function(df, species, season, type, g) {
#   df_out <- df %>% 
#     filter(Type == type, Season == season, Species == species) %>% 
#     group_by(!!sym(g), Year) %>% 
#     summarise(avg = mean(value)) %>% 
#     group_by(!!sym(g))  %>%
#     summarise(E_opt = findE_v(avg),
#               rho_E = findErho_v(avg),
#               Theta = findTheta_v(avg, E_opt),
#               rho_theta = findThetaRho_v(avg, E_opt))
#   return(df_out)
# }
# 
# findSpeciesGroups(catchTidy, season="Fall", type="catch", g="Region", species="scallop")
# findSpeciesGroups(catchTidy, season="Fall", type="wt", g="Region", species="scallop")
# 
# findSpeciesGroups(catchTidy, season="Fall", type="catch", g="Region", species="rock")
# findSpeciesGroups(catchTidy, season="Fall", type="wt", g="Region", species="rock")
# 
# findSpeciesGroups(catchTidy, season="Fall", type="catch", g="Region", species="jonah")
# findSpeciesGroups(catchTidy, season="Fall", type="wt", g="Region", species="jonah")
# 
# 
# do_xmap_ID(df=wtFall %>% filter(Species=="scallop") %>%
#              ungroup() %>% 
#              mutate(Region = as.numeric(Region)) %>% group_by(Region, Year) %>% 
#              summarise(avg = mean(value)) %>% 
#              ungroup() %>% select(Region, Year, avg),
#              predictor="avg", target="avg", ID_col="Region", E_max=7, tp=1)
# 
# do_xmap_ID(df=catchFall %>% filter(Species=="jonah") %>%
#              ungroup() %>% 
#              mutate(Region = as.numeric(Region)) %>% group_by(Region, Year) %>% 
#              summarise(avg = mean(value)) %>% 
#              ungroup() %>% select(Region, Year, avg),
#            predictor="avg", target="avg", ID_col="Region", E_max=7, tp=1)
# 
# #dewdrop with regions as replicates for catch for scallop about the same rho as linear
# #dewdrop with regions as replicates for weight and catch for jonah crab actually give a rho higher than rho_linear 
# #For jonah catch, much higher than linear
# 
# # Strata -----------------------------------------------------------------
# findSpeciesGroups(catchTidy, season="Fall", type="catch", g="Stratum", species="scallop")
# findSpeciesGroups(catchTidy, season="Fall", type="wt", g="Stratum", species="scallop")
# 
# findSpeciesGroups(catchTidy, season="Fall", type="catch", g="Stratum", species="rock")
# findSpeciesGroups(catchTidy, season="Fall", type="wt", g="Stratum", species="rock")
# 
# findSpeciesGroups(catchTidy, season="Fall", type="catch", g="Stratum", species="jonah")
# findSpeciesGroups(catchTidy, season="Fall", type="wt", g="Stratum", species="jonah")
# 
# 
# do_xmap_ID(df=wtFall %>% filter(Species=="scallop") %>%
#              ungroup() %>% 
#              mutate(Stratum = as.numeric(Stratum)) %>% group_by(Stratum, Year) %>% 
#              summarise(avg = mean(value)) %>% 
#              ungroup() %>% select(Stratum, Year, avg),
#            predictor="avg", target="avg", ID_col="Stratum", E_max=7, tp=1)
# 
# do_xmap_ID(df=catchFall %>% filter(Species=="scallop") %>%
#              ungroup() %>% 
#              mutate(Stratum = as.numeric(Stratum)) %>% group_by(Stratum, Year) %>% 
#              summarise(avg = mean(value)) %>% 
#              ungroup() %>% select(Stratum, Year, avg),
#            predictor="avg", target="avg", ID_col="Stratum", E_max=7, tp=1) #rock crabs higher than for linear!
# 
# 
# do_xmap_ID(df= catch %>% filter(Season=="Fall") %>% 
#   ungroup() %>% 
#   mutate(Stratum = as.numeric(Stratum)) %>% group_by(Stratum, Year) %>% 
#   summarise(avgCatchS = mean(avgCatch_s), avgCatchJ = mean(avgCatch_j),avgCatchR = mean(avgCatch_r)) %>% 
#   ungroup() %>% select(Stratum, Year, avgCatchS, avgCatchJ, avgCatchR),
#   predictor="avgCatchS", target="avgCatchJ", ID_col="Stratum", E_max=7, tp=1)
# 
# do_xmap_ID(df= catch %>% filter(Season=="Fall") %>% 
#              ungroup() %>% 
#              mutate(Region = as.numeric(Region)) %>% group_by(Region, Year) %>% 
#              summarise(avgCatchS = mean(avgCatch_s), avgCatchJ = mean(avgCatch_j),avgCatchR = mean(avgCatch_r)) %>% 
#              ungroup() %>% select(Region, Year, avgCatchS, avgCatchJ, avgCatchR),
#            predictor="avgCatchS", target="avgCatchJ", ID_col="Region", E_max=7, tp=1)
# #s as pred, j as target, nonlinear outperforms linear for regions as replicates
# 
# #Terrible, mostly negative rhos
# # do_xmap_noID(df= catch %>% filter(Season=="Fall") %>% group_by(Year) %>% 
# #              summarise(avgCatchS = mean(avgCatch_s), avgCatchJ = mean(avgCatch_j),avgCatchR = mean(avgCatch_r)) %>% 
# #              ungroup() %>% select(Year, avgCatchS, avgCatchJ, avgCatchR),
# #            predictor="avgCatchR", target="avgCatchJ", E_max=5, tp=1)
# 
# 
