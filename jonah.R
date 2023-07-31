#Jonah crab analysis
#Ruby Krasnow
#July 24, 2023


fig_for_slide <- complete_tidy_diff %>% filter(Species=="jonah", Region== 1, Stratum==1, Type=="catch")
fig_for_slide$lag <- lead(fig_for_slide$value)
ggplot(data=fig_for_slide, aes(x=value, y=lag))+geom_line()

EmbedDimension(dataFrame = fig_for_slide %>% select(value), pred="1 37", lib="1 37", columns="value", target="value")
PredictNonlinear(dataFrame = fig_for_slide %>% select(value), pred="1 37", lib="1 37", columns="value", target="value", E=2)

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
# ggplot(data = catchTidy_seasons %>% filter(Type == "catch", Species != "scallop") %>% group_by(Year, Season, Species) %>% 
#          summarise(avg = mean(value, na.rm = TRUE)), aes(x=Year, y=avg))+geom_line()+facet_grid(Season~Species)+labs(y="catch")+theme_classic()
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
# 
# 
# # Testing imputed data ----------------------------------------------------
# library(zoo)
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

map_reg_rho <- left_join(surveyGrid %>% group_by(Region) %>% summarise(num = n_distinct(GridID)), RESULTS_ccm_by_reg_max %>% rename(Region=region))

# reg_cols <- pivot_wider(RESULTS_ccm_by_reg_max, names_from = name, values_from = value) %>%
#        group_by(region) %>% select(-c(xmap, E, LibSize)) %>% rename(Region = region) %>% 
#        fill(everything(), .direction = "downup") %>% slice(1)

(ggplot(data=map_reg_rho)+geom_sf(aes(fill=value))+scale_fill_viridis_c(option="F"))+facet_wrap(~name)

(ggplot(data=left_join(surveyGrid %>% group_by(Region) %>% summarise(num = n_distinct(GridID)), RESULTS_ccm_wt_by_reg_max %>% rename(Region=region)))+geom_sf(aes(fill=value))+scale_fill_viridis_c(option="F"))+facet_wrap(~name)

ggplot(data=left_join(surveyGrid %>% group_by(Stratum) %>% summarise(num = n_distinct(GridID)), RESULTS_ccm_by_strat_max %>% rename(Stratum=stratum)))+geom_sf(aes(fill=value))+facet_wrap(~name)

ggplot(data=left_join(surveyGrid %>% group_by(Stratum) %>% summarise(num = n_distinct(GridID)), RESULTS_ccm_wt_by_strat_max %>% rename(Stratum=stratum)))+geom_sf(aes(fill=value))+facet_wrap(~name)


ggplot(data=left_join(surveyGrid %>% group_by(area) %>% summarise(num = n_distinct(GridID)), RESULTS_ccm_by_area_max %>% rename(area=areaInput)))+geom_sf(aes(fill=value))+facet_wrap(~name) #+scale_fill_viridis_c(option="F")

ggplot(data=left_join(surveyGrid %>% group_by(area) %>% summarise(num = n_distinct(GridID)), RESULTS_ccm_wt_by_area_max %>% rename(area=areaInput)))+geom_sf(aes(fill=value))+facet_wrap(~name)

landings <- read.csv("data/MaineDMR_Landings.csv")
ggplot(data=landings)+geom_line(aes(x=year, y=total_weight, color=species))

library(scales)
ggplot(data=landings %>% filter(species=="Crab Jonah"))+
  geom_line(aes(x=year, y=total_value))+
  theme_classic()+
  labs(x="Year", y="Total value ($)")+
  scale_y_continuous(limits=c(0,2E6), breaks=seq(0,2E6, 5E5) , expand=c(0,0), labels=comma)+
  theme(axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))

cor.test()
catchCCMdf_agg
landings