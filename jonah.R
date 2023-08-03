#Jonah crab analysis
#Ruby Krasnow
#Last modified: Aug 3, 2023

library(tidyverse)
library(rEDM)

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
map_reg_rho <- left_join(surveyGrid %>% group_by(Region) %>% summarise(num = n_distinct(GridID)), RESULTS_ccm_by_reg_max %>% rename(Region=region))

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


# From model_comparison.R ---------------------------------------------------
library(plotly)

#### CATCH
tsdf_plot <- tsdf %>% mutate(yrs=index(ts1))

# Average catch over time
ggplot(data = tsdf_plot, aes(x=yrs, y=value))+
  geom_line()+
  labs(x="Years (t)", y="Avg. catch (x)")+
  theme_classic()+
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        axis.text.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.text.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)))

tsdf_plot <- tsdf_plot %>% mutate(lag_val = lag(value), lag_val2 = lag(lag_val)) 
tsdf_plot <-tsdf_plot %>% rbind(tsdf_plot %>% slice(3))

# E = 2 plot
ggplot(data = tsdf_plot, aes(x=value, y=lag_val))+
  geom_path()+
  labs(x="X(t)", y="X(t-1)")+
  theme_classic()+
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        axis.text.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.text.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)))

fig_catch <- plot_ly(tsdf_plot, x = ~value, y = ~lag_val, z = ~lag_val2, type = 'scatter3d', mode = 'lines+markers',
               line = list(width = 6, color = ~c, colorscale = 'Viridis'),
                marker = list(size = 3.5, color = ~c, colorscale = 'Greens'))
fig_catch

#### WEIGHT
ts_wt <- ts(catchTidy_agg %>% filter(Species=="jonah", Type=="wt") %>% mutate(date=as.Date(date)) %>% arrange(date), frequency = 2, start=c(2001, 1))

ts_wt<- na.spline(ts_wt) #using na.spline produces better EDM results than linear interpolation via na.approx

tsdf_wt <- data.frame(ts_wt[,c("Season", "Year", "value", "date")]) %>%
  mutate(lag_val = lag(value), lag_val2 = lag(lag_val), yrs = index(ts1))

tsdf_wt <-tsdf_wt %>% rbind(tsdf_wt %>% slice(3))

fig_wt <- plot_ly(tsdf_wt, x = ~value, y = ~lag_val, z = ~lag_val2, type = 'scatter3d', mode = 'lines+markers',
                     line = list(width = 6, color = ~c, colorscale = 'Viridis'),
                     marker = list(size = 3.5, color = ~c, colorscale = 'Greens'))

fig_wt

tsdf_combined <- left_join(tsdf_plot, tsdf_wt, by=c("Season", "Year", "date"))

plot_ly(tsdf_combined2, x = ~value.x, y = ~lag_val.x, z = ~lag_val2.x, type = 'scatter3d', mode = 'lines+markers',
     #   line = list(width = 6, color = ~c, colorscale = 'Viridis'),
        line = list(width = 6, color = 'black'),
        marker = list(size = 3.5, color = ~c, colorscale = 'Greens'))%>% 
  add_trace(x= ~value.y, y=~lag_val.y, z= ~lag_val2.y,
            line = list(width = 6, color = 'black'))
         #   line = list(width = 6, color = ~c, colorscale = 'Viridis'))

tsdf_combined2 <- tsdf_combined %>% mutate(across((starts_with("val") | starts_with("lag")), 
                     ~ (.x - mean(.x, na.rm=TRUE))/ sd(.x, na.rm=TRUE)))


plot_ly(tsdf_combined2 %>% slice(1:44), 
        x = ~value.x, 
        y = ~lag_val.x, 
        z = ~lag_val2.x, 
        type = 'scatter3d', 
        mode = 'lines+markers',
        line = list(width = 6, color = ~c, colorscale = 'Viridis'),
        marker = list(size = 3.5, color = ~c, colorscale = 'Greens')) %>% 
  layout(scene = list(xaxis = list(title = 'x(t)', showticklabels=FALSE),
                      yaxis = list(title = 'x(t-1)', showticklabels=FALSE),
                      zaxis = list(title = 'x(t-2)', showticklabels=FALSE)),
         font=list(size=15)) %>% 
  add_trace(x= ~value.y, y=~lag_val.y, z= ~lag_val2.y,
  line = list(width = 6, color = ~c, colorscale = 'Viridis'))

                     
# Average weight over time
ggplot(data = tsdf_wt, aes(x=yrs, y=value))+
  geom_line()+
  labs(x="Years", y="Avg. wt")+
  theme_classic()+
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.text.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.text.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)))
