#Jonah crab analysis
#Ruby Krasnow
#July 24, 2023

jonah_catchE <- findSpeciesE(complete_tidy_diff %>% filter(Species=="jonah") %>% na.omit(), type="catch") %>% rowid_to_column(var="region")
jonah_catchE_rho <- findSpeciesErho(complete_tidy_diff %>% filter(Species=="jonah") %>% na.omit(), type="catch")%>% round(digits=3) %>% rowid_to_column(var="region")

jonah_wtE <- findSpeciesE(complete_tidy_diff %>% filter(Species=="jonah") %>% na.omit(), type="wt") 
jonah_wtE_rho <- findSpeciesErho(complete_tidy_diff %>% filter(Species=="jonah") %>% na.omit(), type="wt")%>% round(digits=3)

c(as.matrix(jonah_catchE))
c(as.matrix(jonah_wtE))
c(as.matrix(jonah_wtE))

#predictability for simplex weight vs catch
boxplot(c(as.matrix(jonah_catchE_rho)), c(as.matrix(jonah_wtE_rho)))

jonah_theta<- findSpeciesTheta(complete_tidy_diff %>% filter(Species=="jonah") %>% na.omit(), type="catch") %>% rowid_to_column(var="region")
jonah_theta_rho <- findSpeciesTheta_rho(complete_tidy_diff %>% filter(Species=="jonah") %>% na.omit(), type="catch") %>% rowid_to_column(var="region")

jonah_theta_long <- jonah_theta %>% pivot_longer(cols=2:5, names_to = "stratum", values_to = "theta")
jonah_E_long <- jonah_catchE %>% pivot_longer(cols=2:5, names_to = "stratum", values_to = "E")
jonah_theta_rho_long <- jonah_theta_rho %>% pivot_longer(cols=2:5, names_to = "stratum", values_to = "rho")
jonah_E_rho_long <- jonah_catchE_rho %>% pivot_longer(cols=2:5, names_to = "stratum", values_to = "rho")




jonahGeom <- left_join(regionsGrid_orig, catchCCMdf) %>% select(-c("scallop", "rock"))

ggplot(data = complete_tidy_diff %>% 
         filter(Type == "wt", Species=="jonah"), aes(x=date, y=value, color=area))+geom_line()

ggplot(data = complete_tidy_diff %>% 
         filter(Type == "wt", Species != "scallop"), aes(x=date, y=value, color=area))+geom_line()+facet_wrap(~Species) 
#a lot more variation in the jonah crabs

ggplot(data = complete_tidy_diff %>% filter(Type == "catch", Species != "scallop") %>% group_by(date, Species) %>% 
         summarise(avg = mean(value, na.rm = TRUE)), aes(x=date, y=avg))+geom_line()+facet_wrap(~Species)+labs(y="2nd-differenced catch")+theme_classic()
#a lot more variation in the jonah crabs


ggplot(data = catchTidy_seasons %>% filter(Type == "catch", Species != "scallop") %>% group_by(Year, Season, Species) %>% 
         summarise(avg = mean(value, na.rm = TRUE)), aes(x=Year, y=avg))+geom_line()+facet_grid(Season~Species)+labs(y="catch")+theme_classic()
#a lot more variation in the jonah crabs

catchTidy_seasons_complete<- complete(data = catchTidy_seasons %>% ungroup() %>%  filter(Type=="catch", Species == "jonah"),  Region, Stratum, Season, Year) %>% 
        mutate(area = as.numeric(paste0(Region, Stratum))) %>% 
        mutate(date=paste(Year, case_when(Season== "Fall" ~ "-11-01", Season =="Spring" ~"-05-01"), sep = ""), .before=Region) %>% filter(date != "2000-05-01")
#%>% select(-c("Species", "Type", "Year", "Season", "Region", "Stratum")) %>% arrange(date) %>% group_by(area)
 

cor(c(as.matrix(sq_miles)), c(as.matrix(jonah_catchE)))


sq_miles_v <- c(as.matrix(sq_miles))
tows_v <- c(as.matrix(tows_per_area))

jonah_catchE_v <- c(as.matrix(jonah_catchE))
jonah_catchE_rho_v <- c(as.matrix(jonah_catchE_rho))

jonah_catch_theta_v <- c(as.matrix(jonah_catch_theta))
jonah_catch_theta_rho_v <- c(as.matrix(jonah_catch_theta_rho))

jonah_wtE_v <- c(as.matrix(jonah_wtE))
jonah_wtE_rho_v <- c(as.matrix(jonah_wtE_rho))

jonah_wt_theta_v <- c(as.matrix(jonah_wt_theta))
jonah_wt_theta_rho_v <- c(as.matrix(jonah_wt_theta_rho))

corDf <- data.frame(sq_miles_v, tows_v, jonah_catchE_v, jonah_catchE_rho_v, jonah_catch_theta_v, jonah_catch_theta_rho_v, jonah_wtE_v, jonah_wtE_rho_v, jonah_wt_theta_v, jonah_wt_theta_rho_v)
areaList <- c(11, 12, 13, 14, 21, 22, 23, 24, 31, 32, 33, 34, 41, 42, 43, 44, 51, 52, 53, 54)
library(corrplot)
corrplot(cor(corDf), method = "circle", order = 'hclust', type="lower")
testRes = cor.mtest(corDf, conf.level = 0.95)
corrplot(cor(corDf), method = "circle", order = 'hclust', type="lower", diag=FALSE, p.mat = testRes$p, sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.9,
         insig = 'label_sig', pch.col = 'grey20')
