#Exploratory data analysis
#June 16, 2023

library(tidyverse)
library(lubridate)

# dat_len<- read.csv("22564_UNION_FSCS_SVLEN.csv")
# head(dat_len)
# summary(dat_len) #530652 observations of 13 vars
# 
# glimpse(dat_len)
# 
# data <- dat_len %>%
#   filter(SCIENTIFIC_NAME == "Placopecten magellanicus (sea scallop)" | 
#            SCIENTIFIC_NAME == "Placopecten magellanicus (sea scallop clapper)")

# svbio <- read.csv("data/22564_UNION_FSCS_SVBIO.csv", quote = "")

# Predators ---------------------------------------------------------------

# species <- svcat %>% 
#   group_by(SCIENTIFIC_NAME) %>% 
#   summarise(avg = mean(EXPCATCHWT))
#   
# species
# trues <- species %>%  filter(!is.na(species$avg)==TRUE)
# 
# # preds <- svcat %>% 
# #   filter(SCIENTIFIC_NAME=="Cancer borealis (Jonah crab)" | 
# #            SCIENTIFIC_NAME =="Placopecten magellanicus (sea scallop clapper)")


# Data wrangling ----------------------------------------------------------

# svcat <- read.csv("data/22564_UNION_FSCS_SVCAT.csv")
# head(svcat)
# summary(svcat)
# scallops <- svcat %>% 
#   filter(SCIENTIFIC_NAME=="Placopecten magellanicus (sea scallop)" | 
#            SCIENTIFIC_NAME =="Placopecten magellanicus (sea scallop clapper)")
# 
# 
# scallops <- scallops %>% 
#   mutate(name = SCIENTIFIC_NAME,
#          CRUISE6 = as.factor(CRUISE6),
#          CRUISE = as.factor(CRUISE),
#          CATCHSEX = as.factor(CATCHSEX),
#          STRATUM = as.factor(STRATUM),
#          .keep = "unused")
# 
# summary(scallops)
# 
# scallops <- scallops %>% 
#   mutate(name = recode(name, "Placopecten magellanicus (sea scallop)" = "scallop", 
#                        "Placopecten magellanicus (sea scallop clapper)" = "clapper"))
# 
# summarise_all(scallops,n_distinct)

#All have sex = 0, means unknown sex
#SVSPP is species, listed as either 401 (scallop) or 400 (clapper), redundant info to species
#So we can remove unnecessary variables
# scallops <- scallops %>% 
#   select(-c("CATCHSEX", "STATUS_CODE", "SVSPP"))

#ggplot(scallops, aes(x = cruise)) + geom_bar()
#ggplot(scallops, aes(x = cruise6)) + geom_bar()
#ggplot(scallops, aes(x = stratum)) + geom_bar()


# Creating a time series ----------------------------------------------------
# 
# cruises <- read.csv("data/22564_SVDBS_CRUISES.csv")
# cruises <- cruises %>% 
#   select(c("CRUISE6","SEASON","YEAR")) %>% 
#   mutate(CRUISE6 = as.factor(CRUISE6), .keep="unused")
# 
# scallops <- left_join(scallops, cruises, by="CRUISE6")


# Fixing lack of data -----------------------------------------------------
# 
# df_stations <- read.csv("data/22564_UNION_FSCS_SVSTA.csv")
# df_strata <- read.csv("data/SVDBS_SVMSTRATA.csv")
# 
# df_stations_wrangled <- df_stations %>%
#   # filter(EST_YEAR %in% unlist(target_chunks)) %>%
#   mutate(DATE = parse_date_time(BEGIN_EST_TOWDATE,orders="mdYHMS",truncated = 3)) %>%
#   select(STATION,STRATUM,DATE) %>%
#   mutate(DATE=year(DATE)) %>%
#   distinct()
# 
# #shows how many occurrences of each station are in the data (i.e. how many years that station was sampled)
# data.frame(table(df_stations_wrangled$STATION))
# data.frame(table(df_stations_wrangled$STRATUM))
# data.frame(table(df_stations_wrangled$DATE)) #only goes up to 2006, then a few in 2015 and 2021. Missing HabCam data?

#ggplot(scallopsNew, aes(x = CRUISE)) + geom_bar()
#ggplot(scallopsNew, aes(x = CRUISE6)) + geom_bar()
#ggplot(scallopsNew, aes(x = STRATUM)) + geom_bar()

# diffMonths <- df_stationsNew %>% 
#   filter(EST_MONTH != GMT_MONTH)

# Redoing everything with new data ----------------------------------------

svcatNew <- read.csv("data/new22564_UNION_FSCS_SVCAT.csv") #there's more data: 96206 observations instead of 94632 (1574 more entries)
head(svcatNew)
summary(svcatNew)
scallopsNew <- svcatNew %>% 
  filter(SCIENTIFIC_NAME=="Placopecten magellanicus (sea scallop)" | 
           SCIENTIFIC_NAME =="Placopecten magellanicus (sea scallop clapper)")

scallopsNew <- scallopsNew %>% 
  mutate(name = SCIENTIFIC_NAME,
         CRUISE6 = as.factor(CRUISE6),
         CRUISE = as.factor(CRUISE),
         CATCHSEX = as.factor(CATCHSEX),
         .keep = "unused")

scallopsNew <- scallopsNew %>% 
  mutate(name = recode(name, "Placopecten magellanicus (sea scallop)" = "scallop", 
                       "Placopecten magellanicus (sea scallop clapper)" = "clapper"))

scallopsNew <- scallopsNew %>% 
  select(-c("CATCHSEX", "STATUS_CODE", "SVSPP"))

# Creating a time series ----------------------------------------------------

cruisesNew <- read.csv("data/new22564_SVDBS_CRUISES.csv")
cruisesNew <- cruisesNew %>% 
  select(c("CRUISE6","SEASON","YEAR")) %>% 
  mutate(CRUISE6 = as.factor(CRUISE6), .keep="unused")

scallopsNew <- left_join(scallopsNew, cruisesNew, by="CRUISE6")

as_tibble(scallopsNew)
str(scallopsNew)
data.frame(table(scallopsNew$YEAR))


# Find strata with all 45 years of data -----------------------------------------------------

#this gets rid of some weird strata in 1984 with letters, which don't seem to 
# correspond to any of the NOAA (shellfish, dredge, or trawl survey) strata
scallopsNew <-  scallopsNew %>%
  mutate(STRATUM = as.integer(STRATUM)) %>% 
  filter(!is.na(STRATUM))

#This will show you which ones were removed
# scallopsTest <-  scallopsNew %>%
#   mutate(strat = as.integer(STRATUM)) %>% 
#   filter(is.na(strat))

df_strataNew <- read.csv("data/newSVDBS_SVMSTRATA.csv")

#shows how many occurrences of each stratum are in the data (i.e. how many years that stratum was sampled)
data.frame(table(scallopsNew$STRATUM))

#shellfish strata start with 6
scallopsNew <-  scallopsNew %>%
  filter(STRATUM > 5999)

scallopsTest <- scallopsNew %>% 
  group_by(STRATUM) %>% 
  summarise(years=n_distinct(YEAR))


# Spatial -----------------------------------------------------------------

library(sf)

# SMAST Scallop data - the data I want, but averaged across years
st_layers("data/SMAST_SCALLOP/SMAST_Scallops.gdb")
test <- st_read("data/SMAST_SCALLOP/SMAST_Scallops.gdb")

class(test)
attr(test, "sf_column")
print(test[1:15], n = 3)
test_geom <- st_geometry(test)

par(mar = c(0,0,1,0))
plot(test[6])

# NEFSC scallop survey data - through 2015
st_layers("~/Downloads/Fish/Fish.gdb")
fish <- st_read("~/Downloads/Fish/Fish.gdb", layer="ScallopBiomass")

attr(fish, "sf_column")
st_geometry(fish)
class(fish)

par(mar = c(0,0,1,0))
plot(fish[6])
