#Exploratory data analysis
#June 16, 2023

library(tidyverse)
library(lubridate)

dat_len<- read.csv("data/22564_UNION_FSCS_SVLEN2022.csv")
dat_cat <- read.csv("data/22564_UNION_FSCS_SVCAT2022.csv")
dat_bio <- read.csv("data/22564_UNION_FSCS_SVBIO2022.csv", quote = "")
cruises <- read.csv("data/22564_SVDBS_CRUISES2022.csv")
df_stations <- read.csv("data/22564_UNION_FSCS_SVSTA2022.csv")
df_strata <- read.csv("data/newSVDBS_SVMSTRATA.csv")

# Cruises ---------------------------------------------------- 

cruises <- cruises %>%
  select(c("CRUISE6","SEASON","YEAR")) %>%
  mutate(CRUISE6 = as.factor(CRUISE6), .keep="unused")

# Length ------------------------------------------------------------------

head(dat_len)
str(dat_len) #543666 observations of 13 vars

scallopLen <- dat_len %>%
  filter(SCIENTIFIC_NAME == "Placopecten magellanicus (sea scallop)" %>% 
     mutate(NAME = recode(SCIENTIFIC_NAME, "Placopecten magellanicus (sea scallop)" = "scallop")) #shorten for readability
                           
#If interested in clappers also:
# SCIENTIFIC_NAME == "Placopecten magellanicus (sea scallop clapper)") %>%  #filter to only scallops
# recode(SCIENTIFIC_NAME, "Placopecten magellanicus (sea scallop clapper)" = "clapper")
      
#All have sex = 0, means sex unknown/not recorded
#SVSPP is species, either 401 (scallop) or 400 (clapper), redundant to name column
#So we can remove unnecessary variables

scallopLen <- scallopLen %>% 
  select(-c("CATCHSEX", "STATUS_CODE", "SVSPP", "SCIENTIFIC_NAME"))

scallopLen <- scallopLen %>%
  mutate(CRUISE6 = as.factor(CRUISE6),
         CRUISE = as.factor(CRUISE),
         .keep = "unused")

summarise_all(scallopLen,n_distinct)

scallopLen <- left_join(scallopLen, cruises, by="CRUISE6")


# Catch ----------------------------------------------------------

scallopCat <- dat_cat %>%
  filter(SCIENTIFIC_NAME == "Placopecten magellanicus (sea scallop)" %>% 
  mutate(NAME = recode(SCIENTIFIC_NAME, "Placopecten magellanicus (sea scallop)" = "scallop"))

scallopCat <- scallopCat %>% 
  select(-c("CATCHSEX", "STATUS_CODE", "SVSPP", "SCIENTIFIC_NAME"))

summarise_all(scallopLen,n_distinct)

scallopCat <- scallopCat %>%
  mutate(CRUISE6 = as.factor(CRUISE6),
         CRUISE = as.factor(CRUISE),
         .keep = "unused")

scallopCat <- left_join(scallopCat, cruises, by="CRUISE6")

#shellfish strata start with 6
 scallopCat <-  scallopCat %>%
   filter(as.numeric(STRATUM) > 5999)

str(scallopCat)
summarise_all(scallopCat,n_distinct)
as_tibble(scallopCat)
data.frame(table(scallopCat$YEAR))

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

#this gets rid of some weird strata in 1984 with letters, which don't seem to 
# correspond to any of the NOAA (shellfish, dredge, or trawl survey) strata
scallopsNew <-  scallopsNew %>%
  mutate(STRATUM = as.integer(STRATUM)) %>% 
  filter(!is.na(STRATUM))

#This will show you which ones were removed
# scallopsTest <-  scallopsNew %>%
#   mutate(strat = as.integer(STRATUM)) %>% 
#   filter(is.na(strat))

#shows how many occurrences of each stratum are in the data (i.e. how many years that stratum was sampled)
data.frame(table(scallopCat$STRATUM))
data.frame(table(scallopLen$STRATUM))



scallopsTest <- scallopCat %>% 
  group_by(STRATUM) %>% 
  summarise(years=n_distinct(YEAR))

# Predators ---------------------------------------------------------------

speciesLen <- dat_len %>% 
  group_by(SCIENTIFIC_NAME) %>% 
  summarise(avg = mean(LENGTH))

speciesCat <- dat_cat %>% 
  group_by(SCIENTIFIC_NAME) %>% 
  summarise(avg = mean(EXPCATCHNUM))

crabsLen <- dat_len %>% 
  filter(SCIENTIFIC_NAME=="Cancer borealis (Jonah crab)" | 
           SCIENTIFIC_NAME =="Cancer irroratus (Atlantic rock crab)")

crabsCat <- dat_cat %>% 
  filter(SCIENTIFIC_NAME=="Cancer borealis (Jonah crab)" | 
           SCIENTIFIC_NAME =="Cancer irroratus (Atlantic rock crab)")