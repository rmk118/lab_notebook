#Exploratory data analysis
#June 14, 2023

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

svcat <- read.csv("data/22564_UNION_FSCS_SVCAT.csv")
head(svcat)
summary(svcat)
scallops <- svcat %>% 
  filter(SCIENTIFIC_NAME=="Placopecten magellanicus (sea scallop)" | 
           SCIENTIFIC_NAME =="Placopecten magellanicus (sea scallop clapper)")


scallops <- scallops %>% 
  mutate(name = SCIENTIFIC_NAME,
         CRUISE6 = as.factor(CRUISE6),
         CRUISE = as.factor(CRUISE),
         CATCHSEX = as.factor(CATCHSEX),
         STRATUM = as.factor(STRATUM),
         .keep = "unused")

summary(scallops)

scallops <- scallops %>% 
  mutate(name = recode(name, "Placopecten magellanicus (sea scallop)" = "scallop", "Placopecten magellanicus (sea scallop clapper)" = "clapper"))

summarise_all(scallops,n_distinct)

#All have sex = 0, means unknown sex
#SVSPP is species, listed as either 401 (scallop) or 400 (clammer), redundant info to species

#Remove unnecessary variables
scallops <- scallops %>% 
  select(-c("CATCHSEX", "STATUS_CODE", "SVSPP"))

#ggplot(scallops, aes(x = cruise)) + geom_bar()
#ggplot(scallops, aes(x = cruise6)) + geom_bar()
#ggplot(scallops, aes(x = stratum)) + geom_bar()


# Creating a time series ----------------------------------------------------

cruises <- read.csv("data/22564_SVDBS_CRUISES.csv")
cruises <- cruises %>% 
  select(c("CRUISE6","SEASON","YEAR")) %>% 
  mutate(CRUISE6 = as.factor(CRUISE6), .keep="unused")

scallops <- left_join(scallops, cruises, by="CRUISE6")


# Fixing lack of data -----------------------------------------------------

df_stations <- read.csv("data/22564_UNION_FSCS_SVSTA.csv")

df_stations_wrangled <- df_stations %>%
  # filter(EST_YEAR %in% unlist(target_chunks)) %>%
  mutate(DATE = parse_date_time(BEGIN_EST_TOWDATE,orders="mdYHMS",truncated = 3)) %>%
  select(STATION,STRATUM,DATE) %>%
  mutate(DATE=year(DATE)) %>%
  distinct()


