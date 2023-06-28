#Summary of data analysis thus far
#Last updated: June 23, 2023

library(tidyverse)
library(lubridate)
library(rEDM)

dat_len<- read.csv("data/22564_UNION_FSCS_SVLEN2022.csv")
dat_cat <- read.csv("data/22564_UNION_FSCS_SVCAT2022.csv")
dat_bio <- read.csv("data/22564_UNION_FSCS_SVBIO2022.csv", quote = "")
cruises <- read.csv("data/22564_SVDBS_CRUISES2022.csv")
df_stations <- read.csv("data/22564_UNION_FSCS_SVSTA2022.csv")
df_strata <- read.csv("data/newSVDBS_SVMSTRATA.csv")

# Cruises & stations ---------------------------------------------------- 

cruises <- cruises %>%
  select(c("CRUISE6","SEASON","YEAR")) %>%
  mutate(CRUISE6 = as.factor(CRUISE6), .keep="unused")

df_stations <- df_stations %>% 
  mutate(STATION = as.numeric(STATION), YEAR = EST_YEAR) %>% 
  select(c("CRUISE6","CRUISE","STRATUM", "TOW","STATION","ID","AREA","SVVESSEL","SVGEAR",
     "BEGIN_EST_TOWDATE","YEAR","SETDEPTH", "AVGDEPTH", "BEGLAT", "BEGLON"))

df_stations <- df_stations %>% 
  mutate(BEGIN_EST_TOWDATE = lubridate::dmy(BEGIN_EST_TOWDATE)) %>% 
  mutate(YEAR = year(BEGIN_EST_TOWDATE)) %>% 
  mutate(TOW = as.integer(TOW))

# Length ------------------------------------------------------------------

head(dat_len)
str(dat_len) #543666 observations of 13 vars

scallopLen <- dat_len %>%
  filter(SCIENTIFIC_NAME == "Placopecten magellanicus (sea scallop)") %>% 
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
         STATION = as.numeric(STATION),
         .keep = "unused")

summarise_all(scallopLen,n_distinct)

scallopLen <- left_join(scallopLen, cruises, by="CRUISE6")

scallopLen <- scallopLen %>%
  mutate(YEAR = as.numeric(YEAR)) %>%
  filter(as.numeric(STRATUM) > 5999) #shellfish strata start with 6

len <- left_join(scallopLen, df_stations)

# Catch ----------------------------------------------------------

scallopCat <- dat_cat %>%
  filter(SCIENTIFIC_NAME == "Placopecten magellanicus (sea scallop)") %>% 
  mutate(NAME = recode(SCIENTIFIC_NAME, "Placopecten magellanicus (sea scallop)" = "scallop"))

# scallopCat <- scallopCat %>% 
#   select(-c("CATCHSEX", "STATUS_CODE", "SVSPP", "SCIENTIFIC_NAME"))

summarise_all(scallopLen,n_distinct)

scallopCat <- scallopCat %>%
  mutate(CRUISE6 = as.factor(CRUISE6),
         CRUISE = as.factor(CRUISE),
         STATION = as.numeric(STATION),
         .keep = "unused")

scallopCat <- left_join(scallopCat, cruises, by="CRUISE6")

#shellfish strata start with 6
 scallopCat <-  scallopCat %>%
   filter(as.numeric(STRATUM) > 5999)

str(scallopCat)
summarise_all(scallopCat,n_distinct)
as_tibble(scallopCat)
data.frame(table(scallopCat$YEAR))

cat <- left_join(scallopCat, df_stations)

catTest <- cat %>% 
  group_by(STRATUM) %>% 
  summarise(years=n_distinct(YEAR))

lenTest <- len %>% 
  group_by(STRATUM) %>% 
  summarise(years=n_distinct(YEAR))



# Predators ---------------------------------------------------------------

speciesLen <- dat_len %>% 
  group_by(SCIENTIFIC_NAME) %>% 
  summarise(avg = mean(LENGTH))

speciesCat <- dat_cat %>% 
  group_by(SCIENTIFIC_NAME) %>% 
  summarise(count = n_distinct(ID))


########## crabs
# crabsLen <- dat_len %>% 
#   filter(SCIENTIFIC_NAME=="Cancer borealis (Jonah crab)" | 
#            SCIENTIFIC_NAME =="Cancer irroratus (Atlantic rock crab)") %>%
#   mutate(CRUISE6 = as.factor(CRUISE6),
#          CRUISE = as.factor(CRUISE),
#          STATION = as.numeric(STATION),
#          .keep = "unused")
# 
# crabsLen <- left_join(crabsLen, cruises, by="CRUISE6")
# crabsTest <- crabsLen %>% 
#   group_by(STRATUM) %>% 
#   summarise(years=n_distinct(YEAR))
# 
# crabsCat <- dat_cat %>% 
#   filter(SCIENTIFIC_NAME=="Cancer borealis (Jonah crab)" | 
#            SCIENTIFIC_NAME =="Cancer irroratus (Atlantic rock crab)") %>%
#   mutate(CRUISE6 = as.factor(CRUISE6),
#          CRUISE = as.factor(CRUISE),
#          STATION = as.numeric(STATION),
#          .keep = "unused")
# 
# crabsCat <- left_join(crabsCat, cruises, by="CRUISE6")
# crabsTest2 <- crabsCat %>% 
#   group_by(STRATUM) %>% 
#   summarise(years=n_distinct(YEAR))


########## Astropecten

# astroLen <- dat_len %>% 
#   filter(SCIENTIFIC_NAME=="Astropecten" | #none
#         SCIENTIFIC_NAME =="Astropecten articulatus (royal sea star)") %>%  #none
#   mutate(CRUISE6 = as.factor(CRUISE6),
#          CRUISE = as.factor(CRUISE),
#          STATION = as.numeric(STATION),
#          .keep = "unused")

astroCat <- dat_cat %>% 
  filter(SCIENTIFIC_NAME=="Astropecten" |
           SCIENTIFIC_NAME =="Astropecten articulatus (royal sea star)") %>%
  mutate(CRUISE6 = as.factor(CRUISE6),
         CRUISE = as.factor(CRUISE),
         STATION = as.numeric(STATION),
         .keep = "unused")

astroCat <- left_join(astroCat, cruises, by="CRUISE6")
astroTest2 <- astroCat %>% 
  group_by(STRATUM, SCIENTIFIC_NAME) %>% 
  summarise(years=n_distinct(YEAR))

########## Asterias

# aLen <- dat_len %>% 
#   filter(SCIENTIFIC_NAME =="Asterias" | #one
#            SCIENTIFIC_NAME =="Asterias rubens (boreal asterias)"  ) %>% #one
#   mutate(CRUISE6 = as.factor(CRUISE6),
#          CRUISE = as.factor(CRUISE),
#          STATION = as.numeric(STATION),
#          .keep = "unused")
# 
# aLen <- left_join(aLen, cruises, by="CRUISE6")
# aTest <- aLen %>% 
#   group_by(STRATUM) %>% 
#   summarise(years=n_distinct(YEAR)) #only one year

aCat <- dat_cat %>% 
  filter(SCIENTIFIC_NAME =="Asterias") %>% 
            # | SCIENTIFIC_NAME =="Asterias rubens (boreal asterias)"  ) %>%
  mutate(CRUISE6 = as.factor(CRUISE6),
         CRUISE = as.factor(CRUISE),
         STATION = as.numeric(STATION),
         .keep = "unused")

aCat <- left_join(aCat, cruises, by="CRUISE6")

aTest2 <- aCat %>% 
  group_by(STRATUM, SCIENTIFIC_NAME) %>% 
  summarise(years=n_distinct(YEAR))

aTest20<- aTest2 %>% 
  filter(years > 19)
  
aCat <- aCat %>% 
  filter(STRATUM %in% (aTest20$STRATUM))

scallopLenOverlap <- len %>% 
  filter(ID %in% aCat$ID)

scallopCatchOverlap <- scallopCat %>% 
  filter(ID %in% aCat$ID) %>% 
  group_by(STRATUM, YEAR) %>% 
  summarize(scallopAvg = mean(EXPCATCHNUM))

asteriasOverlap <- aCat %>% 
  filter(ID %in% scallopCat$ID) %>% 
  group_by(STRATUM, YEAR) %>% 
  summarize(asteriasAvg = mean(EXPCATCHNUM))

scallopCatchOverlap2 <- left_join(scallopCatchOverlap, asteriasOverlap)  %>% 
  pivot_longer(
    cols = ends_with("Avg"),
    names_to = "species",
 #   names_pattern = ".Avg",
    values_to = "avgCatch"
  ) %>% 
  mutate(species = recode(species, "scallopAvg" = "scallop", "asteriasAvg" = "asterias"))

ggplot(scallopCatchOverlap2, aes(x=YEAR, y = avgCatch, group= STRATUM, color=STRATUM))+geom_line()+facet_wrap(~species)

ggplot(scallopCatchOverlap2, aes(x=YEAR, y = avgCatch, group= species, color=species))+geom_line()+facet_wrap(~STRATUM)

scallopCatchOverlap3 <- left_join(scallopCatchOverlap, asteriasOverlap)
ggplot(scallopCatchOverlap3, aes(x=scallopAvg, y = asteriasAvg, group= STRATUM, color=STRATUM))+geom_point()

formatScal<- scallopCatchOverlap3 %>% 
  group_by(YEAR) %>% 
  summarise(avg = mean(scallopAvg)) %>% 
  ungroup()

formatStar<- scallopCatchOverlap3 %>% 
  group_by(YEAR) %>% 
  summarise(avg = mean(asteriasAvg)) %>% 
  ungroup()

formatStar <- formatStar[!is.na(formatStar$avg),]

