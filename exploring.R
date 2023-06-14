#Exploratory data analysis
#June 13, 2023

library(ggplot2)
library(dplyr)

# dat_all<- read.csv("22564_UNION_FSCS_SVLEN.csv")
# head(dat_all)
# summary(dat_all) #530652 observations of 13 vars
# 
# glimpse(dat_all)
# 
# name<-as.factor(dat_all$SCIENTIFIC_NAME)
# summary(name)
# 
# data <- dat_all %>%
#   filter(SCIENTIFIC_NAME == "Placopecten magellanicus (sea scallop)" | 
#            SCIENTIFIC_NAME == "Placopecten magellanicus (sea scallop clapper)")

# cruises <- read.csv("data/22564_SVDBS_CRUISES.csv")
# svbio <- read.csv("data/22564_UNION_FSCS_SVBIO.csv", quote = "")
# svsta <- read.csv("data/22564_UNION_FSCS_SVSTA.csv")

svcat <- read.csv("data/22564_UNION_FSCS_SVCAT.csv")
head(svcat)
summary(svcat)
scallops <- svcat %>% 
  filter(SCIENTIFIC_NAME=="Placopecten magellanicus (sea scallop)" | 
           SCIENTIFIC_NAME =="Placopecten magellanicus (sea scallop clapper)")


scallops <- scallops %>% 
  mutate(name = SCIENTIFIC_NAME,
         cruise6 = as.factor(CRUISE6),
         cruise = as.factor(CRUISE),
         sex = as.factor(CATCHSEX),
         stratum = as.factor(STRATUM),
         .keep = "unused")

summary(scallops)

scallops <- scallops %>% 
  mutate(name = recode(name, "Placopecten magellanicus (sea scallop)" = "scallop", "Placopecten magellanicus (sea scallop clapper)" = "clapper"))

summarise_all(scallops,n_distinct)

#All have sex = 0, means unknown sex
#SVSPP is species, listed as either 401 (scallop) or 400 (clammer), redundant info to species

#Remove unnecessary variables
scallops <- scallops %>% 
  select(-c("sex", "STATUS_CODE", "SVSPP"))

#ggplot(scallops, aes(x = cruise)) + geom_bar()
ggplot(scallops, aes(x = cruise6)) + geom_bar()
#ggplot(scallops, aes(x = stratum)) + geom_bar()

cruises <- read.csv("data/22564_SVDBS_CRUISES.csv")
cruises <- cruises %>% 
  select(c("CRUISE6","SEASON","YEAR")) %>% 
  mutate(cruise6 = as.factor(CRUISE6), .keep="unused")


scallops <- left_join(scallops, cruises, by="cruise6")


species <- svcat %>% 
  group_by(SCIENTIFIC_NAME) %>% 
  summarise(avg = mean(EXPCATCHWT))
  

species

trues <- species %>%  filter(!is.na(species$avg)==TRUE)

# preds <- svcat %>% 
#   filter(SCIENTIFIC_NAME=="Cancer borealis (Jonah crab)" | 
#            SCIENTIFIC_NAME =="Placopecten magellanicus (sea scallop clapper)")