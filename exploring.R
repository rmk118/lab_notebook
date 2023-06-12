#Exploratory data analysis
#June 12, 2023

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
rm(svcat)

scallops <- scallops %>% 
  mutate(id = as.factor(ID),
         name = as.factor(SCIENTIFIC_NAME),
         cruise6 = as.factor(CRUISE6),
         cruise = as.factor(CRUISE),
         .keep = "unused")
