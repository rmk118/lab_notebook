# Adopted from code written by Ysabel Wolfing, Jacoob Nettles, and Maria Hernandez
# for BU BI521 F2022

# sex,sex_description
# 0,"Unsexed, unknown, or sex not observed; Since the summer 1995 Gulf of Maine Trawl Survey forgot to look for American Lobster"
# 1,"Male"
# 2,"Female; Female Stage I for Northern Shrimp; Since the summer 1995 Gulf of Maine Trawl Survey Female (no eggs, no notch) American Lobster"
# 3,"Female Stage II for Northern Shrimp; Since the summer 1995 Gulf of Maine Trawl Survey Female, with eggs, no notch American Lobster"
# 4,"Transitional for Northern Shrimp; Since the summer 1995 Gulf of Maine Trawl Survey Female, with notch, no eggs American Lobster"
# 5,"Ovigerous for Northern Shrimp; Since the summer 1995 Gulf of Maine Trawl Survey Female, with notch and eggs American Lobster"
# 6,"Non-spawning Female for Northern Shrimp"
# 7,"Female for Northern Shrimp not staged (stage I or II not determined)"


setwd("/Users/Shared/Research/Ecosystem Based Management/GoME_BOEM")

library("ggplot2")
library("tidyverse")
library("lubridate")
library("sf")
# library(xtable)

df_catches <- read.csv("./NEFSC_trawls/Data/22560_FSCSTables_Trawl_Fall/22560_UNION_FSCS_SVCAT.csv") %>% tibble()
df_stations <- read.csv("./NEFSC_trawls/Data/22560_FSCSTables_Trawl_Fall/22560_UNION_FSCS_SVSTA.csv") %>% tibble()
df_cruises <- read_csv("./NEFSC_trawls/Data/22560_FSCSTables_Trawl_Fall/22560_SVDBS_CRUISES.csv") 
df_strata <- read.csv("./NEFSC_trawls/Data/SVDBS_SupportTables/SVDBS_SVMSTRATA.csv") %>% tibble()

map_GoME <- st_read("./SIEBA_BaseMap/MIMES_MAP_5.shp")
map_strata <- st_read("./NEFSC_trawls/Data/NMFS_strata/strata.shp",stringsAsFactors=FALSE)

if(FALSE){
  df_species_ID <- read_csv("./NEFSC_trawls/Data/SVDBS_SupportTables/SVDBS_SVSPECIES_LIST.csv")
  (df_species_ID) %>% filter(str_detect(COMNAME,"AMERICAN LOBSTER")) %>% pull(SVSPP)
}

# Set data extraction parameters

target_species_ID <- 301

# target_chunks <- list("90s" = 1990:1999,"00s"=2000:2009,"10s"=2010:2019)
target_chunks <- list(
  # "early-90" = 1990:1994,
  "late-90" = 1995:1999,
  "early-00"=2000:2004,
  "late-00"=2005:2009,
  "early-10s"=2010:2014,
  "late-10s"=2015:2019
)

df_stations_wrangled <- df_stations %>%
  filter(EST_YEAR %in% unlist(target_chunks)) %>%
  mutate(DATE = parse_date_time(BEGIN_EST_TOWDATE,orders="mdYHMS",truncated = 3)) %>%
  select(STATION,STRATUM,DATE) %>%
  mutate(DATE=year(DATE)) %>%
  distinct()



#pulls data where there is all 60 years of data collection for a single station 
strata_core <- df_stations_wrangled %>%
  select(DATE,STRATUM) %>%
  distinct() %>%
  group_by(STRATUM) %>%
  summarise(count=n()) %>%
  filter(count >= length(unique(unlist(target_chunks)))) %>%
  left_join(df_strata %>% select(stratum,stratum_name,stratum_area,midlat,midlon),by=c("STRATUM" = "stratum")) %>%
  # mutate(STRATUM = as.numeric(STRATUM))
  {.}

df_tows_wrangled <- df_stations %>%
  filter(EST_YEAR %in% unlist(target_chunks)) %>%
  mutate(DATE = parse_date_time(BEGIN_EST_TOWDATE,orders="mdYHMS",truncated = 3)) %>%
  select(CRUISE6,STATION,STRATUM,DATE,TOW,STATION,TOWDUR) %>%
  mutate(DATE=year(DATE)) %>%
  distinct() %>%
  {.}

# use left_join operation to get NAs for true 0s, then convert.

df_catches_wrangled <- df_catches %>%
  filter(SVSPP == target_species_ID) %>%
  filter(STRATUM %in% strata_core$STRATUM) %>%
  group_by(CRUISE6,STRATUM,STATION,TOW) %>%
  summarise(across(starts_with("EXPCATCH"),~sum(.x)))



df_lobster <- df_tows_wrangled %>%
  left_join(df_catches_wrangled,by=c("CRUISE6","STRATUM","STATION","TOW")) %>%
  # mutate(across(starts_with("EXPCATCH"),~replace(.,is.na(.),0))) %>%
  group_by(CRUISE6,STRATUM)

df_lobster_decade <- df_lobster %>%
  left_join(tibble(DECADE=names(target_chunks),DATE=target_chunks) %>%
              unnest(cols=c(DATE)) %>% mutate(DATE=as.numeric(DATE))) %>%
  select(DECADE,everything()) %>%
  group_by(DECADE,STRATUM) %>%
  mutate(across(starts_with("EXPCATCH"),~(./TOWDUR))) %>%
  summarise(across(starts_with("EXPCATCH"),~mean(.,na.rm=TRUE)),.groups="keep") %>%
  ungroup() %>%
  # left_join(select(df_stations,CRUISE6,STRATUM,STATION,
  #                  DECDEG_BEGLAT,DECDEG_BEGLON),
  #           by=c("CRUISE6","STRATUM","STATION")) %>%
  # ungroup() %>%
  mutate(DECADE = factor(DECADE,levels=names(target_chunks))) %>%
  # select(-STATION) %>%
  {.}


st_on_GoME <- merge(map_strata %>% rename(STRATUM=STRATA),
                    df_lobster_decade,by="STRATUM") %>%
  st_transform(st_crs(map_GoME))
# st_on_GoME <- full_join(map_strata %>% rename(STRATUM=STRATA),df_lobster_decade,by="STRATUM")

sf_lobster_decade <- df_lobster_decade %>% 
  select(DECDEG_BEGLAT,DECDEG_BEGLON,everything()) %>%
  st_as_sf(coords=c("DECDEG_BEGLON","DECDEG_BEGLAT"),crs=st_crs(4326))

sf_lobster_decade <- st_transform(sf_lobster_decade,st_crs(map_GoME))

st_on_MIMES <- st_join(map_GoME,st_on_GoME)

st_all_overlaps <- st_intersection(map_GoME,st_on_GoME) %>%
  mutate(area = st_area(geometry) %>% drop_units()) %>%
  mutate(across(starts_with("EXPCATCH"),~(.*area))) %>%
  {.}

st_area_weighted <- st_all_overlaps %>%
  group_by(MIMES_ID,DECADE) %>%
  summarise(across(starts_with("EXPCATCH"),~mean(.,na.rm=TRUE)),.groups = "keep") %>%
  select(DECADE,MIMES_ID,starts_with("EXPCATCH"),geometry) %>%
  mutate(area = st_area(geometry) %>% drop_units()) %>%
  mutate(across(starts_with("EXPCATCH"),~(./area)))

# st_on_GoME <- st_join(map_GoME,sf_lobster_decade) %>%
#   group_by(MIMES_ID,DECADE) %>%
#   summarise(across(starts_with("EXPCATCH"),~mean(.,na.rm=TRUE)),.groups = "keep")

ggplot(st_on_MIMES) + geom_sf(aes(fill=60*EXPCATCHWT)) + facet_wrap(~DECADE) + scale_fill_viridis_c(trans="sqrt")

# ggplot(st_on_GoME,aes(geometry=geometry)) + geom_sf(aes(fill=60*EXPCATCHWT)) + facet_wrap(~DECADE) + scale_fill_viridis_c(trans="sqrt")
ggplot(st_area_weighted,aes(geometry=geometry)) + geom_sf(aes(fill=EXPCATCHWT)) + facet_wrap(~DECADE) + scale_fill_viridis_c(trans="sqrt")

if(FALSE){
write_csv(x=st_drop_geometry(st_area_weighted) %>% 
            # rename(!!df_extensions$var_label[[i_folder]] := x) %>%
            select(DECADE,MIMES_ID,everything()) %>%
            arrange(DECADE,MIMES_ID),
          file=paste0("./NEFSC_trawls/lobster_half_decade_by_strata.csv"),
          append = FALSE)
}
