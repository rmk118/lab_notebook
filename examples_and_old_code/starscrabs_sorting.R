#Dredge data sorting 2000-2022
#Ruby Krasnow
#Last updated: July 4, 2023

library(tidyverse)

#Read data
df_data_all<- read.csv("data/starscrabs0022.csv")

#Focus only on stations since 2003, and on stations where starsample is FALSE
df_data_unordered <- df_data_all %>% 
  filter(year > 2002) %>% 
  filter(starsample == FALSE)

#Reorder and select columns to more easily identify questionable stations
colOrder<-c("year", "station", "starsample", "catchcomment", "stacomment", "Crabnum", "Asteriasnum", 
            "Astropectennum", "Scallopbms","Scallopnum",
            "Crabbms", "Asteriasbms", "Astropectenbms", "Poutnum","Poutbms",
            "Leptastariasnum","Leptastariasbms","cruise6")

df_data<- df_data_unordered %>% select(all_of(colOrder)) %>% 
  mutate(Scallopnum = format(Scallopnum, scientific = F)) 

# Manually identified all stations that had a comment in the catchcomment or stacomment column
# that mentioned stars and/or crabs
df_data_flagged_comment <- df_data %>% 
   filter((year == 2003 & station == 238) |
          (year == 2010 & (station %in% c(430,495))) |
          (year == 2011 & (station %in% c(196,266))) |
          (year == 2012 & (station %in% c(43,73))) |
          (year == 2013 & (station %in% c(63, 68, 107, 165, 166))) | # No stars/crabs 2013: 83
          (year == 2015 & (station %in% c(114, 139, 186))) | 
            #No stars/crabs 2015: 40, 49, 61, 77, 129, 137,157, 164, 166, 175, 179, 191, 194
          (year == 2016 & station == 52) |
            #No stars/crabs 2016: 10, 24, 39, 54, 56, 61, 63, 66
          (year == 2018 & (station %in% c(107, 111, 153))) |
          #No stars/crabs 2018: 92, 121
            (year == 2019 & station ==50) |
            (year == 2022 & (station %in% c(99, 127))))

#confirm no instances were missed
# test2<- df_data %>% filter(year < 2019) %>% 
#   filter(str_detect(catchcomment, "star[^t]|crab") | str_detect(stacomment, "star[^t]|crab"))
# anti_join(test2, df_data_flagged_comment)


# Identifying runs of >=3 consecutive stations ----------------------------

##### Method 1
stationsList <- df_data$station #length = 3077 stations

diffs<- c(diff(stationsList),0)
diffsTable <- data.frame(df_data$year, stationsList, diffs)
diffsTable$diffsOne <- (diffsTable$diffs ==1)

indices = c()
for (i in 1:(length(diffsTable$diffs)-3)) {
  if (diffsTable$diffsOne[i]==TRUE & diffsTable$diffsOne[i+1]==TRUE & diffsTable$diffsOne[i+2]==TRUE) {
    indices<- append(indices, i) 
  }
}
#If you only want runs of >3 consecutive stations, add & diffsTable$diffsOne[i+3]==TRUE

diffsTable$flag = NA
diffsTable <- diffsTable %>% mutate(flag = row_number() %in% indices)

df_data_consecutive <- diffsTable %>% 
  filter(flag == TRUE) #A df with 95 rows

finalConsecutive <- df_data_consecutive %>%  
  group_by(df_data.year) %>%  
  filter(!(stationsList %in% (stationsList+1))) #A df with 49 rows

##### Method 2 - to validate the results of Method 1

result <- rle(diff(stationsList)) #2632 runs of 1 or more equal values
result$lengths #how long each run is
result$values #the repeated value (where values are the differences between station numbers)

summary(result$lengths == 3 & result$values ==1) #24 * (3-2) = 24 rows
summary(result$lengths == 4 & result$values ==1) #14 * (4-2) = 28 rows
summary(result$lengths == 5 & result$values ==1) #4 * (5-2) = 12 rows
summary(result$lengths == 6 & result$values ==1) #4 * (6-2) = 16 rows
summary(result$lengths == 7 & result$values ==1) #3 * (7-2) = 15 rows
#24 + 28 + 12 + 16 + 15 = 95, so we should get a data table with 95 rows 
#as we did with df_data_consecutive

summary(result$lengths > 2 & result$values==1)
#So there should be 49 unique instances, as there are in finalConsecutive

############################ Final output #############################

# Data table listing the first station in runs of three or more consecutive 
# stations maked as "starsample = FALSE"
finalConsecutive <- finalConsecutive %>% ungroup() %>% 
  mutate(year = df_data.year, firstStation = stationsList, .keep="none")

tibble(finalConsecutive)

# Data table with the stations 2003-2018 that were flagged based on a mention of 
# the presence or absence of stars and/or crabs in the catchcomment or stacomment columns
tibble(df_data_flagged_comment)



# Updating consecutive table to account for flagged stations (no comment = NC) --------------


dataNC<- anti_join(df_data, df_data_flagged_comment)

stationsListNC <- dataNC$station #length = 3055 stations

diffsNC<- c(diff(stationsListNC),0)
diffsTableNC <- data.frame(dataNC$year, stationsListNC, diffsNC)
diffsTableNC$diffsOne <- (diffsTableNC$diffsNC ==1)

indicesNC = c()
for (i in 1:(length(diffsTableNC$diffsNC)-3)) {
  if (diffsTableNC$diffsOne[i]==TRUE & diffsTableNC$diffsOne[i+1]==TRUE & diffsTableNC$diffsOne[i+2]==TRUE) {
    indicesNC<- append(indicesNC, i) 
  }
}
#If you only want runs of >3 consecutive stations, add & diffsTable$diffsOne[i+3]==TRUE

diffsTableNC$flag = NA
diffsTableNC <- diffsTableNC %>% mutate(flag = row_number() %in% indicesNC)

df_data_consecutiveNC <- diffsTableNC %>% 
  filter(flag == TRUE) #A df with 95 rows

finalConsecutiveNC <- df_data_consecutiveNC %>%  
  group_by(dataNC.year) %>%  
  filter(!(stationsListNC %in% (stationsListNC+1))) %>% ungroup() %>% 
  mutate(year = dataNC.year, firstStation_updated = stationsListNC, .keep="none")
