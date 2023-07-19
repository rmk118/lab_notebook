#EDM help functions for analyzing the Maine inshore trawl survey data
#Ruby Krasnow
#Last modified: July 17, 2023

library(tidyverse)
library(lubridate)
library(tseries)
library(rEDM)
library(patchwork)

########### cleanCatch --------------------------------------------------------------

df_tows<-read.csv("data/Maine_inshore_trawl/MEtows.csv") #tow data
df_s_cat<- read.csv("data/Maine_inshore_trawl/MEscallopCatch.csv") #catch data of interest

cleanCatch <- function(x) {
  full_join(x, df_tows) %>%
    arrange(Survey, Tow_Number) %>% 
    select(-c("Stratum", "Subsample_Weight_kg", "Subsample_Weight_kg_2", "Male_Wt_kg", "Female_Wt_kg","Date", "Surface_WaterTemp_DegC", "Surface_Salinity", "End_Latitude","End_Longitude", "Air_Temp", "Tow_Time")) %>%
    mutate(Number_Caught = replace_na(Number_Caught,0),
           Weight_kg = replace_na(Weight_kg,0),
           Expanded_Catch = replace_na(Expanded_Catch,0),
           Expanded_Weight_kg = replace_na(Expanded_Weight_kg,0)) %>% 
    mutate(Stratum = Depth_Stratum, Date = date(ymd_hms(Start_Date)), .keep="unused") %>% 
    mutate(logCatch = log(Expanded_Catch + 1),
           logWt = log(Expanded_Weight_kg + 1)) %>%
    mutate(area = paste(Region, Stratum),.before= Survey)
}
 
#implementation example
s_cat_clean <- cleanCatch(df_s_cat) %>% 
 mutate(Common_Name = "Scallop")


########## summaryCatch ------------------------------------------------------------

summaryCatch <- function(df) {
  df %>% group_by(area, Season, Year, Region, Stratum) %>%
    summarise(avgCatch = mean(Expanded_Catch),
              avgWt = mean(Expanded_Weight_kg),
              avgLogCatch = mean(logCatch),
              avgLogWt = mean(logWt)) 
}

#implementation example
s_cat_sum <- summaryCatch(s_cat_clean)


########### findSpeciesMean ---------------------------------------------------------

#returns a tibble with the mean catch (avg across years) for each region/stratum combination

# actual function
findSpeciesMean<- function(df, season=NULL, type) {
  if (is.null(season)) {
    df_out <- df %>% 
      filter(Type == type) %>% 
      group_by(Region, Stratum) %>% 
      select(Year, value) %>%
      summarise(avg = mean(value, na.rm=TRUE)) %>%
      pivot_wider(names_from = Stratum, values_from = avg) %>% 
      ungroup() %>% 
      select(-Region)
    
  }
  
  else{
  df_out <- df %>% 
    filter(Type == type, Season == season) %>% 
    group_by(Region, Stratum) %>% 
    select(Year, value) %>%
    summarise(avg = mean(value, na.rm = TRUE)) %>%
    pivot_wider(names_from = Stratum, values_from = avg) %>% 
    ungroup() %>% 
    select(-Region) }
  
  return(df_out)
}

#implementation example

s_catchTidy <- pivot_longer(s_cat_sum, cols = 6:ncol(s_cat_sum)) %>% 
  mutate(Type = case_when(
    name== "avgCatch" ~"avgCatch",
    name=="avgWt" ~"avgWt",
    name=="avgLogWt" ~"avgLogWt",
    name=="avgLogCatch" ~"avgLogCatch")) %>% 
  mutate(Species = "scallop")

s_catchTidy <- s_catchTidy %>% 
  mutate(Species = as.factor(Species),
         Season = as.factor(Season),
         Region = as.factor(Region), 
         area = as.factor(area),
         Stratum = as.factor(Stratum)) %>% 
  select(-name)

findSpeciesMean(s_catchTidy, season="Fall", type="avgLogCatch")
# Check - the below call matches the corresponding cell in the output tibble
# mean(s_catchTidy %>% filter(Region==1, Stratum ==1, Season=="Fall", Type=="avgLogCatch") %>% pull(value))

############ Find E - df input -------------------------------------------------------

findE_df <- function(df) {
  lib_vec <- paste(1,nrow(df))
  rho_E<- EmbedDimension(dataFrame = df, lib = lib_vec, pred = lib_vec, columns = "value",target = "value")
  E_out<-rho_E[which.max(rho_E$rho),"E"][1]
  return(E_out)
}

#implementation example
scalLogCatchFall_1.1 <- s_catchTidy %>% 
  filter(Type=="avgLogCatch", Season=="Fall") %>% 
  select(-c('Species', 'Type')) %>% 
  filter(Region==1, Stratum==1) %>% 
  ungroup() %>% 
  select(Year, value)

findE_df(scalLogCatchFall_1.1)

############ Find E and rho - vector input -------------------------------------------------

findE_v <- function(v) {
  lib_vec <- paste(1, length(v))
  indices <- c(1:length(v))
  df <- data.frame(indices,v)
  colnames(df)<-c("index", "value")
  #print(df)
  rho_E<- EmbedDimension(dataFrame = df, lib = lib_vec, pred = lib_vec, columns = "value",target = "value", maxE = 7)
  E_out<-rho_E[which.max(rho_E$rho),"E"][1]
  return(E_out)
}

findErho_v <- function(v) {
  lib_vec <- paste(1, length(v))
  indices <- c(1:length(v))
  df <- data.frame(indices,v)
  colnames(df)<-c("index", "value")
  #print(df)
  rho_E<- EmbedDimension(dataFrame = df, lib = lib_vec, pred = lib_vec, columns = "value",target = "value", maxE = 7)
  rho_out<-rho_E[which.max(rho_E$rho),"rho"][1]
  return(rho_out)
}

#implementation examples
findE_v(scalLogCatchFall_1.1$value)
findErho_v(scalLogCatchFall_1.1$value)

findTheta_v <- function(v, E) {
  lib_vec <- paste(1, length(v))
  indices <- c(1:length(v))
  df <- data.frame(indices,v)
  colnames(df)<-c("index", "value")
  #print(df)
  rho_Theta<- PredictNonlinear(dataFrame = df, lib = lib_vec, pred = lib_vec, columns = "value",target = "value", E=E)
  Theta_out<-rho_Theta[which.max(rho_Theta$rho),"Theta"][1]
  return(Theta_out)
}

findThetaRho_v <- function(v, E) {
  lib_vec <- paste(1, length(v))
  indices <- c(1:length(v))
  df <- data.frame(indices,v)
  colnames(df)<-c("index", "value")
  #print(df)
  rho_Theta<- PredictNonlinear(dataFrame = df, lib = lib_vec, pred = lib_vec, columns = "value",target = "value", E=E)
  Rho_out<-rho_Theta[which.max(rho_Theta$rho),"rho"][1]
  return(Rho_out)
}
#implementation examples
findTheta_v(scalLogCatchFall_1.1$value, 2)
findThetaRho_v(scalLogCatchFall_1.1$value, 2)

############ findSpeciesE & findSpeciesErho -------------------------------------------------------

#returns a tibble with the optimal embedding dimension for time series from each region/stratum combination
#Same idea as the findSpeciesMean function, but finding the optimal embedding dimension instead of the mean

findSpeciesE <- function(df, season=NULL, type) {
  if (is.null(season)) {
    df_out <- df %>% 
      filter(Type == type) %>% 
      group_by(Region, Stratum) %>% 
      select(Year, value) %>%
      summarise(E_opt = findE_v(value)) %>%
      pivot_wider(names_from = Stratum, values_from = E_opt) %>% 
      ungroup() %>% 
      select(-Region)
  }
  else {
  
  df_out <- df %>% 
    filter(Type == type, Season == season) %>% 
    group_by(Region, Stratum) %>% 
    select(Year, value) %>%
    summarise(E_opt = findE_v(value)) %>%
    pivot_wider(names_from = Stratum, values_from = E_opt) %>% 
    ungroup() %>% 
    select(-Region) }
  
  return(df_out)
}

findSpeciesErho <- function(df, season=NULL, type) {
  if (is.null(season)) {
  df_out <- df %>% 
    filter(Type == type) %>% 
    group_by(Region, Stratum) %>% 
    select(Year, value) %>%
    summarise(E_opt_rho = findErho_v(value)) %>%
    pivot_wider(names_from = Stratum, values_from = E_opt_rho) %>% 
    ungroup() %>% 
    select(-Region) }
  
  else {
    df_out <- df %>% 
      filter(Type == type, Season == season) %>% 
      group_by(Region, Stratum) %>% 
      select(Year, value) %>%
      summarise(E_opt_rho = findErho_v(value)) %>%
      pivot_wider(names_from = Stratum, values_from = E_opt_rho) %>% 
      ungroup() %>% 
      select(-Region)
    
  }
  return(df_out)
}


#Implementation examples
par(mfrow=c(5,4), mar=c(0.6,1,0.4,0.4))
findSpeciesE(s_catchTidy, season="Fall", type="avgLogCatch")
findSpeciesErho(s_catchTidy, season="Fall", type="avgLogCatch")

#Implementation examples
par(mfrow=c(5,4), mar=c(0.6,1,0.4,0.4))
findSpeciesE(s_catchTidy, season="Fall", type="avgLogWt")
findSpeciesErho(s_catchTidy, season="Fall", type="avgLogWt")

############ findSpeciesTheta -------------------------------------------------------

findSpeciesTheta <- function(df, season=NULL, type) {
  df_E <- findSpeciesE(df=df, season=season, type=type)
  
  findE <- function (reg, strat) {
     E <- as.integer(df_E %>% slice(reg) %>% pull(strat))
    return(E) 
    }
  
  if (is.null(season)) {
    df_out <- df %>%
      filter(Type == type) %>%
      mutate(E_row = as.integer(Region), E_col = as.integer(Stratum)) %>% 
      rowwise() %>% 
      mutate(E = findE(reg=E_row, strat=E_col)) %>%
      group_by(Region, Stratum) %>%
      summarise(Theta_opt =  findTheta_v(value, E[1])) %>%
      pivot_wider(names_from = Stratum, values_from = Theta_opt) %>%
      ungroup() %>%
      select(-Region) }
  
  else {
      df_out <- df %>%
        filter(Type == type, Season==season) %>%
        mutate(E_row = as.integer(Region), E_col = as.integer(Stratum)) %>% 
        rowwise() %>% 
        mutate(E = findE(reg=E_row, strat=E_col)) %>%
        group_by(Region, Stratum) %>%
        summarise(Theta_opt =  findTheta_v(value, E[1])) %>%
        pivot_wider(names_from = Stratum, values_from = Theta_opt) %>%
        ungroup() %>%
        select(-Region) }
  
  return(df_out)
}

findSpeciesTheta_rho <- function(df, season=NULL, type) {
  df_E <- findSpeciesE(df=df, season=season, type=type)
  
  findE <- function (reg, strat) {
    E <- as.integer(df_E %>% slice(reg) %>% pull(strat))
    return(E) 
  }
  
  if (is.null(season)) {
    df_out <- df %>%
      filter(Type == type) %>%
      mutate(E_row = as.integer(Region), E_col = as.integer(Stratum)) %>% 
      rowwise() %>% 
      mutate(E = findE(reg=E_row, strat=E_col)) %>%
      group_by(Region, Stratum) %>%
      summarise(Theta_opt =  findThetaRho_v(value, E[1])) %>%
      pivot_wider(names_from = Stratum, values_from = Theta_opt) %>%
      ungroup() %>%
      select(-Region) }
  
  else {
    df_out <- df %>%
      filter(Type == type, Season==season) %>%
      mutate(E_row = as.integer(Region), E_col = as.integer(Stratum)) %>% 
      rowwise() %>% 
      mutate(E = findE(reg=E_row, strat=E_col)) %>%
      group_by(Region, Stratum) %>%
      summarise(Theta_opt =  findThetaRho_v(value, E[1])) %>%
      pivot_wider(names_from = Stratum, values_from = Theta_opt) %>%
      ungroup() %>%
      select(-Region) }
  
  return(df_out)
}

findSpeciesTheta(s_catchTidy, season="Fall", type="avgLogWt")
findSpeciesTheta_rho(s_catchTidy, season="Fall", type="avgLogWt")

############ findSpeciesACF -------------------------------------------------------

#returns a tibble with the p-val for the Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test for 
# time series from each region/stratum combination
#KPSS tests the null hypothesis that x is level stationary

findSpeciesKPSS <- function(df, season=NULL, type) {
  if (is.null(season)) {
    df_out <- df %>% 
      filter(Type == type) %>% 
      group_by(Region, Stratum) %>% 
      select(Year, value) %>%
      summarise(val = (kpss.test(value, null='Level'))$p.value) %>%
      pivot_wider(names_from = Stratum, values_from = val) %>% 
      ungroup() %>% 
      select(-Region) }
  else {
  df_out <- df %>% 
    filter(Type == type, Season == season) %>% 
    group_by(Region, Stratum) %>% 
    select(Year, value) %>%
    summarise(val = (kpss.test(value, null='Level'))$p.value) %>%
    pivot_wider(names_from = Stratum, values_from = val) %>% 
    ungroup() %>% 
    select(-Region) }
  return(df_out)
}
findSpeciesKPSS(s_catchTidy, season="Fall", type="avgLogWt")
findSpeciesKPSS(s_catchTidy, season="Fall", type="avgLogCatch")

# Function 1: delay -------------------------------------------------------

delay <- function(x,n){
  if(n>=0) 
    lead(x,n) 
  else 
    lag(x,abs(n))}


# Function 2A: make_xmap_block_noID ---------------------------------------------

make_xmap_block_noID <- function(df,predictor,target, E_max,cause_lag){
  
  v_delays <- 0:-(E_max-1)
  
    df_ccm <- df %>%
      select({{target}}) %>%
      transmute(target=delay({{target}},cause_lag))
    
    df_lags <- map_dfc(v_delays,function(d_i){
      df %>% transmute("pred_t{d_i}" := delay({{predictor}},d_i))
    })
  
  df_out <- bind_cols(df_ccm,df_lags) %>% 
    ungroup() %>%
    mutate(index=row_number()) %>%
    select(index,everything())
  return(df_out)
}
# Function 2B: make_xmap_block_ID ---------------------------------------------

make_xmap_block_ID <- function(df,predictor,target,ID_col,E_max,cause_lag){
  
  v_delays <- 0:-(E_max-1)
  
  df_ccm <- df %>%
    select({{ID_col}},{{target}}) %>%
    group_by({{ID_col}}) %>%
    transmute(target=delay({{target}},cause_lag))
  
  df_lags <- map_dfc(v_delays,function(d_i){
    df %>%
      group_by({{ID_col}}) %>%
      transmute("pred_t{d_i}" := delay({{predictor}},d_i)) %>%
      ungroup({{ID_col}}) %>% select(-{{ID_col}})
  })
  
  df_out <- bind_cols(df_ccm,df_lags) %>% 
    ungroup() %>%
    mutate(index=row_number()) %>%
    select(index,everything())
  
  return(df_out)
  
}

# Function 3A: do_xmap no ID col------------------------------------------------

do_xmap_noID <- function(df,predictor,target,E_max,tp,keep_preds=FALSE){

    df_2 <- make_xmap_block_noID(df,!!sym(predictor),!!sym(target),E_max,cause_lag=tp) %>% ungroup()
    df_1 <- df_2 %>%
      mutate(target=lag(target,1)) %>% 
      ungroup() %>%
      filter(complete.cases(.))
    
  df_2 <- df_2 %>% filter(complete.cases(.))
  
  lib_1 <- paste(1,nrow(df_1))
  lib_2 <- paste(1,nrow(df_2))
  
  out_1 <- map_df(1:E_max,function(E_i){
  columns_i <- names(df_1)[3:(E_i+2)]
    out_i <- Simplex(dataFrame=df_1,
                     lib=lib_1,pred=lib_1,Tp=0, # The target has already been "manually" lagged
                     target="target",
                     columns=columns_i,
                     embedded=TRUE,
                     parameterList = TRUE,
                     E=E_i)
    
    params_i <- out_i$parameters
    out_i <- out_i$predictions %>% filter(complete.cases(.))
    
    stats_i <- compute_stats(out_i$Observations,out_i$Predictions)
    
    return(bind_cols(data.frame(E=E_i),stats_i))
    
  })
  
  E_star <- out_1 %>% top_n(1,rho) %>% pull(E)
    columns_star <- names(df_1)[3:(E_star+3)]
  
  out <- Simplex(dataFrame=df_2,
                 lib=lib_2,pred=lib_2,Tp=0, # The target has already been "manually" lagged
                 target="target",
                 columns=columns_star,
                 embedded=TRUE,
                 parameterList = TRUE,
                 E=E_star)
  
  fit_linear <- lm(as.formula(paste0("target ~ ",columns_star[1])),data=df_2)
  out_linear_predictions <- predict(fit_linear)
  
  params <- out$parameters
  out <- out$predictions %>% filter(complete.cases(.))
  
  stats <- compute_stats(out$Observations,out$Predictions)
  
  stats_linear <- compute_stats(out$Observations,out_linear_predictions)
  names(stats_linear) <- paste0(names(stats_linear),"_linear")
  
  if(keep_preds){
    return(list(
      stats=bind_cols(Filter(function(x) length(x)==1,params),stats,stats_linear),
      preds=out
    ))
  }
  else{
    return(bind_cols(Filter(function(x) length(x)==1,params),stats,stats_linear))
  }
  
}

# Function 3B: do_xmap with ID col------------------------------------------------

do_xmap_ID <- function(df,predictor,target,ID_col,E_max,tp,keep_preds=FALSE){

  df_2 <- make_xmap_block_ID(df,!!sym(predictor),!!sym(target),!!sym(ID_col),
                          E_max,cause_lag=tp) %>% ungroup()
  
  ## for selecting the optimal E, we discard rows that do not have a full set of lags so that the prediction set is consistent for the comparison.
  
  df_1 <- df_2 %>% group_by(!!sym(ID_col)) %>%
    mutate(target=lag(target,1)) %>% 
    ungroup() %>%
    filter(complete.cases(.))
  
  df_2 <- df_2 %>% filter(complete.cases(.))
  
  lib_1 <- paste(1,nrow(df_1))
  lib_2 <- paste(1,nrow(df_2))
  
  out_1 <- map_df(1:E_max,function(E_i){

    columns_i <- names(df_1)[4:(E_i+3)]
    out_i <- Simplex(dataFrame=df_1,
                     lib=lib_1,pred=lib_1,Tp=0, # The target has already been "manually" lagged
                     target="target",
                     columns=columns_i,
                     embedded=TRUE,
                     parameterList = TRUE,
                     E=E_i)

    params_i <- out_i$parameters
    out_i <- out_i$predictions %>% filter(complete.cases(.))

    stats_i <- compute_stats(out_i$Observations,out_i$Predictions)

    return(bind_cols(data.frame(E=E_i),stats_i))

  })

  E_star <- out_1 %>% top_n(1,rho) %>% pull(E)
  
 
  columns_star <- names(df_1)[4:(E_star+3)]

  out <- Simplex(dataFrame=df_2,
                 lib=lib_2,pred=lib_2,Tp=0, # The target has already been "manually" lagged
                 target="target",
                 columns=columns_star,
                 embedded=TRUE,
                 parameterList = TRUE,
                 E=E_star)

  fit_linear <- lm(as.formula(paste0("target ~ ",columns_star[1])),data=df_2)
  out_linear_predictions <- predict(fit_linear)

  params <- out$parameters
  out <- out$predictions %>% filter(complete.cases(.))

  stats <- compute_stats(out$Observations,out$Predictions)

  stats_linear <- compute_stats(out$Observations,out_linear_predictions)
  names(stats_linear) <- paste0(names(stats_linear),"_linear")

  if(keep_preds){
    return(list(
      stats=bind_cols(Filter(function(x) length(x)==1,params),stats,stats_linear),
      preds=out
    ))
  }
  else{
    return(bind_cols(Filter(function(x) length(x)==1,params),stats,stats_linear))
  }
}


# Function 4: do_randL_xmap_once ------------------------------------------

do_randL_xmap_once <- function(df,predictor,target,ID_col=NULL,lib_size,E,tp,keep_preds=FALSE){
  
  df_2 <- make_xmap_block(df,!!sym(predictor),!!sym(target),!!sym(ID_col),
                          E,cause_lag=tp) %>% ungroup()
  
  df_2 <- df_2 %>%
    filter(complete.cases(.)) %>%
    slice_sample(prop=1)

  lib_L <- paste(1,lib_size)
  pred_L <- paste(1,nrow(df_2))
  
  columns_i <- names(df_2)[4:(E+3)]
  out <- Simplex(dataFrame=df_2,
                 lib=lib_L,pred=pred_L,
                 Tp=0,
                 target="target",
                 columns=columns_i,
                 embedded=TRUE,
                 parameterList = TRUE,
                 E=E)
  
  params <- out$parameters
  out <- out$predictions %>% filter(complete.cases(.))
  
  stats <- compute_stats(out$Observations,out$Predictions)
  stats$lib_size = lib_size
  stats$E=E
  stats$Tp=0
  
  return(stats)
  
}

# # Function 5: do_extend_xmap ----------------------------------------------
# 
# do_extend_xmap <- function(df,predictor,target,ID_col,E_max=12,cause_lag_max=10){
#   
#   ## make block
#   out <- map_dfr(-cause_lag_max:0,function(lag_i){
#     out_i <- do_xmap_once(df,predictor,target,ID_col,E_max,tp=lag_i)
#     out_i$Tp = lag_i
#     return(out_i)
#   })
#   
#   return(out)
# }
# 
