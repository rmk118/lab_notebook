#Examples/practice from Ecosystem-based forecasts of recruitment in two menhaden
# species (Deyle et al., 2018) https://onlinelibrary.wiley.com/doi/10.1111/faf.12287 
#Ruby Krasnow
#6/22/23

load("~/Downloads/lab_notebook/example_code/faf12287-sup-0002-datas1.rdata")

library("dplyr")
library("ggplot2")
library("grid")
library("gridExtra")
library("stringr")
library("purrr")
library('png')
#library('rgl')
library('parallel')
library('rEDM')

df.plot <- bind_rows(
  bio.Atl %>%
    mutate(LPUE = LPUE / sd(LPUE, na.rm = TRUE),
           JAI = JAI / sd(JAI, na.rm = TRUE)) %>%
    select(Year, LPUE, JAI) %>%
    tidyr::gather(key = var, value = value, LPUE, JAI) %>%
    mutate(region = "Atlantic"),
  bio.Gulf.yt %>%
    mutate(LPUE = LPUE / sd(LPUE, na.rm = TRUE),
           JAI = JAI / sd(JAI, na.rm = TRUE)) %>%
    select(Year, LPUE, JAI) %>%
    tidyr::gather(key = var, value = value, LPUE, JAI) %>%
    mutate(region = "Gulf"),
  bio.Gulf.nt %>%
    mutate(`JAI without Texas` = JAI / sd(JAI, na.rm = TRUE)) %>%
    select(Year, `JAI without Texas`) %>%
    tidyr::gather(key = var, value = value, `JAI without Texas`) %>%
    mutate(region = "Gulf")
)

f1_a <- df.plot %>%
  filter(region == "Atlantic") %>%
  ggplot(aes(x=Year,y=value,col=var)) +
  geom_line(linewidth=1) +
  labs(y = "Normalized abundance") +
  theme_bw() +
  theme(  legend.text = element_text(size = 8),
          legend.background = element_rect(color = 'black',linewidth =.1),
          legend.key.height = unit(0.5, "cm"),
          legend.key.width = unit(0.5, "cm"),
          legend.justification=c(0.1,0.1), legend.position=c(0.1,0.1) )

f1_b <- bio.Atl %>%
  ggplot(aes(x=LPUE,y=JAI)) +
  geom_point() +
  theme_bw()

f1_c <- df.plot %>%
  filter(region == "Gulf") %>%
  ggplot(aes(x=Year,y=value,col=var)) +
  geom_line(lwd=1) +
  labs(y = "Normalized abundance") +
  theme_bw() +
  theme(  legend.text = element_text(size = 8),
          legend.background = element_rect(color = 'black',size=.1),
          legend.key.height = unit(0.5, "cm"),
          legend.key.width = unit(0.5, "cm"),
          legend.justification=c(0.1,0.1), legend.position=c(0.1,0.1) )

f1_d <- bio.Gulf.yt %>%
  ggplot(aes(x=LPUE,y=JAI)) +
  geom_point() +
  theme_bw()

grid.arrange(grobs=list(f1_a,f1_b,f1_c,f1_d), ncol=2,widths = c(2.2,1))


# Univariate analysis -----------------------------------------------------

#Set up univariate analysis function

do_univariate_analysis <- function(ts, E.list = 1:8, predict_diff = FALSE, ...) {
  ts <- ts / sd(ts, na.rm = TRUE)
  if(predict_diff){
    simplex_out <- simplex_deltas(ts, E=E.list, ...)$delta_stats
  }else{
    simplex_out <- simplex(ts, E = E.list, ...)
  }
  E.star <- simplex_out$E[which.max(simplex_out$rho)]
  
  smap_out <- if(predict_diff){
    s_map_deltas(ts, E=E.star, ...)$delta_stats
  }else{
    s_map(ts, E = E.star, ...)
  }
  return(list(simplex = simplex_out,smap = smap_out)) }


#Checking for autocorrelation - my addition to see how they knew to use first differences for LPUE but not JAI
acf(bio.Atl$LPUE)
acf(na.omit(bio.Atl$JAI))
acf(bio.Gulf.nt$LPUE)
acf(bio.Gulf.yt$LPUE)
acf(na.omit(bio.Gulf.nt$JAI))
acf(na.omit(bio.Gulf.yt$JAI))

# 3.2 Do univariate analysis on each abundance time series
# We apply the do_univariate_analysis function now to each of the four biological time series of interest.
# Note that since the adult indices (LPUE) show very strong auto-correlation, we use the first-difference approach for these. # # To make plotting with ggplot2 easier later on, we set the results up to go into ‘long’ form, where the columns denote the # # species, variable, and method details.

# RETURNS ERRORS: the JAI ones don't work b/c of the NA values and when predict_diff = TRUE for the LPUE, it says there is an error in the help function simplex_deltas

# results <- list(do_univariate_analysis(bio.Atl$JAI, silent = TRUE) %>% 
#                   lapply(function(df) mutate(df,species = "Atlantic", variable = "JAI", method = "normal")),
#                 do_univariate_analysis(bio.Gulf.nt$JAI, silent = TRUE) %>%
#                   lapply(function(df) mutate(df,species = "Gulf", variable = "JAI without Texas", method = "normal")),
#                 do_univariate_analysis(bio.Gulf.yt$JAI, silent = TRUE) %>% 
#                   lapply(function(df) mutate(df,species = "Gulf", variable = "JAI", method = "normal")),
#                 do_univariate_analysis(bio.Atl$LPUE, predict_diff = TRUE, silent = TRUE) %>%
#                   lapply(function(df) mutate(df,species = "Atlantic", variable = "LPUE", method = "diff")),
#                 do_univariate_analysis(bio.Gulf.yt$LPUE, predict_diff = TRUE, silent = TRUE) %>%
#                   lapply(function(df) mutate(df,species = "Gulf", variable = "LPUE", method = "diff")))

# CHANGED TO ALL NORMAL

# results <- list(do_univariate_analysis(bio.Atl$JAI, silent = TRUE) %>% 
#                   lapply(function(df) mutate(df,species = "Atlantic", variable = "JAI", method = "normal")),
#                 do_univariate_analysis(bio.Gulf.nt$JAI, silent = TRUE) %>%
#                   lapply(function(df) mutate(df,species = "Gulf", variable = "JAI without Texas", method = "normal")),
#                 do_univariate_analysis(bio.Gulf.yt$JAI, silent = TRUE) %>% 
#                   lapply(function(df) mutate(df,species = "Gulf", variable = "JAI", method = "normal")),
#                 do_univariate_analysis(bio.Atl$LPUE, predict_diff = TRUE, silent = TRUE) %>%
#                   lapply(function(df) mutate(df,species = "Atlantic", variable = "LPUE", method = "diff")),
#                 do_univariate_analysis(bio.Gulf.yt$LPUE, predict_diff = TRUE, silent = TRUE) %>%
#                   lapply(function(df) mutate(df,species = "Gulf", variable = "LPUE", method = "diff")))

results<- list(do_univariate_analysis(bio.Atl$LPUE, predict_diff = FALSE, silent = TRUE)  %>%
                     lapply(function(df) mutate(df,species = "Atlantic", variable = "LPUE", method = "normal")),
               do_univariate_analysis(bio.Gulf.yt$LPUE, predict_diff = FALSE, silent = TRUE) %>%
                     lapply(function(df) mutate(df,species = "Gulf", variable = "LPUE", method = "normal")))

# results2<- do_univariate_analysis(bio.Atl$LPUE, predict_diff = FALSE, silent = TRUE)
# failed attempt at debugging
# results <- list(simplex = do.call(rbind, lapply(results,function(L) L[[1]]$simplex)), 
#                 smap = do.call(rbind,lapply(results, function(L) {
#     L[[1]]$smap %>% mutate(drho = rho - first(rho),
#                      dmae = mae - first(mae),
#                      drmse = rmse - first(rmse))
#   })) )
