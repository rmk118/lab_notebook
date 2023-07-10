#Maine spatial CCM - Fall
#Ruby Krasnow
#Last modified: July 10, 2023

library(tidyverse)
library(lubridate)
library(plotly)
library(patchwork)
library(car)

#spatial packages
library(sf)
library(sfheaders)
library(spdep)

#import help functions and data
source('~/Downloads/lab_notebook/Maine/helpFunctionsMaine.R')

#start with most basic and add levels of spatial complexity
#First, average across all tows, regions, and strata
View(logWtFall)

#then do multispatial using the regions as replicates

#multispatial using the strata as replicates

#multispatial using the study areas as replicates

#region 1.1 only, no spatial component

#region 1.1 CCM with bootstrapping

#all regions with bootstrapping

