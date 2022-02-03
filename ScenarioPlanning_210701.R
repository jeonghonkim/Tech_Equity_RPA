

#################################################################################

# rm(list=ls())

suppressPackageStartupMessages({
  library(ipumsr)
  library(dplyr, warn.conflicts = FALSE)  
  library(sf)
  library(tidyr)
  library(devtools)
  library(ggplot2)
  library(mapview)
  library(stringi)
  library(tidyverse)
  library(leaflet)
  library(purrr)
  library(geosphere)
  library(rmapshaper)
  library(htmltools)
  library(scales)
  library(tmap)
  library(RColorBrewer)
  library(psych)
  library(reshape)
  library(rlang)
  library(purrr)
  library(timetk)
  library(kableExtra)
  library(highcharter)
  library(PerformanceAnalytics)
  library(httr)
  library(jsonlite)
  library(geojsonio)
  library(sp)
  library(mapview)
  library(tidycensus)
  library(tidyverse)
  library(tigris)
  library(rgdal)
  library(downloader)
  library(geojsonio)
  require(quantmod)
  options(tigris_class = "sf")
  options(tigris_use_cache = TRUE)
})
theme_set(theme_bw())

# Set file path
setwd("G:/Shared drives/Projects/5035_Tech Equity/Data/A_IPUMS_Data")

#################################################################################

# Microdata Analysis - RECS, CBECS

# 1) RECS

# 2) CBECS Microdata
# EIA makes a public-use microdata file available for each CBECS survey cycle. 
# The 2012 file, which contains over 400 survey variables (not including imputation flags and statistical weights, 
# which bring the variable count over 1,000), is a valuable tool for users conducting detailed analysis of energy use 
# in commercial buildings. This document provides some background on the CBECS design, as well as useful tips and examples 
# that will guide users through the proper statistical use of the CBECS microdata.

# The CBECS uses a multi-stage, multi-frame area probability design to select a sample of buildings 
# that estimate energy characteristics, consumption, and expenditures for the national stock of commercial buildings.


#################################################################################


Micro_RES15 <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/Scenarios_GHG/EIA/RECS_2015/Microdata/recs2015_public_v4.csv')
Micro_COM12 <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/Scenarios_GHG/EIA/CBECS_2012/Microdata/2012_public_use_data_aug2016.csv')

colnames(Micro_RES15)
colnames(Micro_COM12)

# Northeast Region
Micro_NE_RES15 <-
  Micro_RES15 %>%
  filter(REGIONC == 1)

Micro_NE_COM12 <-
  Micro_COM12 %>%
  filter(REGION == 1)


#################################################################################


# 1. RECS

# Ex1. Percentage of households that used natural gas as their main space heating fuel
eg1.1 <-
Micro_NE_RES15 %>%
  summarise_at(vars(NWEIGHT),             
               list(name = sum))

eg1.2 <-
Micro_NE_RES15 %>%
  filter(FUELHEAT == 1) %>%
  summarise_at(vars(NWEIGHT),             
               list(name = sum))

eg1.3 <- eg1.2/eg1.1
# 54% - NE
# 49% - US


# Ex2. Energy intensity by the number of household members in NE
eg2.1 <-
Micro_RES15 %>%
  filter(REGIONC == 1) %>%
  mutate(WNHSLDMEM = NHSLDMEM*NWEIGHT) %>%
  summarize_at(vars(WNHSLDMEM),
               list(name = sum)) # Weighted number of household members

eg2.2 <-
Micro_RES15 %>%
  filter(REGIONC == 1) %>%
  mutate(WTOTALBTU = TOTALBTU*NWEIGHT) %>%
  summarize_at(vars(WTOTALBTU),
               list(name = sum)) # Weighted total fuel consumptiono
  
eg2.3 <-
  eg2.2 / eg2.1

# 38,076 thousand BTU - NE
# 27,275 thousand BTU - S 

#################################################################################


# 2. CBECS

# Ex1. Number of commercial buildings that are 5,000 square feet or smaller
Micro_COM12 %>%
  filter(SQFTC == 2) %>%
  summarise_at(vars(FINALWT),             
               list(name = sum))


# Ex2. Total square footage of commercial buildings in the Northeast region
ex2 <-
Micro_COM12 %>%
  mutate(
    WSQFT = SQFT*FINALWT
  ) %>%
  filter(REGION == 1) %>%
  summarise_at(vars(WSQFT),             
               list(name = sum))


# Ex3. Average size (mean square feet per building) of office buildings in Northeast Region
ex3.1 <-
Micro_NE_COM12 %>%
  mutate(
    WSQFT = SQFT*FINALWT
  ) %>%
  filter(
    PBA == 02
  ) %>%
  summarise_at(vars(WSQFT),             
               list(name = sum)) # Total wegihted square footage

ex3.2 <-
Micro_NE_COM12 %>%
  filter(
    PBA == 02
  ) %>%
  summarise_at(vars(FINALWT),             
               list(name = sum)) # Total number of buildings

ex3.3 <-
  ex3.1 / ex3.2 # The average size of office building
# 25,354.17 - Northeast Region
# 15,811.59 - USA


# Ex4. Average electricity consumption for office buildings in the Midwest
ex4.1 <-
  Micro_COM12 %>%
    filter(
      REGION == 01,
      PBA == 02
    ) %>%
    mutate(
      WELCNS = ELCNS*FINALWT
    ) %>%
    summarise_at(vars(WELCNS),             
                 list(name = sum)) # Total electricity consumption for office buildings in NE

ex4.2 <-
  Micro_COM12 %>%
    filter(
      REGION == 01,
      PBA == 02
    ) %>%
    summarise_at(vars(FINALWT),             
                 list(name = sum)) # Total number of office buildings in NE

ex4.3 <-
  ex4.1 / ex4.2
# 428208.8 kWh of electricity annually

  
  # Ex5. Energy intensity for the sum of major fuels by building floorspace category

ex5.1 <-
Micro_NE_COM12 %>%
  filter(
    SQFTC == 04
    ) %>%
  mutate(
    WSQFT = SQFT*FINALWT,
  ) %>%
  summarise_at(vars(WSQFT),             
               list(name = sum)) # Sum of Square ft in the building floorspace category in NE
  
ex5.2 <-
  Micro_NE_COM12 %>%
  filter(
    SQFTC == 04
  ) %>%
  mutate(
    WMFBTU = MFBTU*FINALWT
  ) %>%
  mutate_at("WMFBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WMFBTU),             
               list(name = sum)) # Sum of weighted major fuel consumption in NE
  
ex5.3 <-
  ex5.2 / ex5.1  
  
# 66.46733 - NE
# 62.1031 - US



