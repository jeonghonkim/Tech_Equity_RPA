

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
Build_FootP <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/Scenarios_GHG/EIA/RECS_2015/building.csv')

colnames(Micro_RES15)
colnames(Micro_COM12)


#################################################################################

colnames(Micro_COM12)

# 1. Calculate average total marjor energy consumption per square foot by floorspace in NE

# 1) 1,001 to 5,000
cb1.0 <-
Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(SQFTC == "2") %>%
  mutate(
    WSQFT = SQFT * FINALWT
  ) %>%
  summarise_at(vars(WSQFT),
               list(name = sum)) # Total buliding square feet 
cb1.1 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(SQFTC == "2") %>%
  mutate(
    WMFBTU = MFBTU*FINALWT
  ) %>%
  mutate_at("WMFBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WMFBTU),
               list(name = sum)) # Total major fuel consumption 
cb1.2 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(SQFTC == "2") %>%
  mutate(
    WELBTU = ELBTU*FINALWT
  ) %>%
  mutate_at("WELBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WELBTU),
               list(name = sum)) # Total electricity consumption 
cb1.3 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(SQFTC == "2") %>%
  mutate(
    WNGBTU = NGBTU*FINALWT
  ) %>%
  mutate_at("WNGBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WNGBTU),
               list(name = sum)) # Total natural gas consumption 

cb1.1.0 <-
  cb1.1 / cb1.0 # Average major energy consumption per square foot in NE
cb1.2.0 <-
  cb1.2 / cb1.0 # Average electricity consumption per square foot in NE
cb1.3.0 <-
  cb1.3 / cb1.0 # Average natural gas consumption per square foot in NE



# 2) 5,001 to 10,000
cb2.0 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(SQFTC == "3") %>%
  mutate(
    WSQFT = SQFT * FINALWT
  ) %>%
  summarise_at(vars(WSQFT),
               list(name = sum)) # Total building square feet 
cb2.1 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(SQFTC == "3") %>%
  mutate(
    WMFBTU = MFBTU*FINALWT
  ) %>%
  mutate_at("WMFBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WMFBTU),
               list(name = sum)) # Total major fuel consumption 
cb2.2 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(SQFTC == "3") %>%
  mutate(
    WELBTU = ELBTU*FINALWT
  ) %>%
  mutate_at("WELBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WELBTU),
               list(name = sum)) # Total electricity consumption 
cb2.3 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(SQFTC == "3") %>%
  mutate(
    WNGBTU = NGBTU*FINALWT
  ) %>%
  mutate_at("WNGBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WNGBTU),
               list(name = sum)) # Total natural gas consumption 

cb2.1.0 <-
  cb2.1 / cb2.0 # Average major energy consumption per square foot in NE
cb2.2.0 <-
  cb2.2 / cb2.0 # Average electricity consumption per square foot in NE
cb2.3.0 <-
  cb2.3 / cb2.0 # Average natural gas consumption per square foot in NE



# 3) 10,001 to 25,000
cb3.0 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(SQFTC == "4") %>%
  mutate(
    WSQFT = SQFT * FINALWT
  ) %>%
  summarise_at(vars(WSQFT),
               list(name = sum)) # Total building square feet 
cb3.1 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(SQFTC == "4") %>%
  mutate(
    WMFBTU = MFBTU*FINALWT
  ) %>%
  mutate_at("WMFBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WMFBTU),
               list(name = sum)) # Total major fuel consumption 
cb3.2 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(SQFTC == "4") %>%
  mutate(
    WELBTU = ELBTU*FINALWT
  ) %>%
  mutate_at("WELBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WELBTU),
               list(name = sum)) # Total electricity consumption 
cb3.3 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(SQFTC == "4") %>%
  mutate(
    WNGBTU = NGBTU*FINALWT
  ) %>%
  mutate_at("WNGBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WNGBTU),
               list(name = sum)) # Total natural gas consumption 

cb3.1.0 <-
  cb3.1 / cb3.0 # Average major energy consumption per square foot in NE
cb3.2.0 <-
  cb3.2 / cb3.0 # Average electricity consumption per square foot in NE
cb3.3.0 <-
  cb3.3 / cb3.0 # Average natural gas consumption per square foot in NE



# 4) 25,001 to 50,000
cb4.0 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(SQFTC == "5") %>%
  mutate(
    WSQFT = SQFT * FINALWT
  ) %>%
  summarise_at(vars(WSQFT),
               list(name = sum)) # Total building square feet 
cb4.1 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(SQFTC == "5") %>%
  mutate(
    WMFBTU = MFBTU*FINALWT
  ) %>%
  mutate_at("WMFBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WMFBTU),
               list(name = sum)) # Total major fuel consumption 
cb4.2 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(SQFTC == "5") %>%
  mutate(
    WELBTU = ELBTU*FINALWT
  ) %>%
  mutate_at("WELBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WELBTU),
               list(name = sum)) # Total electricity consumption 
cb4.3 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(SQFTC == "5") %>%
  mutate(
    WNGBTU = NGBTU*FINALWT
  ) %>%
  mutate_at("WNGBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WNGBTU),
               list(name = sum)) # Total natural gas consumption 

cb4.1.0 <-
  cb4.1 / cb4.0 # Average major energy consumption per square foot in NE
cb4.2.0 <-
  cb4.2 / cb4.0 # Average electricity consumption per square foot in NE
cb4.3.0 <-
  cb4.3 / cb4.0 # Average natural gas consumption per square foot in NE



# 5) 50,001 to 100,000
cb5.0 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(SQFTC == "6") %>%
  mutate(
    WSQFT = SQFT * FINALWT
  ) %>%
  summarise_at(vars(WSQFT),
               list(name = sum)) # Total building square feet 
cb5.1 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(SQFTC == "6") %>%
  mutate(
    WMFBTU = MFBTU*FINALWT
  ) %>%
  mutate_at("WMFBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WMFBTU),
               list(name = sum)) # Total major fuel consumption 
cb5.2 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(SQFTC == "6") %>%
  mutate(
    WELBTU = ELBTU*FINALWT
  ) %>%
  mutate_at("WELBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WELBTU),
               list(name = sum)) # Total electricity consumption 
cb5.3 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(SQFTC == "6") %>%
  mutate(
    WNGBTU = NGBTU*FINALWT
  ) %>%
  mutate_at("WNGBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WNGBTU),
               list(name = sum)) # Total natural gas consumption 

cb5.1.0 <-
  cb5.1 / cb5.0 # Average major energy consumption per square foot in NE
cb5.2.0 <-
  cb5.2 / cb5.0 # Average electricity consumption per square foot in NE
cb5.3.0 <-
  cb5.3 / cb5.0 # Average natural gas consumption per square foot in NE



# 6) 5100,001 to 200,000
cb6.0 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(SQFTC == "7") %>%
  mutate(
    WSQFT = SQFT * FINALWT
  ) %>%
  summarise_at(vars(WSQFT),
               list(name = sum)) # Total building square feet 
cb6.1 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(SQFTC == "7") %>%
  mutate(
    WMFBTU = MFBTU*FINALWT
  ) %>%
  mutate_at("WMFBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WMFBTU),
               list(name = sum)) # Total major fuel consumption 
cb6.2 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(SQFTC == "7") %>%
  mutate(
    WELBTU = ELBTU*FINALWT
  ) %>%
  mutate_at("WELBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WELBTU),
               list(name = sum)) # Total electricity consumption 
cb6.3 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(SQFTC == "7") %>%
  mutate(
    WNGBTU = NGBTU*FINALWT
  ) %>%
  mutate_at("WNGBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WNGBTU),
               list(name = sum)) # Total natural gas consumption 

cb6.1.0 <-
  cb6.1 / cb6.0 # Average major energy consumption per square foot in NE
cb6.2.0 <-
  cb6.2 / cb6.0 # Average electricity consumption per square foot in NE
cb6.3.0 <-
  cb6.3 / cb6.0 # Average natural gas consumption per square foot in NE



# 7) 200,001 to 500,000
cb7.0 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(SQFTC == "8") %>%
  mutate(
    WSQFT = SQFT * FINALWT
  ) %>%
  summarise_at(vars(WSQFT),
               list(name = sum)) # Total building square feet 
cb7.1 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(SQFTC == "8") %>%
  mutate(
    WMFBTU = MFBTU*FINALWT
  ) %>%
  mutate_at("WMFBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WMFBTU),
               list(name = sum)) # Total major fuel consumption 
cb7.2 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(SQFTC == "8") %>%
  mutate(
    WELBTU = ELBTU*FINALWT
  ) %>%
  mutate_at("WELBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WELBTU),
               list(name = sum)) # Total electricity consumption 
cb7.3 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(SQFTC == "8") %>%
  mutate(
    WNGBTU = NGBTU*FINALWT
  ) %>%
  mutate_at("WNGBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WNGBTU),
               list(name = sum)) # Total natural gas consumption 

cb7.1.0 <-
  cb7.1 / cb7.0 # Average major energy consumption per square foot in NE
cb7.2.0 <-
  cb7.2 / cb7.0 # Average electricity consumption per square foot in NE
cb7.3.0 <-
  cb7.3 / cb7.0 # Average natural gas consumption per square foot in NE



# 8) over 500,000 
cb8.0 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(SQFTC == "9" | SQFTC == "10") %>%
  mutate(
    WSQFT = SQFT * FINALWT
  ) %>%
  summarise_at(vars(WSQFT),
               list(name = sum)) # Total building square feet 
cb8.1 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(SQFTC == "9" | SQFTC == "10") %>%
  mutate(
    WMFBTU = MFBTU*FINALWT
  ) %>%
  mutate_at("WMFBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WMFBTU),
               list(name = sum)) # Total major fuel consumption 
cb8.2 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(SQFTC == "9" | SQFTC == "10") %>%
  mutate(
    WELBTU = ELBTU*FINALWT
  ) %>%
  mutate_at("WELBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WELBTU),
               list(name = sum)) # Total electricity consumption 
cb8.3 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(SQFTC == "9" | SQFTC == "10") %>%
  mutate(
    WNGBTU = NGBTU*FINALWT
  ) %>%
  mutate_at("WNGBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WNGBTU),
               list(name = sum)) # Total natural gas consumption 

cb8.1.0 <-
  cb8.1 / cb8.0 # Average major energy consumption per square foot in NE
cb8.2.0 <-
  cb8.2 / cb8.0 # Average electricity consumption per square foot in NE
cb8.3.0 <-
  cb8.3 / cb8.0 # Average natural gas consumption per square foot in NE





# 2. Calculate average total major energy consumption per square foot by principal activity in NE

# 1) Education - 14
cby_1.0 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(PBA == "14") %>%
  mutate(
    WSQFT = SQFT * FINALWT
  ) %>%
  summarise_at(vars(WSQFT),
               list(name = sum)) # Total building square feet 
cby_1.1 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(PBA == "14") %>%
  mutate(
    WMFBTU = MFBTU*FINALWT
  ) %>%
  mutate_at("WMFBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WMFBTU),
               list(name = sum)) # Total major fuel consumption 
cby_1.2 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(PBA == "14") %>%
  mutate(
    WELBTU = ELBTU*FINALWT
  ) %>%
  mutate_at("WELBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WELBTU),
               list(name = sum)) # Total electricity consumption 
cby_1.3 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(PBA == "14") %>%
  mutate(
    WNGBTU = NGBTU*FINALWT
  ) %>%
  mutate_at("WNGBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WNGBTU),
               list(name = sum)) # Total natural gas consumption 

cby_1.1.0 <-
  cby_1.1 / cby_1.0 # Average major energy consumption per square foot in NE
cby_1.2.0 <-
  cby_1.2 / cby_1.0 # Average electricity consumption per square foot in NE
cby_1.3.0 <-
  cby_1.3 / cby_1.0 # Average natural gas consumption per square foot in NE



# 2) Health Care - 8, 16
cby_2.0 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(PBA == "8" | PBA == "16") %>%
  mutate(
    WSQFT = SQFT * FINALWT
  ) %>%
  summarise_at(vars(WSQFT),
               list(name = sum)) # Total building square feet 
cby_2.1 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(PBA == "8" | PBA == "16") %>%
  mutate(
    WMFBTU = MFBTU*FINALWT
  ) %>%
  mutate_at("WMFBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WMFBTU),
               list(name = sum)) # Total major fuel consumption 
cby_2.2 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(PBA == "8" | PBA == "16") %>%
  mutate(
    WELBTU = ELBTU*FINALWT
  ) %>%
  mutate_at("WELBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WELBTU),
               list(name = sum)) # Total electricity consumption 
cby_2.3 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(PBA == "8" | PBA == "16") %>%
  mutate(
    WNGBTU = NGBTU*FINALWT
  ) %>%
  mutate_at("WNGBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WNGBTU),
               list(name = sum)) # Total natural gas consumption 

cby_2.1.0 <-
  cby_2.1 / cby_2.0 # Average major energy consumption per square foot in NE
cby_2.2.0 <-
  cby_2.2 / cby_2.0 # Average electricity consumption per square foot in NE
cby_2.3.0 <-
  cby_2.3 / cby_2.0 # Average natural gas consumption per square foot in NE



# 3) Food Sale - 6
cby_3.0 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(PBA == "6") %>%
  mutate(
    WSQFT = SQFT * FINALWT
  ) %>%
  summarise_at(vars(WSQFT),
               list(name = sum)) # Total building square feet 
cby_3.1 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(PBA == "6") %>%
  mutate(
    WMFBTU = MFBTU*FINALWT
  ) %>%
  mutate_at("WMFBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WMFBTU),
               list(name = sum)) # Total major fuel consumption 
cby_3.2 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(PBA == "6") %>%
  mutate(
    WELBTU = ELBTU*FINALWT
  ) %>%
  mutate_at("WELBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WELBTU),
               list(name = sum)) # Total electricity consumption 
cby_3.3 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(PBA == "6") %>%
  mutate(
    WNGBTU = NGBTU*FINALWT
  ) %>%
  mutate_at("WNGBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WNGBTU),
               list(name = sum)) # Total natural gas consumption 

cby_3.1.0 <-
  cby_3.1 / cby_3.0 # Average major energy consumption per square foot in NE
cby_3.2.0 <-
  cby_3.2 / cby_3.0 # Average electricity consumption per square foot in NE
cby_3.3.0 <-
  cby_3.3 / cby_3.0 # Average natural gas consumption per square foot in NE



# 4) Food Service - 15
cby_4.0 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(PBA == "15") %>%
  mutate(
    WSQFT = SQFT * FINALWT
  ) %>%
  summarise_at(vars(WSQFT),
               list(name = sum)) # Total building square feet 
cby_4.1 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(PBA == "15") %>%
  mutate(
    WMFBTU = MFBTU*FINALWT
  ) %>%
  mutate_at("WMFBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WMFBTU),
               list(name = sum)) # Total major fuel consumption 
cby_4.2 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(PBA == "15") %>%
  mutate(
    WELBTU = ELBTU*FINALWT
  ) %>%
  mutate_at("WELBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WELBTU),
               list(name = sum)) # Total electricity consumption 
cby_4.3 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(PBA == "15") %>%
  mutate(
    WNGBTU = NGBTU*FINALWT
  ) %>%
  mutate_at("WNGBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WNGBTU),
               list(name = sum)) # Total natural gas consumption 

cby_4.1.0 <-
  cby_4.1 / cby_4.0 # Average major energy consumption per square foot in NE
cby_4.2.0 <-
  cby_4.2 / cby_4.0 # Average electricity consumption per square foot in NE
cby_4.3.0 <-
  cby_4.3 / cby_4.0 # Average natural gas consumption per square foot in NE



# 5) Lodging - 18, 17
cby_5.0 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(PBA == 18 | PBA == 17) %>%
  mutate(
    WSQFT = SQFT * FINALWT
  ) %>%
  summarise_at(vars(WSQFT),
               list(name = sum)) # Total building square feet 
cby_5.1 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(PBA == 18 | PBA == 17) %>%
  mutate(
    WMFBTU = MFBTU*FINALWT
  ) %>%
  mutate_at("WMFBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WMFBTU),
               list(name = sum)) # Total major fuel consumption 
cby_5.2 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(PBA == 18 | PBA == 17) %>%
  mutate(
    WELBTU = ELBTU*FINALWT
  ) %>%
  mutate_at("WELBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WELBTU),
               list(name = sum)) # Total electricity consumption 
cby_5.3 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(PBA == 18 | PBA == 17) %>%
  mutate(
    WNGBTU = NGBTU*FINALWT
  ) %>%
  mutate_at("WNGBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WNGBTU),
               list(name = sum)) # Total natural gas consumption 

cby_5.1.0 <-
  cby_5.1 / cby_5.0 # Average major energy consumption per square foot in NE
cby_5.2.0 <-
  cby_5.2 / cby_5.0 # Average electricity consumption per square foot in NE
cby_5.3.0 <-
  cby_5.3 / cby_5.0 # Average natural gas consumption per square foot in NE



# 6) Mercantile - 23, 24, 25
cby_6.0 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(PBA == "23" | PBA == "24" | PBA == "25") %>%
  mutate(
    WSQFT = SQFT * FINALWT
  ) %>%
  summarise_at(vars(WSQFT),
               list(name = sum)) # Total building square feet 
cby_6.1 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(PBA == "23" | PBA == "24" | PBA == "25") %>%
  mutate(
    WMFBTU = MFBTU*FINALWT
  ) %>%
  mutate_at("WMFBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WMFBTU),
               list(name = sum)) # Total major fuel consumption 
cby_6.2 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(PBA == "23" | PBA == "24" | PBA == "25") %>%
  mutate(
    WELBTU = ELBTU*FINALWT
  ) %>%
  mutate_at("WELBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WELBTU),
               list(name = sum)) # Total electricity consumption 
cby_6.3 <-
  Micro_COM12 %>%
  filter(REGION == "1") %>%
  filter(PBA == "23" | PBA == "24" | PBA == "25") %>%
  mutate(
    WNGBTU = NGBTU*FINALWT
  ) %>%
  mutate_at("WNGBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WNGBTU),
               list(name = sum)) # Total natural gas consumption 

cby_6.1.0 <-
  cby_6.1 / cby_6.0 # Average major energy consumption per square foot in NE
cby_6.2.0 <-
  cby_6.2 / cby_6.0 # Average electricity consumption per square foot in NE
cby_6.3.0 <-
  cby_6.3 / cby_6.0 # Average natural gas consumption per square foot in NE



# 7) Office - 2
cby_7.0 <-
  Micro_COM12 %>%
  filter(REGION == 1) %>%
  filter(PBA == 2) %>%
  mutate(
    WSQFT = SQFT * FINALWT
  ) %>%
  summarise_at(vars(WSQFT),
               list(name = sum)) # Total building square feet 
cby_7.1 <-
  Micro_COM12 %>%
  filter(REGION == 1) %>%
  filter(PBA == 2) %>%
  mutate(
    WMFBTU = MFBTU*FINALWT
  ) %>%
  mutate_at("WMFBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WMFBTU),
               list(name = sum)) # Total major fuel consumption 
cby_7.2 <-
  Micro_COM12 %>%
  filter(REGION == 1) %>%
  filter(PBA == 2) %>%
  mutate(
    WELBTU = ELBTU*FINALWT
  ) %>%
  mutate_at("WELBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WELBTU),
               list(name = sum)) # Total electricity consumption 
cby_7.3 <-
  Micro_COM12 %>%
  filter(REGION == 1) %>%
  filter(PBA == 2) %>%
  mutate(
    WNGBTU = NGBTU*FINALWT
  ) %>%
  mutate_at("WNGBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WNGBTU),
               list(name = sum)) # Total natural gas consumption 

cby_7.1.0 <-
  cby_7.1 / cby_7.0 # Average major energy consumption per square foot in NE
cby_7.2.0 <-
  cby_7.2 / cby_7.0 # Average electricity consumption per square foot in NE
cby_7.3.0 <-
  cby_7.3 / cby_7.0 # Average natural gas consumption per square foot in NE



# 8) Public Assembly - 13
cby_8.0 <-
  Micro_COM12 %>%
  filter(REGION == 1) %>%
  filter(PBA == 13) %>%
  mutate(
    WSQFT = SQFT * FINALWT
  ) %>%
  summarise_at(vars(WSQFT),
               list(name = sum)) # Total building square feet 
cby_8.1 <-
  Micro_COM12 %>%
  filter(REGION == 1) %>%
  filter(PBA == 13) %>%
  mutate(
    WMFBTU = MFBTU*FINALWT
  ) %>%
  mutate_at("WMFBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WMFBTU),
               list(name = sum)) # Total major fuel consumption 
cby_8.2 <-
  Micro_COM12 %>%
  filter(REGION == 1) %>%
  filter(PBA == 13) %>%
  mutate(
    WELBTU = ELBTU*FINALWT
  ) %>%
  mutate_at("WELBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WELBTU),
               list(name = sum)) # Total electricity consumption 
cby_8.3 <-
  Micro_COM12 %>%
  filter(REGION == 1) %>%
  filter(PBA == 13) %>%
  mutate(
    WNGBTU = NGBTU*FINALWT
  ) %>%
  mutate_at("WNGBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WNGBTU),
               list(name = sum)) # Total natural gas consumption 

cby_8.1.0 <-
  cby_8.1 / cby_8.0 # Average major energy consumption per square foot in NE
cby_8.2.0 <-
  cby_8.2 / cby_8.0 # Average electricity consumption per square foot in NE
cby_8.3.0 <-
  cby_8.3 / cby_8.0 # Average natural gas consumption per square foot in NE



# 9) Public order and safety - 7
cby_9.0 <-
  Micro_COM12 %>%
  filter(REGION == 1) %>%
  filter(PBA == 7) %>%
  mutate(
    WSQFT = SQFT * FINALWT
  ) %>%
  summarise_at(vars(WSQFT),
               list(name = sum)) # Total building square feet 
cby_9.1 <-
  Micro_COM12 %>%
  filter(REGION == 1) %>%
  filter(PBA == 7) %>%
  mutate(
    WMFBTU = MFBTU*FINALWT
  ) %>%
  mutate_at("WMFBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WMFBTU),
               list(name = sum)) # Total major fuel consumption 
cby_9.2 <-
  Micro_COM12 %>%
  filter(REGION == 1) %>%
  filter(PBA == 7) %>%
  mutate(
    WELBTU = ELBTU*FINALWT
  ) %>%
  mutate_at("WELBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WELBTU),
               list(name = sum)) # Total electricity consumption 
cby_9.3 <-
  Micro_COM12 %>%
  filter(REGION == 1) %>%
  filter(PBA == 7) %>%
  mutate(
    WNGBTU = NGBTU*FINALWT
  ) %>%
  mutate_at("WNGBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WNGBTU),
               list(name = sum)) # Total natural gas consumption 

cby_9.1.0 <-
  cby_9.1 / cby_9.0 # Average major energy consumption per square foot in NE
cby_9.2.0 <-
  cby_9.2 / cby_9.0 # Average electricity consumption per square foot in NE
cby_9.3.0 <-
  cby_9.3 / cby_9.0 # Average natural gas consumption per square foot in NE



# 10) Religious worship - 12
cby_10.0 <-
  Micro_COM12 %>%
  filter(REGION == 1) %>%
  filter(PBA == 12) %>%
  mutate(
    WSQFT = SQFT * FINALWT
  ) %>%
  summarise_at(vars(WSQFT),
               list(name = sum)) # Total building square feet 
cby_10.1 <-
  Micro_COM12 %>%
  filter(REGION == 1) %>%
  filter(PBA == 12) %>%
  mutate(
    WMFBTU = MFBTU*FINALWT
  ) %>%
  mutate_at("WMFBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WMFBTU),
               list(name = sum)) # Total major fuel consumption 
cby_10.2 <-
  Micro_COM12 %>%
  filter(REGION == 1) %>%
  filter(PBA == 12) %>%
  mutate(
    WELBTU = ELBTU*FINALWT
  ) %>%
  mutate_at("WELBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WELBTU),
               list(name = sum)) # Total electricity consumption 
cby_10.3 <-
  Micro_COM12 %>%
  filter(REGION == 1) %>%
  filter(PBA == 12) %>%
  mutate(
    WNGBTU = NGBTU*FINALWT
  ) %>%
  mutate_at("WNGBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WNGBTU),
               list(name = sum)) # Total natural gas consumption 

cby_10.1.0 <-
  cby_10.1 / cby_10.0 # Average major energy consumption per square foot in NE
cby_10.2.0 <-
  cby_10.2 / cby_10.0 # Average electricity consumption per square foot in NE
cby_10.3.0 <-
  cby_10.3 / cby_10.0 # Average natural gas consumption per square foot in NE



# 11) Service - 26
cby_11.0 <-
  Micro_COM12 %>%
  filter(REGION == 1) %>%
  filter(PBA == 26) %>%
  mutate(
    WSQFT = SQFT * FINALWT
  ) %>%
  summarise_at(vars(WSQFT),
               list(name = sum)) # Total building square feet 
cby_11.1 <-
  Micro_COM12 %>%
  filter(REGION == 1) %>%
  filter(PBA == 26) %>%
  mutate(
    WMFBTU = MFBTU*FINALWT
  ) %>%
  mutate_at("WMFBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WMFBTU),
               list(name = sum)) # Total major fuel consumption 
cby_11.2 <-
  Micro_COM12 %>%
  filter(REGION == 1) %>%
  filter(PBA == 26) %>%
  mutate(
    WELBTU = ELBTU*FINALWT
  ) %>%
  mutate_at("WELBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WELBTU),
               list(name = sum)) # Total electricity consumption 
cby_11.3 <-
  Micro_COM12 %>%
  filter(REGION == 1) %>%
  filter(PBA == 26) %>%
  mutate(
    WNGBTU = NGBTU*FINALWT
  ) %>%
  mutate_at("WNGBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WNGBTU),
               list(name = sum)) # Total natural gas consumption 

cby_11.1.0 <-
  cby_11.1 / cby_11.0 # Average major energy consumption per square foot in NE
cby_11.2.0 <-
  cby_11.2 / cby_11.0 # Average electricity consumption per square foot in NE
cby_11.3.0 <-
  cby_11.3 / cby_11.0 # Average natural gas consumption per square foot in NE



# 12) Warehouse and storage - 5, 11
cby_12.0 <-
  Micro_COM12 %>%
  filter(REGION == 1) %>%
  filter(PBA == 5 | PBA == 11) %>%
  mutate(
    WSQFT = SQFT * FINALWT
  ) %>%
  summarise_at(vars(WSQFT),
               list(name = sum)) # Total building square feet 
cby_12.1 <-
  Micro_COM12 %>%
  filter(REGION == 1) %>%
  filter(PBA == 5 | PBA == 11) %>%
  mutate(
    WMFBTU = MFBTU*FINALWT
  ) %>%
  mutate_at("WMFBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WMFBTU),
               list(name = sum)) # Total major fuel consumption 
cby_12.2 <-
  Micro_COM12 %>%
  filter(REGION == 1) %>%
  filter(PBA == 5 | PBA == 11) %>%
  mutate(
    WELBTU = ELBTU*FINALWT
  ) %>%
  mutate_at("WELBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WELBTU),
               list(name = sum)) # Total electricity consumption 
cby_12.3 <-
  Micro_COM12 %>%
  filter(REGION == 1) %>%
  filter(PBA == 5 | PBA == 11) %>%
  mutate(
    WNGBTU = NGBTU*FINALWT
  ) %>%
  mutate_at("WNGBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WNGBTU),
               list(name = sum)) # Total natural gas consumption 

cby_12.1.0 <-
  cby_12.1 / cby_12.0 # Average major energy consumption per square foot in NE
cby_12.2.0 <-
  cby_12.2 / cby_12.0 # Average electricity consumption per square foot in NE
cby_12.3.0 <-
  cby_12.3 / cby_12.0 # Average natural gas consumption per square foot in NE



# 13) Other - 91, 4
cby_13.0 <-
  Micro_COM12 %>%
  filter(REGION == 1) %>%
  filter(PBA == 91 | PBA == 4) %>%
  mutate(
    WSQFT = SQFT * FINALWT
  ) %>%
  summarise_at(vars(WSQFT),
               list(name = sum)) # Total building square feet 
cby_13.1 <-
  Micro_COM12 %>%
  filter(REGION == 1) %>%
  filter(PBA == 91 | PBA == 4) %>%
  mutate(
    WMFBTU = MFBTU*FINALWT
  ) %>%
  mutate_at("WMFBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WMFBTU),
               list(name = sum)) # Total major fuel consumption 
cby_13.2 <-
  Micro_COM12 %>%
  filter(REGION == 1) %>%
  filter(PBA == 91 | PBA == 4) %>%
  mutate(
    WELBTU = ELBTU*FINALWT
  ) %>%
  mutate_at("WELBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WELBTU),
               list(name = sum)) # Total electricity consumption 
cby_13.3 <-
  Micro_COM12 %>%
  filter(REGION == 1) %>%
  filter(PBA == 91 | PBA == 4) %>%
  mutate(
    WNGBTU = NGBTU*FINALWT
  ) %>%
  mutate_at("WNGBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WNGBTU),
               list(name = sum)) # Total natural gas consumption 

cby_13.1.0 <-
  cby_13.1 / cby_13.0 # Average major energy consumption per square foot in NE
cby_13.2.0 <-
  cby_13.2 / cby_13.0 # Average electricity consumption per square foot in NE
cby_13.3.0 <-
  cby_13.3 / cby_13.0 # Average natural gas consumption per square foot in NE



# 14) Vacant - 1
cby_14.0 <-
  Micro_COM12 %>%
  filter(REGION == 1) %>%
  filter(PBA == 1) %>%
  mutate(
    WSQFT = SQFT * FINALWT
  ) %>%
  summarise_at(vars(WSQFT),
               list(name = sum)) # Total building square feet 
cby_14.1 <-
  Micro_COM12 %>%
  filter(REGION == 1) %>%
  filter(PBA == 1) %>%
  mutate(
    WMFBTU = MFBTU*FINALWT
  ) %>%
  mutate_at("WMFBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WMFBTU),
               list(name = sum)) # Total major fuel consumption 
cby_14.2 <-
  Micro_COM12 %>%
  filter(REGION == 1) %>%
  filter(PBA == 1) %>%
  mutate(
    WELBTU = ELBTU*FINALWT
  ) %>%
  mutate_at("WELBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WELBTU),
               list(name = sum)) # Total electricity consumption 
cby_14.3 <-
  Micro_COM12 %>%
  filter(REGION == 1) %>%
  filter(PBA == 1) %>%
  mutate(
    WNGBTU = NGBTU*FINALWT
  ) %>%
  mutate_at("WNGBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WNGBTU),
               list(name = sum)) # Total natural gas consumption 

cby_14.1.0 <-
  cby_14.1 / cby_14.0 # Average major energy consumption per square foot in NE
cby_14.2.0 <-
  cby_14.2 / cby_14.0 # Average electricity consumption per square foot in NE
cby_14.3.0 <-
  cby_14.3 / cby_14.0 # Average natural gas consumption per square foot in NE



# 15) Accommodation and Food Services  - 6, 15, 17, 18
cby_15.0 <-
  Micro_COM12 %>%
  filter(REGION == 1) %>%
  filter(PBA == 6 | PBA == 15 | PBA == 17 | PBA == 18) %>%
  mutate(
    WSQFT = SQFT * FINALWT
  ) %>%
  summarise_at(vars(WSQFT),
               list(name = sum)) # Total building square feet 
cby_15.1 <-
  Micro_COM12 %>%
  filter(REGION == 1) %>%
  filter(PBA == 6 | PBA == 15 | PBA == 17 | PBA == 18) %>%
  mutate(
    WMFBTU = MFBTU*FINALWT
  ) %>%
  mutate_at("WMFBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WMFBTU),
               list(name = sum)) # Total major fuel consumption 
cby_15.2 <-
  Micro_COM12 %>%
  filter(REGION == 1) %>%
  filter(PBA == 6 | PBA == 15 | PBA == 17 | PBA == 18) %>%
  mutate(
    WELBTU = ELBTU*FINALWT
  ) %>%
  mutate_at("WELBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WELBTU),
               list(name = sum)) # Total electricity consumption 
cby_15.3 <-
  Micro_COM12 %>%
  filter(REGION == 1) %>%
  filter(PBA == 6 | PBA == 15 | PBA == 17 | PBA == 18) %>%
  mutate(
    WNGBTU = NGBTU*FINALWT
  ) %>%
  mutate_at("WNGBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WNGBTU),
               list(name = sum)) # Total natural gas consumption 

cby_15.1.0 <-
  cby_15.1 / cby_15.0 # Average major energy consumption per square foot in NE
cby_15.2.0 <-
  cby_15.2 / cby_15.0 # Average electricity consumption per square foot in NE
cby_15.3.0 <-
  cby_15.3 / cby_15.0 # Average natural gas consumption per square foot in NE



# 16) Other Services  - 12, 26
cby_16.0 <-
  Micro_COM12 %>%
  filter(REGION == 1) %>%
  filter(PBA == 12 | PBA == 26) %>%
  mutate(
    WSQFT = SQFT * FINALWT
  ) %>%
  summarise_at(vars(WSQFT),
               list(name = sum)) # Total building square feet 
cby_16.1 <-
  Micro_COM12 %>%
  filter(REGION == 1) %>%
  filter(PBA == 12 | PBA == 26) %>%
  mutate(
    WMFBTU = MFBTU*FINALWT
  ) %>%
  mutate_at("WMFBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WMFBTU),
               list(name = sum)) # Total major fuel consumption 
cby_16.2 <-
  Micro_COM12 %>%
  filter(REGION == 1) %>%
  filter(PBA == 12 | PBA == 26) %>%
  mutate(
    WELBTU = ELBTU*FINALWT
  ) %>%
  mutate_at("WELBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WELBTU),
               list(name = sum)) # Total electricity consumption 
cby_16.3 <-
  Micro_COM12 %>%
  filter(REGION == 1) %>%
  filter(PBA == 12 | PBA == 26) %>%
  mutate(
    WNGBTU = NGBTU*FINALWT
  ) %>%
  mutate_at("WNGBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WNGBTU),
               list(name = sum)) # Total natural gas consumption 

cby_16.1.0 <-
  cby_16.1 / cby_16.0 # Average major energy consumption per square foot in NE
cby_16.2.0 <-
  cby_16.2 / cby_16.0 # Average electricity consumption per square foot in NE
cby_16.3.0 <-
  cby_16.3 / cby_16.0 # Average natural gas consumption per square foot in NE




# 17) TOTAL AVERAGE
cby_17.0 <-
  Micro_COM12 %>%
  filter(REGION == 1) %>%
  mutate(
    WSQFT = SQFT * FINALWT
  ) %>%
  summarise_at(vars(WSQFT),
               list(name = sum)) # Total building square feet 
cby_17.1 <-
  Micro_COM12 %>%
  filter(REGION == 1) %>%
  mutate(
    WMFBTU = MFBTU*FINALWT
  ) %>%
  mutate_at("WMFBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WMFBTU),
               list(name = sum)) # Total major fuel consumption 
cby_17.2 <-
  Micro_COM12 %>%
  filter(REGION == 1) %>%
  mutate(
    WELBTU = ELBTU*FINALWT
  ) %>%
  mutate_at("WELBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WELBTU),
               list(name = sum)) # Total electricity consumption 
cby_17.3 <-
  Micro_COM12 %>%
  filter(REGION == 1) %>%
  mutate(
    WNGBTU = NGBTU*FINALWT
  ) %>%
  mutate_at("WNGBTU", ~replace(., is.na(.), 0)) %>%
  summarise_at(vars(WNGBTU),
               list(name = sum)) # Total natural gas consumption 

cby_17.1.0 <-
  cby_17.1 / cby_17.0 # Average major energy consumption per square foot in NE # 93.90 PER SUQARE FEET FLOORSPACE
cby_17.2.0 <-
  cby_17.2 / cby_17.0 # Average electricity consumption per square foot in NE
cby_17.3.0 <-
  cby_17.3 / cby_17.0 # Average natural gas consumption per square foot in NE



#################################################################################


# RESIDNETIAL BUILDING INPUT

# 0-1.          Energy Consumption
# TOTALBTU    Total usage, in thousand Btu, 2015
# 0-2.          Electricity Consumption
# BTUEL       Total site electricity usage, in thousand Btu, 2015
# 0-3.          Natural Gas Consumption
# BTUNG       Total natural gas usage, in thousand Btu, 2015



# 1. Average Use Rate by Housing Type

# 1-0. Re-categorize as UrbanFootprint
Micro_RES15_RE <-
  Micro_RES15 %>%
  mutate(TYPEHUQ_RE = case_when(
    TYPEHUQ == 1 ~ 1, # 1. Mobile Home -> Detached Single Family Small Lot
    TYPEHUQ == 2 & TOTSQFT_EN < 2000 ~ 1, # 2. Detached Single Family under 2,000 sqft -> Detached Single Family Small Lot
    TYPEHUQ == 2 & TOTSQFT_EN >= 2000 ~ 2, # 3. Detached Single Family over 2,000 sqft -> Detached Single Family Large Lot
    TYPEHUQ == 3 ~ 3, # 4. Attached Singled Family -> Attached Singled Family
    TYPEHUQ == 4 ~ 4, # 5. Apartment in building with 2-4 units -> Multifamily
    TYPEHUQ == 5 ~ 4, # 6. Apartment in building with 5+ Units -> Multifamily
  ))

# 1-1. Detached Single Family Small Lot in NE

RES_1.1 <-
Micro_RES15_RE %>%
  filter(REGIONC == 1) %>%
  filter(TYPEHUQ_RE == 1) %>%
  mutate(WTOTALBTU = TOTALBTU*NWEIGHT) %>%
  summarise_at(vars(WTOTALBTU),             
               list(name = sum)) # TOTAL ENERGY CONSUMPTION OF DETACHED SINGLE FAMILY SMALL LOT IN NE

RES_1.2 <-
  Micro_RES15_RE %>%
  filter(REGIONC == 1) %>%
  filter(TYPEHUQ_RE == 1) %>%
  mutate(WBTUEL = BTUEL*NWEIGHT) %>%
  summarise_at(vars(WBTUEL),             
               list(name = sum)) # TOTAL ELECTRICITY CONSUMPTION OF DETACHED SINGLE FAMILY SMALL LOT IN NE

RES_1.3 <-
  Micro_RES15_RE %>%
  filter(REGIONC == 1) %>%
  filter(TYPEHUQ_RE == 1) %>%
  mutate(WBTUNG = BTUNG*NWEIGHT) %>%
  summarise_at(vars(WBTUNG),             
               list(name = sum)) # TOTAL NATURAL GAS CONSUMPTION OF DETACHED SINGLE FAMILY SMALL LOT IN NE

RES_1.0 <-
  Micro_RES15_RE %>%
  filter(REGIONC == 1) %>%
  filter(TYPEHUQ_RE == 1) %>%
  summarise_at(vars(NWEIGHT),             
               list(name = sum)) # TOTAL NUMBER OF DETACHED SINGLE FAMILY SMALL LOT IN NE

RES_1.1.0 <-
RES_1.1 / RES_1.0 # AVERAGE TOTAL ENERGY CONSUMPTION RATE OF DETACHED SIGLE FAMILY SMALL LOT IN NE

RES_1.2.0 <-
  RES_1.2 / RES_1.0 # AVERAGE TOTAL ELECTRICITY CONSUMPTION RATE OF DETACHED SIGLE FAMILY SMALL LOT IN NE

RES_1.3.0 <-
  RES_1.3 / RES_1.0 # AVERAGE TOTAL NATURAL GAS CONSUMPTION RATE OF DETACHED SIGLE FAMILY SMALL LOT IN NE



# 1-2. Detached Single Family Large Lot in NE

RES_2.1 <-
  Micro_RES15_RE %>%
  filter(REGIONC == 1) %>%
  filter(TYPEHUQ_RE == 2) %>%
  mutate(WTOTALBTU = TOTALBTU*NWEIGHT) %>%
  summarise_at(vars(WTOTALBTU),             
               list(name = sum)) # TOTAL ENERGY CONSUMPTION OF DETACHED SINGLE FAMILY LARGE LOT IN NE

RES_2.2 <-
  Micro_RES15_RE %>%
  filter(REGIONC == 1) %>%
  filter(TYPEHUQ_RE == 2) %>%
  mutate(WBTUEL = BTUEL*NWEIGHT) %>%
  summarise_at(vars(WBTUEL),             
               list(name = sum)) # TOTAL ELECTRICITY CONSUMPTION OF DETACHED SINGLE FAMILY LARGE LOT IN NE

RES_2.3 <-
  Micro_RES15_RE %>%
  filter(REGIONC == 1) %>%
  filter(TYPEHUQ_RE == 2) %>%
  mutate(WBTUNG = BTUNG*NWEIGHT) %>%
  summarise_at(vars(WBTUNG),             
               list(name = sum)) # TOTAL NATURAL GAS CONSUMPTION OF DETACHED SINGLE FAMILY LARGE LOT IN NE

RES_2.0 <-
  Micro_RES15_RE %>%
  filter(REGIONC == 1) %>%
  filter(TYPEHUQ_RE == 2) %>%
  summarise_at(vars(NWEIGHT),             
               list(name = sum)) # TOTAL NUMBER OF DETACHED SINGLE FAMILY LARGE LOT IN NE

RES_2.1.0 <-
  RES_2.1 / RES_2.0 # AVERAGE TOTAL ENERGY CONSUMPTION RATE OF DETACHED SIGLE FAMILY LARGE LOT IN NE

RES_2.2.0 <-
  RES_2.2 / RES_2.0 # AVERAGE TOTAL ELECTRICITY CONSUMPTION RATE OF DETACHED SIGLE FAMILY LARGE LOT IN NE

RES_2.3.0 <-
  RES_2.3 / RES_2.0 # AVERAGE TOTAL NATURAL GAS CONSUMPTION RATE OF DETACHED SIGLE FAMILY LARGE LOT IN NE



# 1-3, Attached Singled Family in NE

RES_3.1 <-
  Micro_RES15_RE %>%
  filter(REGIONC == 1) %>%
  filter(TYPEHUQ_RE == 3) %>%
  mutate(WTOTALBTU = TOTALBTU*NWEIGHT) %>%
  summarise_at(vars(WTOTALBTU),             
               list(name = sum)) # TOTAL ENERGY CONSUMPTION OF ATTACHED SINGLED FAMILY IN NE

RES_3.2 <-
  Micro_RES15_RE %>%
  filter(REGIONC == 1) %>%
  filter(TYPEHUQ_RE == 3) %>%
  mutate(WBTUEL = BTUEL*NWEIGHT) %>%
  summarise_at(vars(WBTUEL),             
               list(name = sum)) # TOTAL ELECTRICITY CONSUMPTION OF ATTACHED SINGLED FAMILY IN NE

RES_3.3 <-
  Micro_RES15_RE %>%
  filter(REGIONC == 1) %>%
  filter(TYPEHUQ_RE == 3) %>%
  mutate(WBTUNG = BTUNG*NWEIGHT) %>%
  summarise_at(vars(WBTUNG),             
               list(name = sum)) # TOTAL NATURAL GAS CONSUMPTION OF ATTACHED SINGLED FAMILY IN NE

RES_3.0 <-
  Micro_RES15_RE %>%
  filter(REGIONC == 1) %>%
  filter(TYPEHUQ_RE == 3) %>%
  summarise_at(vars(NWEIGHT),             
               list(name = sum)) # TOTAL NUMBER OF ATTACHED SINGLED FAMILY IN NE

RES_3.1.0 <-
  RES_3.1 / RES_3.0 # AVERAGE TOTAL ENERGY CONSUMPTION RATE OF ATTACHED SINGLED FAMILY IN NE

RES_3.2.0 <-
  RES_3.2 / RES_3.0 # AVERAGE TOTAL ELECTRICITY CONSUMPTION RATE OF ATTACHED SINGLED FAMILY IN NE

RES_3.3.0 <-
  RES_3.3 / RES_3.0 # AVERAGE TOTAL NATURAL GAS CONSUMPTION RATE OF ATTACHED SINGLED FAMILY IN NE



# 1-4. Multifamily in NE

RES_4.1 <-
  Micro_RES15_RE %>%
  filter(REGIONC == 1) %>%
  filter(TYPEHUQ_RE == 4) %>%
  mutate(WTOTALBTU = TOTALBTU*NWEIGHT) %>%
  summarise_at(vars(WTOTALBTU),             
               list(name = sum)) # TOTAL ENERGY CONSUMPTION OF MULTIFAMILY IN NE
RES_4.2 <-
  Micro_RES15_RE %>%
  filter(REGIONC == 1) %>%
  filter(TYPEHUQ_RE == 4) %>%
  mutate(WBTUEL = BTUEL*NWEIGHT) %>%
  summarise_at(vars(WBTUEL),             
               list(name = sum)) # TOTAL ELECTRICITY CONSUMPTION OF MULTIFAMILY IN NE
RES_4.3 <-
  Micro_RES15_RE %>%
  filter(REGIONC == 1) %>%
  filter(TYPEHUQ_RE == 4) %>%
  mutate(WBTUNG = BTUNG*NWEIGHT) %>%
  summarise_at(vars(WBTUNG),             
               list(name = sum)) # TOTAL NATURAL GAS CONSUMPTION OF MULTIFAMILY IN NE
RES_4.0 <-
  Micro_RES15_RE %>%
  filter(REGIONC == 1) %>%
  filter(TYPEHUQ_RE == 4) %>%
  summarise_at(vars(NWEIGHT),             
               list(name = sum)) # TOTAL NUMBER OF MULTIFAMILY IN NE
RES_4.1.0 <-
  RES_4.1 / RES_4.0 # AVERAGE TOTAL ENERGY CONSUMPTION RATE OF MULTIFAMILY IN NE
RES_4.2.0 <-
  RES_4.2 / RES_4.0 # AVERAGE TOTAL ELECTRICITY CONSUMPTION RATE OF MULTIFAMILY IN NE
RES_4.3.0 <-
  RES_4.3 / RES_4.0 # AVERAGE TOTAL NATURAL GAS CONSUMPTION RATE OF MULTIFAMILY IN NE

SF1_10_TRI <- rbind(sf1_10_ny, sf1_10_nj, sf1_10_ct)

SF1_10_TRI2 <-
  SF1_10_TRI %>% 
  cast(GEOID~variable) %>%
  dplyr::rename(
    GEOID10 = "GEOID",
    HH_TTL = "H013001", # Total Household
    HH_1PH = "H013002", # Total!!1-person household
    HH_2PH = "H013003", # Total!!2-person household
    HH_3PH = "H013004", # Total!!3-person household
    HH_4PH = "H013005", # Total!!4-person household
    HH_5PH = "H013006", # Total!!5-person household
    HH_6PH = "H013007", # Total!!6-person household
    HH_7OV = "H013008", # Total!!7-or-more-person household
    OV_TTL = "H003001", # Total Household
    OV_OCP = "H003002", # Total!!Occupied
    OV_VAC = "H003003"  # Total!!Vacant
  ) %>%
  mutate(
    OCP_PT = (OV_OCP/OV_TTL)*100
  ) %>%
  mutate_at(vars(OCP_PT), 
            funs(round(., 1)))

SF1_10_TRI2 <- mutate_all(SF1_10_TRI2, ~replace(., is.na(.), 0))

# Calculate the Total Estimated Energy Consumption
SF1_10_TRI3 <-
  SF1_10_TRI2 %>%
  mutate(
    ENG_TTL = (HH_1PH*59544.65) + (HH_2PH*91988.08) + (HH_3PH*108451.1) + (HH_4PH*128971.6) + 
      (HH_5PH*125381.9) + (HH_6PH*130221.2) + (HH_7OV*163132.5),
    ELC_TTL = (HH_1PH*17412.23) + (HH_2PH*27391.14) + (HH_3PH*30860.98) + (HH_4PH*39222.74) + 
      (HH_5PH*36871.54) + (HH_6PH*42111.24) + (HH_7OV*55334.93),
    NGS_TTL = (HH_1PH*28264.55) + (HH_2PH*39398.32) + (HH_3PH*51856.87) + (HH_4PH*59105.16) + 
      (HH_5PH*73064.9) + (HH_6PH*56748.47) + (HH_7OV*80412.91)
  )

# Combine with Census Blocks
geo_Blocks = st_read("G:/Shared drives/Projects/5035_Tech Equity/Geographies/RPA_Blocks_shp.shp")
csv_Blocks <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Geographies/RPA_Blocks_ref.csv')

geoSF1_ENG <- merge(geo_Blocks, SF1_10_TRI3, by = "GEOID10", all.x = TRUE)
st_write(geoSF1_ENG,
         "G:/Shared drives/Projects/5035_Tech Equity/Data/A_Shapefile/Scenario/ENERGY_TTL.shp", 
         driver = "ESRI Shapefile")
# 2. Average Use Rate by Household Size
# 2-0. Re-categorize as ACS Decennial Survey
Micro_RES15_RE2 <-
  Micro_RES15_RE %>%
  mutate(NHSLDMEM_RE = case_when(
    NHSLDMEM == 1 ~ 1, 
    NHSLDMEM == 2 ~ 2, 
    NHSLDMEM == 3 ~ 3, 
    NHSLDMEM == 4 ~ 4, 
    NHSLDMEM == 5 ~ 5, 
    NHSLDMEM == 6 ~ 6, 
    NHSLDMEM == 7 ~ 7, 
    NHSLDMEM == 8 ~ 7, 
    NHSLDMEM == 9 ~ 7, 
    NHSLDMEM == 10 ~ 7, 
    NHSLDMEM == 11 ~ 7, 
    NHSLDMEM == 12 ~ 7, 
    NHSLDMEM == 13 ~ 7, 
    NHSLDMEM == 14 ~ 7, 
    NHSLDMEM == 15 ~ 7, 
    NHSLDMEM == 16 ~ 7, 
    NHSLDMEM == 17 ~ 7, 
    NHSLDMEM == 18 ~ 7, 
    NHSLDMEM == 19 ~ 7, 
    NHSLDMEM == 20 ~ 7
  ))

# 2-1. 1-person household in NE
RES2_1.1 <-
  Micro_RES15_RE2 %>%
  filter(REGIONC == 1) %>%
  filter(NHSLDMEM_RE == 1) %>%
  mutate(WTOTALBTU = TOTALBTU*NWEIGHT) %>%
  summarise_at(vars(WTOTALBTU),             
               list(name = sum)) # TOTAL ENERGY CONSUMPTION OF 1-PERSON HOUSEHOLD IN NE

RES2_1.2 <-
  Micro_RES15_RE2 %>%
  filter(REGIONC == 1) %>%
  filter(NHSLDMEM_RE == 1) %>%
  mutate(WBTUEL = BTUEL*NWEIGHT) %>%
  summarise_at(vars(WBTUEL),             
               list(name = sum)) # TOTAL ELECTRICITY CONSUMPTION OF 1-PERSON HOUSEHOLD IN NE

RES2_1.3 <-
  Micro_RES15_RE2 %>%
  filter(REGIONC == 1) %>%
  filter(NHSLDMEM_RE == 1) %>%
  mutate(WBTUNG = BTUNG*NWEIGHT) %>%
  summarise_at(vars(WBTUNG),             
               list(name = sum)) # TOTAL NATURAL GAS CONSUMPTION OF 1-PERSON HOUSEHOLD IN NE

RES2_1.0 <-
  Micro_RES15_RE2 %>%
  filter(REGIONC == 1) %>%
  filter(NHSLDMEM_RE == 1) %>%
  summarise_at(vars(NWEIGHT),             
               list(name = sum)) # TOTAL NUMBER OF 1-PERSON HOUSEHOLD IN NE

RES2_1.1.0 <-
  RES2_1.1 / RES2_1.0 # AVERAGE TOTAL ENERGY CONSUMPTION RATE OF 1-PERSON HOUSEHOLD IN NE

RES2_1.2.0 <-
  RES2_1.2 / RES2_1.0 # AVERAGE TOTAL ELECTRICITY CONSUMPTION RATE OF 1-PERSON HOUSEHOLD IN NE

RES2_1.3.0 <-
  RES2_1.3 / RES2_1.0 # AVERAGE TOTAL NATURAL GAS CONSUMPTION RATE OF 1-PERSON HOUSEHOLD IN NE



# 2-2. 2-person household in NE

RES2_2.1 <-
  Micro_RES15_RE2 %>%
  filter(REGIONC == 1) %>%
  filter(NHSLDMEM_RE == 2) %>%
  mutate(WTOTALBTU = TOTALBTU*NWEIGHT) %>%
  summarise_at(vars(WTOTALBTU),             
               list(name = sum)) # TOTAL ENERGY CONSUMPTION OF 2-PERSON HOUSEHOLD IN NE

RES2_2.2 <-
  Micro_RES15_RE2 %>%
  filter(REGIONC == 1) %>%
  filter(NHSLDMEM_RE == 2) %>%
  mutate(WBTUEL = BTUEL*NWEIGHT) %>%
  summarise_at(vars(WBTUEL),             
               list(name = sum)) # TOTAL ELECTRICITY CONSUMPTION OF 2-PERSON HOUSEHOLD IN NE

RES2_2.3 <-
  Micro_RES15_RE2 %>%
  filter(REGIONC == 1) %>%
  filter(NHSLDMEM_RE == 2) %>%
  mutate(WBTUNG = BTUNG*NWEIGHT) %>%
  summarise_at(vars(WBTUNG),             
               list(name = sum)) # TOTAL NATURAL GAS CONSUMPTION OF 2-PERSON HOUSEHOLD IN NE

RES2_2.0 <-
  Micro_RES15_RE2 %>%
  filter(REGIONC == 1) %>%
  filter(NHSLDMEM_RE == 2) %>%
  summarise_at(vars(NWEIGHT),             
               list(name = sum)) # TOTAL NUMBER OF 2-PERSON HOUSEHOLD IN NE

RES2_2.1.0 <-
  RES2_2.1 / RES2_2.0 # AVERAGE TOTAL ENERGY CONSUMPTION RATE OF 2-PERSON HOUSEHOLD IN NE

RES2_2.2.0 <-
  RES2_2.2 / RES2_2.0 # AVERAGE TOTAL ELECTRICITY CONSUMPTION RATE OF 2-PERSON HOUSEHOLD IN NE

RES2_2.3.0 <-
  RES2_2.3 / RES2_2.0 # AVERAGE TOTAL NATURAL GAS CONSUMPTION RATE OF 2-PERSON HOUSEHOLD IN NE



# 2-3. 3-person household in NE

RES2_3.1 <-
  Micro_RES15_RE2 %>%
  filter(REGIONC == 1) %>%
  filter(NHSLDMEM_RE == 3) %>%
  mutate(WTOTALBTU = TOTALBTU*NWEIGHT) %>%
  summarise_at(vars(WTOTALBTU),             
               list(name = sum)) # TOTAL ENERGY CONSUMPTION OF 3-PERSON HOUSEHOLD IN NE

RES2_3.2 <-
  Micro_RES15_RE2 %>%
  filter(REGIONC == 1) %>%
  filter(NHSLDMEM_RE == 3) %>%
  mutate(WBTUEL = BTUEL*NWEIGHT) %>%
  summarise_at(vars(WBTUEL),             
               list(name = sum)) # TOTAL ELECTRICITY CONSUMPTION OF 3-PERSON HOUSEHOLD IN NE

RES2_3.3 <-
  Micro_RES15_RE2 %>%
  filter(REGIONC == 1) %>%
  filter(NHSLDMEM_RE == 3) %>%
  mutate(WBTUNG = BTUNG*NWEIGHT) %>%
  summarise_at(vars(WBTUNG),             
               list(name = sum)) # TOTAL NATURAL GAS CONSUMPTION OF 3-PERSON HOUSEHOLD IN NE

RES2_3.0 <-
  Micro_RES15_RE2 %>%
  filter(REGIONC == 1) %>%
  filter(NHSLDMEM_RE == 3) %>%
  summarise_at(vars(NWEIGHT),             
               list(name = sum)) # TOTAL NUMBER OF 3-PERSON HOUSEHOLD IN NE

RES2_3.1.0 <-
  RES2_3.1 / RES2_3.0 # AVERAGE TOTAL ENERGY CONSUMPTION RATE OF 3-PERSON HOUSEHOLD IN NE

RES2_3.2.0 <-
  RES2_3.2 / RES2_3.0 # AVERAGE TOTAL ELECTRICITY CONSUMPTION RATE OF 3-PERSON HOUSEHOLD IN NE

RES2_3.3.0 <-
  RES2_3.3 / RES2_3.0 # AVERAGE TOTAL NATURAL GAS CONSUMPTION RATE OF 3-PERSON HOUSEHOLD IN NE



# 2-4. 4-person household in NE

RES2_4.1 <-
  Micro_RES15_RE2 %>%
  filter(REGIONC == 1) %>%
  filter(NHSLDMEM_RE == 4) %>%
  mutate(WTOTALBTU = TOTALBTU*NWEIGHT) %>%
  summarise_at(vars(WTOTALBTU),             
               list(name = sum)) # TOTAL ENERGY CONSUMPTION OF 4-PERSON HOUSEHOLD IN NE

RES2_4.2 <-
  Micro_RES15_RE2 %>%
  filter(REGIONC == 1) %>%
  filter(NHSLDMEM_RE == 4) %>%
  mutate(WBTUEL = BTUEL*NWEIGHT) %>%
  summarise_at(vars(WBTUEL),             
               list(name = sum)) # TOTAL ELECTRICITY CONSUMPTION OF 4-PERSON HOUSEHOLD IN NE

RES2_4.3 <-
  Micro_RES15_RE2 %>%
  filter(REGIONC == 1) %>%
  filter(NHSLDMEM_RE == 4) %>%
  mutate(WBTUNG = BTUNG*NWEIGHT) %>%
  summarise_at(vars(WBTUNG),             
               list(name = sum)) # TOTAL NATURAL GAS CONSUMPTION OF 4-PERSON HOUSEHOLD IN NE

RES2_4.0 <-
  Micro_RES15_RE2 %>%
  filter(REGIONC == 1) %>%
  filter(NHSLDMEM_RE == 4) %>%
  summarise_at(vars(NWEIGHT),             
               list(name = sum)) # TOTAL NUMBER OF 4-PERSON HOUSEHOLD IN NE

RES2_4.1.0 <-
  RES2_4.1 / RES2_4.0 # AVERAGE TOTAL ENERGY CONSUMPTION RATE OF 4-PERSON HOUSEHOLD IN NE

RES2_4.2.0 <-
  RES2_4.2 / RES2_4.0 # AVERAGE TOTAL ELECTRICITY CONSUMPTION RATE OF 4-PERSON HOUSEHOLD IN NE

RES2_4.3.0 <-
  RES2_4.3 / RES2_4.0 # AVERAGE TOTAL NATURAL GAS CONSUMPTION RATE OF 4-PERSON HOUSEHOLD IN NE



# 2-5. 5-person household in NE

RES2_5.1 <-
  Micro_RES15_RE2 %>%
  filter(REGIONC == 1) %>%
  filter(NHSLDMEM_RE == 5) %>%
  mutate(WTOTALBTU = TOTALBTU*NWEIGHT) %>%
  summarise_at(vars(WTOTALBTU),             
               list(name = sum)) # TOTAL ENERGY CONSUMPTION OF 5-PERSON HOUSEHOLD IN NE

RES2_5.2 <-
  Micro_RES15_RE2 %>%
  filter(REGIONC == 1) %>%
  filter(NHSLDMEM_RE == 5) %>%
  mutate(WBTUEL = BTUEL*NWEIGHT) %>%
  summarise_at(vars(WBTUEL),             
               list(name = sum)) # TOTAL ELECTRICITY CONSUMPTION OF 5-PERSON HOUSEHOLD IN NE

RES2_5.3 <-
  Micro_RES15_RE2 %>%
  filter(REGIONC == 1) %>%
  filter(NHSLDMEM_RE == 5) %>%
  mutate(WBTUNG = BTUNG*NWEIGHT) %>%
  summarise_at(vars(WBTUNG),             
               list(name = sum)) # TOTAL NATURAL GAS CONSUMPTION OF 5-PERSON HOUSEHOLD IN NE

RES2_5.0 <-
  Micro_RES15_RE2 %>%
  filter(REGIONC == 1) %>%
  filter(NHSLDMEM_RE == 5) %>%
  summarise_at(vars(NWEIGHT),             
               list(name = sum)) # TOTAL NUMBER OF 5-PERSON HOUSEHOLD IN NE

RES2_5.1.0 <-
  RES2_5.1 / RES2_5.0 # AVERAGE TOTAL ENERGY CONSUMPTION RATE OF 5-PERSON HOUSEHOLD IN NE

RES2_5.2.0 <-
  RES2_5.2 / RES2_5.0 # AVERAGE TOTAL ELECTRICITY CONSUMPTION RATE OF 5-PERSON HOUSEHOLD IN NE

RES2_5.3.0 <-
  RES2_5.3 / RES2_5.0 # AVERAGE TOTAL NATURAL GAS CONSUMPTION RATE OF 5-PERSON HOUSEHOLD IN NE



# 2-6. 6-person household in NE

RES2_6.1 <-
  Micro_RES15_RE2 %>%
  filter(REGIONC == 1) %>%
  filter(NHSLDMEM_RE == 6) %>%
  mutate(WTOTALBTU = TOTALBTU*NWEIGHT) %>%
  summarise_at(vars(WTOTALBTU),             
               list(name = sum)) # TOTAL ENERGY CONSUMPTION OF 6-PERSON HOUSEHOLD IN NE

RES2_6.2 <-
  Micro_RES15_RE2 %>%
  filter(REGIONC == 1) %>%
  filter(NHSLDMEM_RE == 6) %>%
  mutate(WBTUEL = BTUEL*NWEIGHT) %>%
  summarise_at(vars(WBTUEL),             
               list(name = sum)) # TOTAL ELECTRICITY CONSUMPTION OF 6-PERSON HOUSEHOLD IN NE

RES2_6.3 <-
  Micro_RES15_RE2 %>%
  filter(REGIONC == 1) %>%
  filter(NHSLDMEM_RE == 6) %>%
  mutate(WBTUNG = BTUNG*NWEIGHT) %>%
  summarise_at(vars(WBTUNG),             
               list(name = sum)) # TOTAL NATURAL GAS CONSUMPTION OF 6-PERSON HOUSEHOLD IN NE

RES2_6.0 <-
  Micro_RES15_RE2 %>%
  filter(REGIONC == 1) %>%
  filter(NHSLDMEM_RE == 6) %>%
  summarise_at(vars(NWEIGHT),             
               list(name = sum)) # TOTAL NUMBER OF 6-PERSON HOUSEHOLD IN NE

RES2_6.1.0 <-
  RES2_6.1 / RES2_6.0 # AVERAGE TOTAL ENERGY CONSUMPTION RATE OF 6-PERSON HOUSEHOLD IN NE

RES2_6.2.0 <-
  RES2_6.2 / RES2_6.0 # AVERAGE TOTAL ELECTRICITY CONSUMPTION RATE OF 6-PERSON HOUSEHOLD IN NE

RES2_6.3.0 <-
  RES2_6.3 / RES2_6.0 # AVERAGE TOTAL NATURAL GAS CONSUMPTION RATE OF 6-PERSON HOUSEHOLD IN NE



# 2-7. 7-person household in NE

RES2_7.1 <-
  Micro_RES15_RE2 %>%
  filter(REGIONC == 1) %>%
  filter(NHSLDMEM_RE == 7) %>%
  mutate(WTOTALBTU = TOTALBTU*NWEIGHT) %>%
  summarise_at(vars(WTOTALBTU),             
               list(name = sum)) # TOTAL ENERGY CONSUMPTION OF 7-PERSON HOUSEHOLD IN NE

RES2_7.2 <-
  Micro_RES15_RE2 %>%
  filter(REGIONC == 1) %>%
  filter(NHSLDMEM_RE == 7) %>%
  mutate(WBTUEL = BTUEL*NWEIGHT) %>%
  summarise_at(vars(WBTUEL),             
               list(name = sum)) # TOTAL ELECTRICITY CONSUMPTION OF 7-PERSON HOUSEHOLD IN NE

RES2_7.3 <-
  Micro_RES15_RE2 %>%
  filter(REGIONC == 1) %>%
  filter(NHSLDMEM_RE == 7) %>%
  mutate(WBTUNG = BTUNG*NWEIGHT) %>%
  summarise_at(vars(WBTUNG),             
               list(name = sum)) # TOTAL NATURAL GAS CONSUMPTION OF 7-PERSON HOUSEHOLD IN NE

RES2_7.0 <-
  Micro_RES15_RE2 %>%
  filter(REGIONC == 1) %>%
  filter(NHSLDMEM_RE == 7) %>%
  summarise_at(vars(NWEIGHT),             
               list(name = sum)) # TOTAL NUMBER OF 7-PERSON HOUSEHOLD IN NE

RES2_7.1.0 <-
  RES2_7.1 / RES2_7.0 # AVERAGE TOTAL ENERGY CONSUMPTION RATE OF 7-PERSON HOUSEHOLD IN NE

RES2_7.2.0 <-
  RES2_7.2 / RES2_7.0 # AVERAGE TOTAL ELECTRICITY CONSUMPTION RATE OF 7-PERSON HOUSEHOLD IN NE

RES2_7.3.0 <-
  RES2_7.3 / RES2_7.0 # AVERAGE TOTAL NATURAL GAS CONSUMPTION RATE OF 7-PERSON HOUSEHOLD IN NE



#################################################################################

# ACS DECENNIAL DATA, 2010 - CENSUS BLOCK LEVEL

# 0. Exploring Census Block Data

census_api_key("5e8ebd0bdf69ccee12f6a6be2cdc51acae7b2339", overwrite = TRUE, install = TRUE)

acs_variable_list.dec.10 <- load_variables(2010, 
                                         "sf1", 
                                         cache = TRUE)
acs_variable_list.5.19 <- load_variables(2019, 
                                         "acs5", 
                                         cache = TRUE)
# HOUSEHOLD SIZE
#  H013001      Total Household
#  H013002      Total!!1-person household
#  H013003      Total!!2-person household
#  H013004      Total!!3-person household
#  H013005      Total!!4-person household
#  H013006      Total!!5-person household
#  H013007      Total!!6-person household
#  H013008      Total!!7-or-more-person household

# OCCUPANCY-VACANCY STATUS
#   H003001     Total Household
#   H003002     Total!!Occupied
#   H003003     Total!!Vacant

sf1_10_ny <- get_decennial(geography = "block", 
                           variables = c(
                             "H013001", # Total Household
                             "H013002", # Total!!1-person household
                             "H013003", # Total!!2-person household
                             "H013004", # Total!!3-person household
                             "H013005", # Total!!4-person household
                             "H013006", # Total!!5-person household
                             "H013007", # Total!!6-person household
                             "H013008", # Total!!7-or-more-person household
                             "H003001", # Total Household
                             "H003002", # Total!!Occupied
                             "H003003"  # Total!!Vacant
                           ),
                           state = "NY", county = c("005", "027", "047", "059", "061",
                                                    "071", "079", "081", "085", "087",
                                                    "103", "105", "111", "119"),
                           survey = "sf1", year = 2010)

sf1_10_nj <- get_decennial(geography = "block",
                           variables = c(
                             "H013001", # Total Household
                             "H013002", # Total!!1-person household
                             "H013003", # Total!!2-person household
                             "H013004", # Total!!3-person household
                             "H013005", # Total!!4-person household
                             "H013006", # Total!!5-person household
                             "H013007", # Total!!6-person household
                             "H013008", # Total!!7-or-more-person household
                             "H003001", # Total Household
                             "H003002", # Total!!Occupied
                             "H003003"  # Total!!Vacant
                           ),
                           state = "NJ", county = c("003", "013", "017", "019", "021",
                                                    "023", "025", "027", "029", "031", 
                                                    "035", "037", "039", "041"),
                           survey = "sf1", year = 2010)

sf1_10_ct <- get_decennial(geography = "block",
                           variables = c(
                             "H013001", # Total Household
                             "H013002", # Total!!1-person household
                             "H013003", # Total!!2-person household
                             "H013004", # Total!!3-person household
                             "H013005", # Total!!4-person household
                             "H013006", # Total!!5-person household
                             "H013007", # Total!!6-person household
                             "H013008", # Total!!7-or-more-person household
                             "H003001", # Total Household
                             "H003002", # Total!!Occupied
                             "H003003"  # Total!!Vacant
                           ),
                           state = "CT", county = c("001", "005", "009"),
                           survey = "sf1", year = 2010)

SF1_10_TRI <- rbind(sf1_10_ny, sf1_10_nj, sf1_10_ct)

SF1_10_TRI2 <-
  SF1_10_TRI %>% 
  cast(GEOID~variable) %>%
  dplyr::rename(
    GEOID10 = "GEOID",
    HH_TTL = "H013001", # Total Household
    HH_1PH = "H013002", # Total!!1-person household
    HH_2PH = "H013003", # Total!!2-person household
    HH_3PH = "H013004", # Total!!3-person household
    HH_4PH = "H013005", # Total!!4-person household
    HH_5PH = "H013006", # Total!!5-person household
    HH_6PH = "H013007", # Total!!6-person household
    HH_7OV = "H013008", # Total!!7-or-more-person household
    OV_TTL = "H003001", # Total Household
    OV_OCP = "H003002", # Total!!Occupied
    OV_VAC = "H003003"  # Total!!Vacant
  ) %>%
  mutate(
    OCP_PT = (OV_OCP/OV_TTL)*100
  ) %>%
  mutate_at(vars(OCP_PT), 
            funs(round(., 1)))

SF1_10_TRI2 <- mutate_all(SF1_10_TRI2, ~replace(., is.na(.), 0))

# Calculate the Total Estimated Energy Consumption
SF1_10_TRI3 <-
SF1_10_TRI2 %>%
  mutate(
    ENG_TTL = (HH_1PH*59544.65) + (HH_2PH*91988.08) + (HH_3PH*108451.1) + (HH_4PH*128971.6) + 
      (HH_5PH*125381.9) + (HH_6PH*130221.2) + (HH_7OV*163132.5),
    ELC_TTL = (HH_1PH*17412.23) + (HH_2PH*27391.14) + (HH_3PH*30860.98) + (HH_4PH*39222.74) + 
      (HH_5PH*36871.54) + (HH_6PH*42111.24) + (HH_7OV*55334.93),
    NGS_TTL = (HH_1PH*28264.55) + (HH_2PH*39398.32) + (HH_3PH*51856.87) + (HH_4PH*59105.16) + 
      (HH_5PH*73064.9) + (HH_6PH*56748.47) + (HH_7OV*80412.91)
  )

# Combine with Census Blocks
geo_Blocks = st_read("G:/Shared drives/Projects/5035_Tech Equity/Geographies/RPA_Blocks_shp.shp")
csv_Blocks <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Geographies/RPA_Blocks_ref.csv')

geoSF1_ENG <- merge(geo_Blocks, SF1_10_TRI3, by = "GEOID10", all.x = TRUE)
st_write(geoSF1_ENG,
         "G:/Shared drives/Projects/5035_Tech Equity/Data/A_Shapefile/Scenario/ENERGY_TTL.shp", 
         driver = "ESRI Shapefile")

#################################################################################


acs19_5yr_NY <- get_acs(geography = "tract", 
                        variables = c("B08008_001" # Estimate!!Total
                        ),
                        state = "NY", county = c("005", "027", "047", "059", "061", 
                                                 "071", "079", "081", "085", "087", 
                                                 "103", "105", "111", "119"),
                        survey = "acs5", year = 2019)

acs19_5yr_NJ <- get_acs(geography = "tract", 
                        variables = c("B08008_001" # Estimate!!Total
                        ),
                        state = "NJ", county = c("003", "013", "017", "019", "021", 
                                                 "023", "025", "027", "029", "031", 
                                                 "035", "037", "039", "041"),
                        survey = "acs5", year = 2019)

acs19_5yr_CT <- get_acs(geography = "tract", 
                        variables = c("B08008_001" # Estimate!!Total
                        ),
                        state = "CT", county = c("001", "005", "009"),
                        survey = "acs5", year = 2019)

acs19_5yr_TRI <- rbind(acs19_5yr_NY, acs19_5yr_NJ, acs19_5yr_CT)
acs19_5yr_TRI2 <-
acs19_5yr_TRI %>% 
  cast(GEOID~variable) %>%
  dplyr::rename(
    GEOID10 = "GEOID",
    N_WORK = "B08008_001" # Estimate!!Total
  )

#################################################################################



# EPLORING MICRO DATA

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
               list(name = sum)) # Weighted total fuel consumption
  
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



