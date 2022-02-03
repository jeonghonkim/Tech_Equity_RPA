

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



