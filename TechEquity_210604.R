

# June 4. 2021
# 1. Tech Equity Initiative

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
setwd("G:/Shared drives/Projects/5035_Tech Equity/Data")

#################################################################################


# Make Visualizations for the presentation

# 1. Maximum Broadband Speed
#   1-1. Maximum broadband speed available in 2016 for RPA region
#   1-2. Maximum broadband speed available in 2020 for RPA region

# 2. Percentage of households with at least 100 Mbps
#   2-1. Percentage of households in census blocks with at least 100 Mbps in 2016 by census tracts
#   2-2. Percentage of households in census blocks with at least 100 Mbps in 2020 by census tracts


#################################################################################


# Step 0. Read Files

# 0-1. Read Geographies - Census Blocks, Tracts, Counties
geo_Blocks = st_read("G:/Shared drives/Projects/5035_Tech Equity/Geographies/RPA_Blocks_shp.shp")
geo_Tracts = st_read("G:/Shared drives/Projects/5035_Tech Equity/Geographies/RPA_tracts_shp.shp")
geo_Counties = st_read("G:/Shared drives/Projects/5035_Tech Equity/Geographies/RPA_counties_shp.shp")

# 0-2. Read FCC Deployment Data
FCC_DPL20 <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/FCC_DPL20.csv')
FCC_DPL16 <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/FCC_DPL16.csv')

# 0-3. Read FCC Area Table
FCC_AREA16 <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/FCC_ARA16.csv')
FCC_AREA20 <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/FCC_ARA20.csv')
FCC_AREA16$id = stri_pad_left(FCC_AREA16$id, 5, "0")
FCC_AREA20$id = stri_pad_left(FCC_AREA20$id, 5, "0")


#################################################################################

# Step 1. Maximum Download Speeds by Census Block
#   1-1. Maximum broadband speed available in 2016 for RPA region
#   1-2. Maximum broadband speed available in 2020 for RPA region

View(FCC_DPL20)
colnames(FCC_DPL20)

FCC_DPL20_MAX <-
  FCC_DPL20 %>%
  mutate(
    GEOID10 = str_pad(as.character(BlockCode), width = 15, pad = "0")
  ) %>%
  select(TechCode:GEOID10) %>%
  dplyr::rename(
    TECCD20 = "TechCode",
    CNSMR20 = "Consumer",
    MXADN20 = "MaxAdDown",
    MXAUP20 = "MaxAdUp",
    BSNSS20 = "Business"
  ) %>%
  relocate(GEOID10) %>%
  group_by(GEOID10) %>%
  slice(which.max(MXADN20))

FCC_DPL16_MAX <-
  FCC_DPL16 %>%
  mutate(
    GEOID10 = str_pad(as.character(BlockCode), width = 15, pad = "0")
  ) %>%
  select(TechCode:GEOID10) %>%
  dplyr::rename(
    TECCD16 = "TechCode",
    CNSMR16 = "Consumer",
    MXADN16 = "MaxAdDown",
    MXAUP16 = "MaxAdUp",
    BSNSS16 = "Business",
    MXCDN16 = "MaxCIRDown",
    MXCUP16 = "MaxCIRUp"
  ) %>%
  relocate(GEOID10) %>%
  group_by(GEOID10) %>%
  slice(which.max(MXADN16))

geoFCC_DPL20_MAX <- merge(geo_Blocks, FCC_DPL20_MAX, by = "GEOID10", all.x = TRUE)
geoFCC_DPL2016_MAX <- merge(geoFCC_DPL20_MAX, FCC_DPL16_MAX, by = "GEOID10", all.x = TRUE)

View(geoFCC_DPL2016_MAX)
st_write(geoFCC_DPL2016_MAX,
         "G:/Shared drives/Projects/5035_Tech Equity/Data/A_Shapefile/Broadband_Data/FCC/FCC_DPL_MAX_2016_2.shp", 
         driver = "ESRI Shapefile")


#################################################################################

# Step 2. Percentage of households with at least 100 Mbps
#   2-1. Percentage of households in census blocks with at least 100 Mbps in 2016 by census tracts
#   2-2. Percentage of households in census blocks with at least 100 Mbps in 2020 by census tracts

View(FCC_DPL20)
colnames(FCC_DPL20)

FCC_DPL20_MAX

FCC_DPL20_ATL100_MAX
FCC_DPL20 %>%
  mutate(
    GEOID10 = str_pad(as.character(BlockCode), width = 15, pad = "0")
  ) %>%
  select(TechCode:GEOID10) %>%
  dplyr::rename(
    TECCD20 = "TechCode",
    CNSMR20 = "Consumer",
    MXADN20 = "MaxAdDown",
    MXAUP20 = "MaxAdUp",
    BSNSS20 = "Business"
  ) %>%
  relocate(GEOID10) %>% 
  filter(MXADN20 > 100) %>%
  group_by(GEOID10) %>%
  slice(which.max(MXADN20)) %>%
    head()

FCC_DPL20_ATL100_MIN
FCC_DPL20 %>%
  mutate(
    GEOID10 = str_pad(as.character(BlockCode), width = 15, pad = "0")
  ) %>%
  select(TechCode:GEOID10) %>%
  dplyr::rename(
    TECCD20 = "TechCode",
    CNSMR20 = "Consumer",
    MXADN20 = "MaxAdDown",
    MXAUP20 = "MaxAdUp",
    BSNSS20 = "Business"
  ) %>%
  relocate(GEOID10) %>% 
  filter(MXADN20 > 100) %>%
  group_by(GEOID10, TECCD20) %>%
  slice(which.min(MXADN20)) %>%
  head()






























#################################################################################
#################################################################################
#################################################################################


FCC_AREA16 <-
  FCC_AREA16 %>%
  select(id, speed:has_3more) %>%
  filter(
    id == "09001" | id == "09005" | id == "09009" | id == "34003" | id == "34013" | 
      id == "34017" | id == "34019" | id == "34021" | id == "34023" | id == "34025" | 
      id == "34027" | id == "34029" | id == "34031" | id == "34035" | id == "34037" | 
      id == "34039" | id == "34041" | id == "36005" | id == "36027" | id == "36047" | 
      id == "36059" | id == "36061" | id == "36071" | id == "36079" | id == "36081" | 
      id == "36085" | id == "36087" | id == "36103" | id == "36105" | id == "36111" | 
      id == "36119"
  )

FCC_AREA20 <-
  FCC_AREA20 %>%
  select(id, speed:has_3more) %>%
  filter(
    id == "09001" | id == "09005" | id == "09009" | id == "34003" | id == "34013" | 
      id == "34017" | id == "34019" | id == "34021" | id == "34023" | id == "34025" | 
      id == "34027" | id == "34029" | id == "34031" | id == "34035" | id == "34037" | 
      id == "34039" | id == "34041" | id == "36005" | id == "36027" | id == "36047" | 
      id == "36059" | id == "36061" | id == "36071" | id == "36079" | id == "36081" | 
      id == "36085" | id == "36087" | id == "36103" | id == "36105" | id == "36111" | 
      id == "36119"
  )

write.csv(FCC_AREA16, file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/FCC_ARA16.csv')
write.csv(FCC_AREA20, file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/FCC_ARA20.csv')

# Calculate Sum of Providers by Census Counties
FCC_AREA16_SUM <-
  FCC_AREA16 %>%
  group_by(id, speed) %>%
  summarise(has_0_sum = sum(has_0),
            has_1_sum = sum(has_1),
            has_2_sum = sum(has_2),
            has_3more_sum = sum(has_3more)
  ) %>%
  dplyr::rename(
    FIPS = "id",
    SPD16 = "speed",
    H0_16 = "has_0_sum",
    H1_16 = "has_1_sum",
    H2_16 = "has_2_sum",
    H3M16 = "has_3more_sum"
  )

FCC_AREA20_SUM <-
  FCC_AREA20 %>%
  group_by(id, speed) %>%
  summarise(has_0_sum = sum(has_0),
            has_1_sum = sum(has_1),
            has_2_sum = sum(has_2),
            has_3more_sum = sum(has_3more)
  ) %>%
  dplyr::rename(
    FIPS = "id",
    SPD20 = "speed",
    H0_20 = "has_0_sum",
    H1_20 = "has_1_sum",
    H2_20 = "has_2_sum",
    H3M20 = "has_3more_sum"
  )



