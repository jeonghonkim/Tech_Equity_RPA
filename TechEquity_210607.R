

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


# Check CT data

FCC_DPL20_NY <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/NY-Fixed-Jun2020-v1.csv')
FCC_DPL20_NJ <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/NJ-Fixed-Jun2020-v1.csv')
FCC_DPL20_CT <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/CT-Fixed-Jun2020-v1.csv')

FCC_DPL16_NY <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/NY-Fixed-Jun2016-v4.csv')
FCC_DPL16_NJ <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/NJ-Fixed-Jun2016-v4.csv')
FCC_DPL16_CT <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/CT-Fixed-Jun2016-v4.csv')

FCC_DPL20_NY$BlockCode = stri_pad_left(FCC_DPL20_NY$BlockCode, 15)
FCC_DPL16_NY$BlockCode = stri_pad_left(FCC_DPL16_NY$BlockCode, 15)

FCC_DPL20_NJ$BlockCode = stri_pad_left(FCC_DPL20_NJ$BlockCode, 15)
FCC_DPL16_NJ$BlockCode = stri_pad_left(FCC_DPL16_NJ$BlockCode, 15)

FCC_DPL20_CT$BlockCode = stri_pad_left(FCC_DPL20_CT$BlockCode, 15, "0")
FCC_DPL16_CT$BlockCode = stri_pad_left(FCC_DPL16_CT$BlockCode, 15, "0")

FCC_DPL20_TRI <- rbind(FCC_DPL20_NY, FCC_DPL20_NJ, FCC_DPL20_CT)
FCC_DPL16_TRI <- rbind(FCC_DPL16_NY, FCC_DPL16_NJ, FCC_DPL16_CT)

View(FCC_DPL20_TRI)


FCC_DPL20_TRI_MAX <- 
  FCC_DPL20_TRI %>%
  dplyr::rename(
    TecCode20 = "TechCode",
    Consumr20 = "Consumer",
    MaxAdDn20 = "MaxAdDown",
    MaxAdUp20 = "MaxAdUp",
    Busines20 = "Business"
  ) %>%
  select(BlockCode:Busines20) %>%
  group_by(BlockCode) %>%
  slice(which.max(MaxAdDn20)) %>%
  dplyr::rename(
    GEOID10 = "BlockCode"
  )

FCC_DPL16_TRI_MAX <-
  FCC_DPL16_TRI %>%
  dplyr::rename(
    TecCode16 = "TechCode",
    Consumr16 = "Consumer",
    MaxAdDn16 = "MaxAdDown",
    MaxAdUp16 = "MaxAdUp",
    Busines16 = "Business",
    MxCIRDn16 = "MaxCIRDown",
    MxCIRUp16 = "MaxCIRUp"
  ) %>%
  select(BlockCode:MxCIRUp16) %>%
  group_by(BlockCode) %>%
  slice(which.max(MaxAdDn16)) %>%
  dplyr::rename(
    GEOID10 = "BlockCode"
  )

write.csv(FCC_DPL20_TRI_MAX, file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/FCC_DPL20_TRI_MAX.csv')
write.csv(FCC_DPL16_TRI_MAX, file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/FCC_DPL16_TRI_MAX.csv')

FCC_DPL20_TRI_MAX <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/FCC_DPL20_TRI_MAX.csv')
FCC_DPL16_TRI_MAX <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/FCC_DPL16_TRI_MAX.csv')

View(FCC_DPL20_TRI_MAX)
View(FCC_DPL16_TRI_MAX)
colnames(FCC_DPL20_TRI_MAX)
colnames(FCC_DPL16_TRI_MAX)

geoFCC_DPL20_TRI_MAX <- merge(geo_Blocks, FCC_DPL20_TRI_MAX, by = "GEOID10", all.x = TRUE)
geoFCC_DPL2016_TRI_MAX <- merge(geoFCC_DPL20_TRI_MAX, FCC_DPL16_TRI_MAX, by = "GEOID10", all.x = TRUE)

st_write(geoFCC_DPL2016_TRI_MAX,
         "G:/Shared drives/Projects/5035_Tech Equity/Data/A_Shapefile/Broadband_Data/FCC/FCC_DPL2016_TRI_MAX.shp", 
         driver = "ESRI Shapefile")

geoFCC_DPL2016_MAX = st_read("G:/Shared drives/Projects/5035_Tech Equity/Data/A_Shapefile/Broadband_Data/FCC/FCC_DPL_MAX_2016.shp")























#################################################################################


# Step 2. Percentage of households with at least 100 Mbps
#   2-1. Percentage of households in census blocks with at least 100 Mbps in 2016 by census tracts
#   2-2. Percentage of households in census blocks with at least 100 Mbps in 2020 by census tracts


View(geoFCC_DPL2016_MAX)

geoFCC_DPL2016_MAX_100OV <-
geoFCC_DPL2016_MAX %>%
  mutate(
    MAXADN20_100OV = case_when(
      MXADN20 >= 100 ~ 1,
      MXADN20 < 100 ~ 0
    ),
    MAXADN16_100OV = case_when(
      MXADN16 >= 100 ~ 1,
      MXADN16 < 100 ~ 0
    )
  )

FCC_DPL20_ATL100_MAX

FCC_DPL20 %>%
  mutate(
    GEOID10 = as.character(BlockCode)
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
  slice(which.max(MXADN20)) %>%
  filter(MXADN20 >= 100)


FCC_DPL20_ATL100_MIN
FCC_DPL20 %>%
  mutate(
    GEOID10 = as.character(BlockCode)
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

# Max Download - But, CT data is vacant

View(FCC_DPL20)
colnames(FCC_DPL20)

FCC_DPL20_MAX <-
  FCC_DPL20 %>%
  mutate(
    GEOID10 = as.character(BlockCode)
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
    GEOID10 = as.character(BlockCode)
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

# Write FCC maximum file to csv
write.csv(FCC_DPL20_MAX, file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/FCC_DPL20_MAX.csv')
write.csv(FCC_DPL16_MAX, file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/FCC_DPL16_MAX.csv')

FCC_DPL20_MAX <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/FCC_DPL20_MAX.csv')
FCC_DPL16_MAX <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/FCC_DPL16_MAX.csv')

# Merge with Census Block Shapefile
geoFCC_DPL20_MAX <- merge(geo_Blocks, FCC_DPL20_MAX, by = "GEOID10", all.x = TRUE)
geoFCC_DPL2016_MAX <- merge(geoFCC_DPL20_MAX, FCC_DPL16_MAX, by = "GEOID10", all.x = TRUE)

# Write Shapefile
st_write(geoFCC_DPL2016_MAX,
         "G:/Shared drives/Projects/5035_Tech Equity/Data/A_Shapefile/Broadband_Data/FCC/FCC_DPL_MAX_2016.shp", 
         driver = "ESRI Shapefile")

geoFCC_DPL2016_MAX = st_read("G:/Shared drives/Projects/5035_Tech Equity/Data/A_Shapefile/Broadband_Data/FCC/FCC_DPL_MAX_2016.shp")





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



