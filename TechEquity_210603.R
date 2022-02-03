
# Tech Equity Initiative
#   1) Creating IPUMS layers
#       https://usa.ipums.org/usa/
#       Sample: USA, ACS 2019 1-YEAR AND ACS 2014-2019 5-YEAR
#       Variables: CINETHH, CISMRTPHN, CIDATAPLN, and CIHISPEED

# rm(list=ls())
# install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", "ggspatial", "libwgeom", 
#                     "rnaturalearth", "rnaturalearthdata", "devtools", "purrr", "highcharter"))

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

# Get Census Blocks & Counties
geo_Blocks = st_read("G:/Shared drives/Projects/5035_Tech Equity/Geographies/RPA_Blocks_shp.shp")
geo_Counties = st_read("G:/Shared drives/Projects/5035_Tech Equity/Geographies/RPA_counties_shp.shp")

#################################################################################

# 1. Make FCC data and layer by Number of Providers

  # 1) 2016 FCC Area Table
P_FCC_AREA_16 <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/Area_Table_Dec2016.csv')
P_FCC_AREA_20 <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/Area_Table_June_2020_V1.csv')

P_FCC_AREA_16$id = stri_pad_left(P_FCC_AREA_16$id, 5, "0")
P_FCC_AREA_20$id = stri_pad_left(P_FCC_AREA_20$id, 5, "0")

FCC_AREA16 <-
P_FCC_AREA_16 %>%
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
  P_FCC_AREA_20 %>%
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

# FCC16
# FCC16 - 0 PROVIDERS
FCC_AREA16_SUM_0 <-
FCC_AREA16_SUM %>%
  select(FIPS:H0_16) %>%
  cast(FIPS~SPD16) %>%
  dplyr::rename(
    H0_16_0.2 = "0.2",
    H0_16_4 = "4",
    H0_16_10 = "10",
    H0_16_25 = "25",
    H0_16_100 = "100",
    H0_16_250 = "250",
    H0_16_1000 = "1000"
  ) 

# FCC16 - 1 PROVIDERS
FCC_AREA16_SUM_1 <- 
FCC_AREA16_SUM %>%
  select(FIPS:SPD16, H1_16) %>%
  cast(FIPS~SPD16) %>%
  dplyr::rename(
    H1_16_0.2 = "0.2",
    H1_16_4 = "4",
    H1_16_10 = "10",
    H1_16_25 = "25",
    H1_16_100 = "100",
    H1_16_250 = "250",
    H1_16_1000 = "1000"
  ) 

# FCC16 - 2 PROVIDERS
FCC_AREA16_SUM_2 <-
FCC_AREA16_SUM %>%
  select(FIPS:SPD16, H2_16) %>%
  cast(FIPS~SPD16) %>%
  dplyr::rename(
    H2_16_0.2 = "0.2",
    H2_16_4 = "4",
    H2_16_10 = "10",
    H2_16_25 = "25",
    H2_16_100 = "100",
    H2_16_250 = "250",
    H2_16_1000 = "1000"
  ) 

# FCC16 - 3 PROVIDERS
FCC_AREA16_SUM_3  <-
FCC_AREA16_SUM %>%
  select(FIPS:SPD16, H3M16) %>%
  cast(FIPS~SPD16) %>%
  dplyr::rename(
    H3_16_0.2 = "0.2",
    H3_16_4 = "4",
    H3_16_10 = "10",
    H3_16_25 = "25",
    H3_16_100 = "100",
    H3_16_250 = "250",
    H3_16_1000 = "1000"
  ) 

# Merge with Geographies one by one
geoFCC16_Counties_00 <- merge(geo_Counties, FCC_AREA16_SUM_0, by = "FIPS", all.x = TRUE)
geoFCC16_Counties_01 <- merge(geoFCC16_Counties_00, FCC_AREA16_SUM_1, by = "FIPS", all.x = TRUE)
geoFCC16_Counties_12 <- merge(geoFCC16_Counties_01, FCC_AREA16_SUM_2, by = "FIPS", all.x = TRUE)
geoFCC16_Counties_23 <- merge(geoFCC16_Counties_12, FCC_AREA16_SUM_3, by = "FIPS", all.x = TRUE)
View(geoFCC16_Counties_23)

geoFCC_AREA16 <- geoFCC16_Counties_23
st_write(geoFCC_AREA16,
         "G:/Shared drives/Projects/5035_Tech Equity/Data/A_Shapefile/Broadband_Data/FCC/FCC16_PSP.shp", 
         driver = "ESRI Shapefile")

# FCC20
# FCC20 - 0 PROVIDERS
FCC_AREA20_SUM_0 <-
  FCC_AREA20_SUM %>%
  select(FIPS:H0_20) %>%
  cast(FIPS~SPD20) %>%
  dplyr::rename(
    H0_20_0.2 = "0.2",
    H0_20_4 = "4",
    H0_20_10 = "10",
    H0_20_25 = "25",
    H0_20_100 = "100",
    H0_20_250 = "250",
    H0_20_1000 = "1000"
  ) 

# FCC16 - 1 PROVIDERS
FCC_AREA20_SUM_1 <- 
  FCC_AREA20_SUM %>%
  select(FIPS:SPD20, H1_20) %>%
  cast(FIPS~SPD20) %>%
  dplyr::rename(
    H1_20_0.2 = "0.2",
    H1_20_4 = "4",
    H1_20_10 = "10",
    H1_20_25 = "25",
    H1_20_100 = "100",
    H1_20_250 = "250",
    H1_20_1000 = "1000"
  ) 

# FCC16 - 2 PROVIDERS
FCC_AREA20_SUM_2 <-
  FCC_AREA20_SUM %>%
  select(FIPS:SPD20, H2_20) %>%
  cast(FIPS~SPD20) %>%
  dplyr::rename(
    H2_20_0.2 = "0.2",
    H2_20_4 = "4",
    H2_20_10 = "10",
    H2_20_25 = "25",
    H2_20_100 = "100",
    H2_20_250 = "250",
    H2_20_1000 = "1000"
  ) 

# FCC16 - 3 PROVIDERS
FCC_AREA20_SUM_3  <-
  FCC_AREA20_SUM %>%
  select(FIPS:SPD20, H3M20) %>%
  cast(FIPS~SPD20) %>%
  dplyr::rename(
    H3_20_0.2 = "0.2",
    H3_20_4 = "4",
    H3_20_10 = "10",
    H3_20_25 = "25",
    H3_20_100 = "100",
    H3_20_250 = "250",
    H3_20_1000 = "1000"
  ) 

# Merge with Geographies one by one
geoFCC20_Counties_00 <- merge(geo_Counties, FCC_AREA20_SUM_0, by = "FIPS", all.x = TRUE)
geoFCC20_Counties_01 <- merge(geoFCC20_Counties_00, FCC_AREA20_SUM_1, by = "FIPS", all.x = TRUE)
geoFCC20_Counties_12 <- merge(geoFCC20_Counties_01, FCC_AREA20_SUM_2, by = "FIPS", all.x = TRUE)
geoFCC20_Counties_23 <- merge(geoFCC20_Counties_12, FCC_AREA20_SUM_3, by = "FIPS", all.x = TRUE)
View(geoFCC20_Counties_23)

geoFCC_AREA20 <- geoFCC20_Counties_23
st_write(geoFCC_AREA20,
         "G:/Shared drives/Projects/5035_Tech Equity/Data/A_Shapefile/Broadband_Data/FCC/FCC20_PSP.shp", 
         driver = "ESRI Shapefile")

#################################################################################

# 2. Make FCC data and layer by Tech Code

# Read the Combined FCC csv
FCC_TRI_20 <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/Tri-2020.csv')
FCC_TRI_19 <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/Tri-2019.csv')
FCC_TRI_18 <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/Tri-2018.csv')
FCC_TRI_17 <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/Tri-2017.csv')
FCC_TRI_16 <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/Tri-2016.csv')
FCC_TRI_15 <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/Tri-2015.csv')
colnames(FCC_TRI_20)
colnames(FCC_TRI_19)
colnames(FCC_TRI_18)
colnames(FCC_TRI_17)
colnames(FCC_TRI_16)
colnames(FCC_TRI_15)

# TechCode = 10
FCC_TRI20_TC10 <-
FCC_TRI_20 %>%
  filter(TechCode == 10) %>%
  mutate(
    GEOID10 = as.character(BlockCode)
  ) %>%
  select(TechCode:GEOID10) %>%
  dplyr::rename(
    CNSMR20 = "Consumer",
    MXADN20 = "MaxAdDown",
    MXAUP20 = "MaxAdUp",
    BSNSS20 = "Business"
  ) %>%
  relocate(GEOID10)

FCC_TRI19_TC10 <-
  FCC_TRI_19 %>%
  filter(TechCode == 10) %>%
  mutate(
    GEOID10 = as.character(BlockCode)
  ) %>%
  select(Consumer:GEOID10) %>%
  dplyr::rename(
    CNSMR19 = "Consumer",
    MXADN19 = "MaxAdDown",
    MXAUP19 = "MaxAdUp",
    BSNSS19 = "Business",
    MXCDN19 = "MaxCIRDown",
    MXCUP19 = "MaxCIRUp"
  ) %>%
  relocate(GEOID10)

FCC_TRI18_TC10 <-
  FCC_TRI_18 %>%
  filter(TechCode == 10) %>%
  mutate(
    GEOID10 = as.character(BlockCode)
  ) %>%
  select(Consumer:GEOID10) %>%
  dplyr::rename(
    CNSMR18 = "Consumer",
    MXADN18 = "MaxAdDown",
    MXAUP18 = "MaxAdUp",
    BSNSS18 = "Business",
    MXCDN18 = "MaxCIRDown",
    MXCUP18 = "MaxCIRUp"
  ) %>%
  relocate(GEOID10)

FCC_TRI17_TC10 <-
  FCC_TRI_17 %>%
  filter(TechCode == 10) %>%
  mutate(
    GEOID10 = as.character(BlockCode)
  ) %>%
  select(Consumer:GEOID10) %>%
  dplyr::rename(
    CNSMR17 = "Consumer",
    MXADN17 = "MaxAdDown",
    MXAUP17 = "MaxAdUp",
    BSNSS17 = "Business",
    MXCDN17 = "MaxCIRDown",
    MXCUP17 = "MaxCIRUp"
  ) %>%
  relocate(GEOID10)

FCC_TRI16_TC10 <-
  FCC_TRI_16 %>%
  filter(TechCode == 10) %>%
  mutate(
    GEOID10 = as.character(BlockCode)
  ) %>%
  select(Consumer:GEOID10) %>%
  dplyr::rename(
    CNSMR16 = "Consumer",
    MXADN16 = "MaxAdDown",
    MXAUP16 = "MaxAdUp",
    BSNSS16 = "Business",
    MXCDN16 = "MaxCIRDown",
    MXCUP16 = "MaxCIRUp"
  ) %>%
  relocate(GEOID10)

FCC_TRI15_TC10 <-
  FCC_TRI_15 %>%
  filter(TechCode == 10) %>%
  mutate(
    GEOID10 = as.character(BlockCode)
  ) %>%
  select(Consumer:GEOID10) %>%
  dplyr::rename(
    CNSMR15 = "Consumer",
    MXADN15 = "MaxAdDown",
    MXAUP15 = "MaxAdUp",
    BSNSS15 = "Business",
    MXCDN15 = "MaxCIRDown",
    MXCUP15 = "MaxCIRUp"
  ) %>%
  relocate(GEOID10)

colnames(geo_Blocks)  
colnames(FCC_TRI20_TC10)  

# Merging 1
geoFCC_Blocks_0020 <- merge(geo_Blocks, FCC_TRI20_TC10, by = "GEOID10", all.x = TRUE)
geoFCC_Blocks_TC10_1920 <- merge(geoFCC_Blocks_0020, FCC_TRI19_TC10, by = "GEOID10", all.x = TRUE) 
geoFCC_Blocks_TC10_1820 <- merge(geoFCC_Blocks_TC10_1920, FCC_TRI18_TC10, by = "GEOID10", all.x = TRUE)
geoFCC_Blocks_TC10_1720 <- merge(geoFCC_Blocks_TC10_1820, FCC_TRI17_TC10, by = "GEOID10", all.x = TRUE)
geoFCC_Blocks_TC10_1620 <- merge(geoFCC_Blocks_TC10_1720, FCC_TRI16_TC10, by = "GEOID10", all.x = TRUE)
# geoFCC_Blocks_TC10_1520 <- merge(geoFCC_Blocks_TC10_1620, FCC_TRI15_TC10, by = "GEOID10", all.x = TRUE)
View(geoFCC_Blocks_TC10_1620)

st_write(geoFCC_Blocks_TC10_1620,
         "G:/Shared drives/Projects/5035_Tech Equity/Data/A_Shapefile/Broadband_Data/FCC/FCC1620_Tech10_Blocks_shp.shp", 
         driver = "ESRI Shapefile")

# Merging 2
geoFCC_Blocks_1520 <-
list (geo_Blocks, FCC_TRI20_TC10, FCC_TRI19_TC10, FCC_TRI18_TC10,
      FCC_TRI17_TC10, FCC_TRI16_TC10, FCC_TRI15_TC10) %>%
  reduce(left_join, by = "GEOID10", all.x = TRUE)

st_write(geoFCC_Blocks_1520,
         "G:/Shared drives/Projects/5035_Tech Equity/Data/A_Shapefile/Broadband_Data/FCC/FCC1520_Tech10_Blocks_shp.shp", 
         driver = "ESRI Shapefile")



# TechCode = 11
FCC_TRI20_TC11 <-
  FCC_TRI_20 %>%
  filter(TechCode == 11) %>%
  mutate(
    GEOID10 = as.character(BlockCode)
  ) %>%
  select(TechCode:GEOID10) %>%
  dplyr::rename(
    CNSMR20 = "Consumer",
    MXADN20 = "MaxAdDown",
    MXAUP20 = "MaxAdUp",
    BSNSS20 = "Business"
  ) %>%
  relocate(GEOID10)

FCC_TRI19_TC11 <-
  FCC_TRI_19 %>%
  filter(TechCode == 11) %>%
  mutate(
    GEOID10 = as.character(BlockCode)
  ) %>%
  select(Consumer:GEOID10) %>%
  dplyr::rename(
    CNSMR19 = "Consumer",
    MXADN19 = "MaxAdDown",
    MXAUP19 = "MaxAdUp",
    BSNSS19 = "Business",
    MXCDN19 = "MaxCIRDown",
    MXCUP19 = "MaxCIRUp"
  ) %>%
  relocate(GEOID10)

FCC_TRI18_TC11 <-
  FCC_TRI_18 %>%
  filter(TechCode == 11) %>%
  mutate(
    GEOID10 = as.character(BlockCode)
  ) %>%
  select(Consumer:GEOID10) %>%
  dplyr::rename(
    CNSMR18 = "Consumer",
    MXADN18 = "MaxAdDown",
    MXAUP18 = "MaxAdUp",
    BSNSS18 = "Business",
    MXCDN18 = "MaxCIRDown",
    MXCUP18 = "MaxCIRUp"
  ) %>%
  relocate(GEOID10)

FCC_TRI17_TC11 <-
  FCC_TRI_17 %>%
  filter(TechCode == 11) %>%
  mutate(
    GEOID10 = as.character(BlockCode)
  ) %>%
  select(Consumer:GEOID10) %>%
  dplyr::rename(
    CNSMR17 = "Consumer",
    MXADN17 = "MaxAdDown",
    MXAUP17 = "MaxAdUp",
    BSNSS17 = "Business",
    MXCDN17 = "MaxCIRDown",
    MXCUP17 = "MaxCIRUp"
  ) %>%
  relocate(GEOID10)

FCC_TRI16_TC11 <-
  FCC_TRI_16 %>%
  filter(TechCode == 11) %>%
  mutate(
    GEOID10 = as.character(BlockCode)
  ) %>%
  select(Consumer:GEOID10) %>%
  dplyr::rename(
    CNSMR16 = "Consumer",
    MXADN16 = "MaxAdDown",
    MXAUP16 = "MaxAdUp",
    BSNSS16 = "Business",
    MXCDN16 = "MaxCIRDown",
    MXCUP16 = "MaxCIRUp"
  ) %>%
  relocate(GEOID10)

FCC_TRI15_TC11 <-
  FCC_TRI_15 %>%
  filter(TechCode == 11) %>%
  mutate(
    GEOID10 = as.character(BlockCode)
  ) %>%
  select(Consumer:GEOID10) %>%
  dplyr::rename(
    CNSMR15 = "Consumer",
    MXADN15 = "MaxAdDown",
    MXAUP15 = "MaxAdUp",
    BSNSS15 = "Business",
    MXCDN15 = "MaxCIRDown",
    MXCUP15 = "MaxCIRUp"
  ) %>%
  relocate(GEOID10)

geoFCC_Blocks_TC11_1620 <-
  list (geo_Blocks, FCC_TRI20_TC11, FCC_TRI19_TC11, FCC_TRI18_TC11,
        FCC_TRI17_TC11, FCC_TRI16_TC11) %>%
  reduce(left_join, by = "GEOID10", all.x = TRUE)

st_write(geoFCC_Blocks_TC11_1620,
         "G:/Shared drives/Projects/5035_Tech Equity/Data/A_Shapefile/Broadband_Data/FCC/FCC1620_Tech11_Blocks_shp.shp", 
         driver = "ESRI Shapefile")

# TechCode = 12
FCC_TRI20_TC12 <-
  FCC_TRI_20 %>%
  filter(TechCode == 12) %>%
  mutate(
    GEOID10 = as.character(BlockCode)
  ) %>%
  select(TechCode:GEOID10) %>%
  dplyr::rename(
    CNSMR20 = "Consumer",
    MXADN20 = "MaxAdDown",
    MXAUP20 = "MaxAdUp",
    BSNSS20 = "Business"
  ) %>%
  relocate(GEOID10)

FCC_TRI19_TC12 <-
  FCC_TRI_19 %>%
  filter(TechCode == 12) %>%
  mutate(
    GEOID10 = as.character(BlockCode)
  ) %>%
  select(Consumer:GEOID10) %>%
  dplyr::rename(
    CNSMR19 = "Consumer",
    MXADN19 = "MaxAdDown",
    MXAUP19 = "MaxAdUp",
    BSNSS19 = "Business",
    MXCDN19 = "MaxCIRDown",
    MXCUP19 = "MaxCIRUp"
  ) %>%
  relocate(GEOID10)

FCC_TRI18_TC12 <-
  FCC_TRI_18 %>%
  filter(TechCode == 12) %>%
  mutate(
    GEOID10 = as.character(BlockCode)
  ) %>%
  select(Consumer:GEOID10) %>%
  dplyr::rename(
    CNSMR18 = "Consumer",
    MXADN18 = "MaxAdDown",
    MXAUP18 = "MaxAdUp",
    BSNSS18 = "Business",
    MXCDN18 = "MaxCIRDown",
    MXCUP18 = "MaxCIRUp"
  ) %>%
  relocate(GEOID10)

FCC_TRI17_TC12 <-
  FCC_TRI_17 %>%
  filter(TechCode == 12) %>%
  mutate(
    GEOID10 = as.character(BlockCode)
  ) %>%
  select(Consumer:GEOID10) %>%
  dplyr::rename(
    CNSMR17 = "Consumer",
    MXADN17 = "MaxAdDown",
    MXAUP17 = "MaxAdUp",
    BSNSS17 = "Business",
    MXCDN17 = "MaxCIRDown",
    MXCUP17 = "MaxCIRUp"
  ) %>%
  relocate(GEOID10)

FCC_TRI16_TC12 <-
  FCC_TRI_16 %>%
  filter(TechCode == 12) %>%
  mutate(
    GEOID10 = as.character(BlockCode)
  ) %>%
  select(Consumer:GEOID10) %>%
  dplyr::rename(
    CNSMR16 = "Consumer",
    MXADN16 = "MaxAdDown",
    MXAUP16 = "MaxAdUp",
    BSNSS16 = "Business",
    MXCDN16 = "MaxCIRDown",
    MXCUP16 = "MaxCIRUp"
  ) %>%
  relocate(GEOID10)

FCC_TRI15_TC12 <-
  FCC_TRI_15 %>%
  filter(TechCode == 12) %>%
  mutate(
    GEOID10 = as.character(BlockCode)
  ) %>%
  select(Consumer:GEOID10) %>%
  dplyr::rename(
    CNSMR15 = "Consumer",
    MXADN15 = "MaxAdDown",
    MXAUP15 = "MaxAdUp",
    BSNSS15 = "Business",
    MXCDN15 = "MaxCIRDown",
    MXCUP15 = "MaxCIRUp"
  ) %>%
  relocate(GEOID10)

geoFCC_Blocks_TC12_1620 <-
  list (geo_Blocks, FCC_TRI20_TC12, FCC_TRI19_TC12, FCC_TRI18_TC12,
        FCC_TRI17_TC12, FCC_TRI16_TC12) %>%
  reduce(left_join, by = "GEOID10", all.x = TRUE)

st_write(geoFCC_Blocks_TC12_1620,
         "G:/Shared drives/Projects/5035_Tech Equity/Data/A_Shapefile/Broadband_Data/FCC/FCC1520_Tech12_Blocks_shp.shp", 
         driver = "ESRI Shapefile")



# TechCode = 30
FCC_TRI20_TC30 <-
  FCC_TRI_20 %>%
  filter(TechCode == 30) %>%
  mutate(
    GEOID10 = as.character(BlockCode)
  ) %>%
  select(TechCode:GEOID10) %>%
  dplyr::rename(
    CNSMR20 = "Consumer",
    MXADN20 = "MaxAdDown",
    MXAUP20 = "MaxAdUp",
    BSNSS20 = "Business"
  ) %>%
  relocate(GEOID10)

FCC_TRI19_TC30 <-
  FCC_TRI_19 %>%
  filter(TechCode == 30) %>%
  mutate(
    GEOID10 = as.character(BlockCode)
  ) %>%
  select(Consumer:GEOID10) %>%
  dplyr::rename(
    CNSMR19 = "Consumer",
    MXADN19 = "MaxAdDown",
    MXAUP19 = "MaxAdUp",
    BSNSS19 = "Business",
    MXCDN19 = "MaxCIRDown",
    MXCUP19 = "MaxCIRUp"
  ) %>%
  relocate(GEOID10)

FCC_TRI18_TC30 <-
  FCC_TRI_18 %>%
  filter(TechCode == 30) %>%
  mutate(
    GEOID10 = as.character(BlockCode)
  ) %>%
  select(Consumer:GEOID10) %>%
  dplyr::rename(
    CNSMR18 = "Consumer",
    MXADN18 = "MaxAdDown",
    MXAUP18 = "MaxAdUp",
    BSNSS18 = "Business",
    MXCDN18 = "MaxCIRDown",
    MXCUP18 = "MaxCIRUp"
  ) %>%
  relocate(GEOID10)

FCC_TRI17_TC30 <-
  FCC_TRI_17 %>%
  filter(TechCode == 30) %>%
  mutate(
    GEOID10 = as.character(BlockCode)
  ) %>%
  select(Consumer:GEOID10) %>%
  dplyr::rename(
    CNSMR17 = "Consumer",
    MXADN17 = "MaxAdDown",
    MXAUP17 = "MaxAdUp",
    BSNSS17 = "Business",
    MXCDN17 = "MaxCIRDown",
    MXCUP17 = "MaxCIRUp"
  ) %>%
  relocate(GEOID10)

FCC_TRI16_TC30 <-
  FCC_TRI_16 %>%
  filter(TechCode == 30) %>%
  mutate(
    GEOID10 = as.character(BlockCode)
  ) %>%
  select(Consumer:GEOID10) %>%
  dplyr::rename(
    CNSMR16 = "Consumer",
    MXADN16 = "MaxAdDown",
    MXAUP16 = "MaxAdUp",
    BSNSS16 = "Business",
    MXCDN16 = "MaxCIRDown",
    MXCUP16 = "MaxCIRUp"
  ) %>%
  relocate(GEOID10)

geoFCC_Blocks_TC30_1620 <-
  list (geo_Blocks, FCC_TRI20_TC30, FCC_TRI19_TC30, FCC_TRI18_TC30,
        FCC_TRI17_TC30, FCC_TRI16_TC30) %>%
  reduce(left_join, by = "GEOID10", all.x = TRUE)

st_write(geoFCC_Blocks_TC30_1620,
         "G:/Shared drives/Projects/5035_Tech Equity/Data/A_Shapefile/Broadband_Data/FCC/FCC1620_Tech30_Blocks_shp.shp", 
         driver = "ESRI Shapefile")



# TechCode = 70
FCC_TRI20_TC70 <-
  FCC_TRI_20 %>%
  filter(TechCode == 70) %>%
  mutate(
    GEOID10 = as.character(BlockCode)
  ) %>%
  select(TechCode:GEOID10) %>%
  dplyr::rename(
    CNSMR20 = "Consumer",
    MXADN20 = "MaxAdDown",
    MXAUP20 = "MaxAdUp",
    BSNSS20 = "Business"
  ) %>%
  relocate(GEOID10)

FCC_TRI19_TC70 <-
  FCC_TRI_19 %>%
  filter(TechCode == 70) %>%
  mutate(
    GEOID10 = as.character(BlockCode)
  ) %>%
  select(Consumer:GEOID10) %>%
  dplyr::rename(
    CNSMR19 = "Consumer",
    MXADN19 = "MaxAdDown",
    MXAUP19 = "MaxAdUp",
    BSNSS19 = "Business",
    MXCDN19 = "MaxCIRDown",
    MXCUP19 = "MaxCIRUp"
  ) %>%
  relocate(GEOID10)

FCC_TRI18_TC70 <-
  FCC_TRI_18 %>%
  filter(TechCode == 70) %>%
  mutate(
    GEOID10 = as.character(BlockCode)
  ) %>%
  select(Consumer:GEOID10) %>%
  dplyr::rename(
    CNSMR18 = "Consumer",
    MXADN18 = "MaxAdDown",
    MXAUP18 = "MaxAdUp",
    BSNSS18 = "Business",
    MXCDN18 = "MaxCIRDown",
    MXCUP18 = "MaxCIRUp"
  ) %>%
  relocate(GEOID10)

FCC_TRI17_TC70 <-
  FCC_TRI_17 %>%
  filter(TechCode == 70) %>%
  mutate(
    GEOID10 = as.character(BlockCode)
  ) %>%
  select(Consumer:GEOID10) %>%
  dplyr::rename(
    CNSMR17 = "Consumer",
    MXADN17 = "MaxAdDown",
    MXAUP17 = "MaxAdUp",
    BSNSS17 = "Business",
    MXCDN17 = "MaxCIRDown",
    MXCUP17 = "MaxCIRUp"
  ) %>%
  relocate(GEOID10)

FCC_TRI16_TC70 <-
  FCC_TRI_16 %>%
  filter(TechCode == 70) %>%
  mutate(
    GEOID10 = as.character(BlockCode)
  ) %>%
  select(Consumer:GEOID10) %>%
  dplyr::rename(
    CNSMR16 = "Consumer",
    MXADN16 = "MaxAdDown",
    MXAUP16 = "MaxAdUp",
    BSNSS16 = "Business",
    MXCDN16 = "MaxCIRDown",
    MXCUP16 = "MaxCIRUp"
  ) %>%
  relocate(GEOID10)

geoFCC_Blocks_TC70_1620 <-
  list (geo_Blocks, FCC_TRI20_TC70, FCC_TRI19_TC70, FCC_TRI18_TC70,
        FCC_TRI17_TC70, FCC_TRI16_TC70) %>%
  reduce(left_join, by = "GEOID10", all.x = TRUE)

st_write(geoFCC_Blocks_TC70_1620,
         "G:/Shared drives/Projects/5035_Tech Equity/Data/A_Shapefile/Broadband_Data/FCC/FCC1620_Tech70_Blocks_shp.shp", 
         driver = "ESRI Shapefile")


###################################################################################
colnames(FCC_TRI_20)

# Maximum Download speed by Census Blocks
FCC_TRI_20_MAX <-
FCC_TRI_20 %>%
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

FCC_TRI_19_MAX <-
  FCC_TRI_19 %>%
  mutate(
    GEOID10 = as.character(BlockCode)
  ) %>%
  select(TechCode:GEOID10) %>%
  dplyr::rename(
    TECCD19 = "TechCode",
    CNSMR19 = "Consumer",
    MXADN19 = "MaxAdDown",
    MXAUP19 = "MaxAdUp",
    BSNSS19 = "Business",
    MXCDN19 = "MaxCIRDown",
    MXCUP19 = "MaxCIRUp"
  ) %>%
  relocate(GEOID10) %>%
  group_by(GEOID10) %>%
  slice(which.max(MXADN19))

FCC_TRI_18_MAX <-
  FCC_TRI_18 %>%
  mutate(
    GEOID10 = as.character(BlockCode)
  ) %>%
  select(TechCode:GEOID10) %>%
  dplyr::rename(
    TECCD18 = "TechCode",
    CNSMR18 = "Consumer",
    MXADN18 = "MaxAdDown",
    MXAUP18 = "MaxAdUp",
    BSNSS18 = "Business",
    MXCDN18 = "MaxCIRDown",
    MXCUP18 = "MaxCIRUp"
  ) %>%
  relocate(GEOID10) %>%
  group_by(GEOID10) %>%
  slice(which.max(MXADN18))

FCC_TRI_17_MAX <-
  FCC_TRI_17 %>%
  mutate(
    GEOID10 = as.character(BlockCode)
  ) %>%
  select(TechCode:GEOID10) %>%
  dplyr::rename(
    TECCD17 = "TechCode",
    CNSMR17 = "Consumer",
    MXADN17 = "MaxAdDown",
    MXAUP17 = "MaxAdUp",
    BSNSS17 = "Business",
    MXCDN17 = "MaxCIRDown",
    MXCUP17 = "MaxCIRUp"
  ) %>%
  relocate(GEOID10) %>%
  group_by(GEOID10) %>%
  slice(which.max(MXADN17))

FCC_TRI_16_MAX <-
  FCC_TRI_16 %>%
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

# Merge
geoFCC_MAX_Block_0016 <- merge(geo_Blocks, FCC_TRI_16_MAX, by = "GEOID10", all.x = TRUE)
geoFCC_MAX_Block_1620 <- merge(geoFCC_MAX_Block_0016, FCC_TRI_20_MAX, by = "GEOID10", all.x = TRUE)

geoFCC_MAX1620 <- geoFCC_MAX_Block_1620
st_write(geoFCC_MAX1620,
         "G:/Shared drives/Projects/5035_Tech Equity/Data/A_Shapefile/Broadband_Data/FCC/FCC1620_MAX.shp", 
         driver = "ESRI Shapefile")

View(geoFCC_MAX1620)


#################################################################################


geoFCC_MAX1620_Block <-
  list (geo_Blocks, FCC_TRI_20_MAX, FCC_TRI_19_MAX, FCC_TRI_18_MAX,
        FCC_TRI_17_MAX, FCC_TRI_16_MAX) %>%
  reduce(left_join, by = "GEOID10", all.x = TRUE)

st_write(geoFCC_MAX1620_Block,
         "G:/Shared drives/Projects/5035_Tech Equity/Data/A_Shapefile/Broadband_Data/FCC/FCC_MAX1620_Block_shp.shp", 
         driver = "ESRI Shapefile")

colnames(geoFCC_MAX1620_Block)

mapview(geoFCC_MAX1620_Block, 
        zcol = c("MXADN20", "MXADN19", "MXADN18", "MXADN17", "MXADN16"), 
        legend = TRUE, 
        col.regions=brewer.pal(9, "RdYlGn"))

mapview(geoFCC_MAX1620_Block, 
        zcol = "MXADN20", 
        legend = TRUE, 
        col.regions=brewer.pal(9, "RdYlGn"))


#################################################################################
#################################################################################
  
# TECH CODE
#       20      19        18        17        16        15
# 0  -  0       
# 10 -  348188  O
# 11 -  115459  O
# 12 -  87886   O
# 20 -  1101    
# 30 -  142106  O
# 40 -  958     
# 41 -  853     
# 42 -  65041   
# 43 -  400046  
# 50 -  485147  
# 60 -  2270888 
# 70 -  64656   O
# 90 -  0       
  
#################################################################################


# Download Census Blocks
# Connecticut
CT_001 <- blocks(state = "CT", county = "Fairfield")
CT_005 <- blocks(state = "CT", county = "Litchfield")
CT_009 <- blocks(state = "CT", county = "New Haven")
CT_All <- rbind(CT_001, CT_005, CT_009)

# New Jersey
NJ_003 <- blocks(state = "NJ", county = 003)
NJ_013 <- blocks(state = 34, county = 013)
NJ_017 <- blocks(state = 34, county = 017)
NJ_019 <- blocks(state = 34, county = 019)
NJ_021 <- blocks(state = 34, county = 021)
NJ_023 <- blocks(state = 34, county = 023)
NJ_025 <- blocks(state = 34, county = 025)
NJ_027 <- blocks(state = 34, county = 027)
NJ_029 <- blocks(state = 34, county = 029)
NJ_031 <- blocks(state = 34, county = 031)
NJ_035 <- blocks(state = 34, county = 035)
NJ_037 <- blocks(state = 34, county = 037)
NJ_039 <- blocks(state = 34, county = 039)
NJ_041 <- blocks(state = 34, county = 041)
NJ_All <- rbind(NJ_003, NJ_013, NJ_017, NJ_019, NJ_021, NJ_023, NJ_025, 
                NJ_027, NJ_029, NJ_031, NJ_035, NJ_037, NJ_039, NJ_041)

# New York
NY_005 <- blocks(state = "NY", county = 005)
NY_027 <- blocks(state = 36, county = 027)
NY_047 <- blocks(state = 36, county = 047)
NY_059 <- blocks(state = 36, county = 059)
NY_061 <- blocks(state = 36, county = 061)
NY_071 <- blocks(state = 36, county = 071)
NY_079 <- blocks(state = 36, county = 079)
NY_081 <- blocks(state = 36, county = 081)
NY_085 <- blocks(state = 36, county = 085)
NY_087 <- blocks(state = 36, county = 087)
NY_103 <- blocks(state = 36, county = 103)
NY_105 <- blocks(state = 36, county = 105)
NY_111 <- blocks(state = 36, county = 111)
NY_119 <- blocks(state = 36, county = 119)
NY_All <- rbind(NY_005, NY_027, NY_047, NY_059, NY_061, NY_071, NY_079,
                NY_081, NY_085, NY_087, NY_103, NY_105, NY_111, NY_119)

RPA_Blocks <- rbind(CT_All, NJ_All, NY_All)

st_write(RPA_Blocks,
         "G:/Shared drives/Projects/5035_Tech Equity/Geographies/RPA_Blocks_shp.shp", 
         driver = "ESRI Shapefile")


#################################################################################


# Make FCC Tri State data
# Import goedatasets
FCC_NY_20 <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/NY-Fixed-Jun2020-v1.csv')
FCC_NJ_20 <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/NJ-Fixed-Jun2020-v1.csv')
FCC_CT_20 <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/CT-Fixed-Jun2020-v1.csv')

FCC_NY_19 <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/NY-Fixed-Jun2019-v2.csv')
FCC_NJ_19 <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/NJ-Fixed-Jun2019-v2.csv')
FCC_CT_19 <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/CT-Fixed-Jun2019-v2.csv')

FCC_NY_18 <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/NY-Fixed-Jun2018-v1.csv')
FCC_NJ_18 <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/NJ-Fixed-Jun2018-v1.csv')
FCC_CT_18 <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/CT-Fixed-Jun2018-v1.csv')

FCC_NY_17 <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/NY-Fixed-Jun2017-v3.csv')
FCC_NJ_17 <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/NJ-Fixed-Jun2017-v3.csv')
FCC_CT_17 <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/CT-Fixed-Jun2017-v3.csv')

FCC_NY_16 <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/NY-Fixed-Jun2016-v4.csv')
FCC_NJ_16 <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/NJ-Fixed-Jun2016-v4.csv')
FCC_CT_16 <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/CT-Fixed-Jun2016-v4.csv')

FCC_NY_15 <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/NY-Fixed-Jun2015-v5.csv')
FCC_NJ_15 <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/NJ-Fixed-Jun2015-v5.csv')
FCC_CT_15 <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/CT-Fixed-Jun2015-v5.csv')

# Rbind fcc data in three states
FCC_TRI_20 <- rbind(FCC_NY_20, FCC_NJ_20, FCC_CT_20)
FCC_TRI_19 <- rbind(FCC_NY_19, FCC_NJ_19, FCC_CT_19)
FCC_TRI_18 <- rbind(FCC_NY_18, FCC_NJ_18, FCC_CT_18)
FCC_TRI_17 <- rbind(FCC_NY_17, FCC_NJ_17, FCC_CT_17)
FCC_TRI_16 <- rbind(FCC_NY_16, FCC_NJ_16, FCC_CT_16)
FCC_TRI_15 <- rbind(FCC_NY_15, FCC_NJ_15, FCC_CT_15)

# WRite the CSV file
write.csv(FCC_TRI_20, file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/Tri-2020.csv')
write.csv(FCC_TRI_19, file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/Tri-2019.csv')
write.csv(FCC_TRI_18, file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/Tri-2018.csv')
write.csv(FCC_TRI_17, file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/Tri-2017.csv')
write.csv(FCC_TRI_16, file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/Tri-2016.csv')
write.csv(FCC_TRI_15, file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/Tri-2015.csv')


#################################################################################


FCC_TRI_20_sub <- FCC_TRI_20 %>%
  dplyr::rename(
    TecCode20 = "TechCode",
    Consumr20 = "Consumer",
    MaxAdDn20 = "MaxAdDown",
    MaxAdUp20 = "MaxAdUp",
    Busines20 = "Business"
  ) %>%
  select(BlockCode:Busines20) %>%
  mutate(
    GEOIDBlck = as.character(BlockCode)
  ) %>%
  select(TecCode20:GEOIDBlck) %>%
  relocate(GEOIDBlck)

FCC_TRI_19_sub <- FCC_TRI_19 %>%
  dplyr::rename(
    TecCode19 = "TechCode",
    Consumr19 = "Consumer",
    MaxAdDn19 = "MaxAdDown",
    MaxAdUp19 = "MaxAdUp",
    Busines19 = "Business",
    MxCIRDn19 = "MaxCIRDown",
    MxCIRUp19 = "MaxCIRUp"
  ) %>%
  select(BlockCode:MxCIRUp19) %>%
  mutate(
    GEOIDBlck = as.character(BlockCode)
  ) %>%
  select(TecCode19:GEOIDBlck) %>%
  relocate(GEOIDBlck)

FCC_TRI_18_sub <- FCC_TRI_18 %>%
  dplyr::rename(
    TecCode18 = "TechCode",
    Consumr18 = "Consumer",
    MaxAdDn18 = "MaxAdDown",
    MaxAdUp18 = "MaxAdUp",
    Busines18 = "Business",
    MxCIRDn18 = "MaxCIRDown",
    MxCIRUp18 = "MaxCIRUp"
  ) %>%
  select(BlockCode:MxCIRUp18) %>%
  mutate(
    GEOIDBlck = as.character(BlockCode)
  ) %>%
  select(TecCode18:GEOIDBlck) %>%
  relocate(GEOIDBlck)

FCC_TRI_17_sub <- FCC_TRI_17 %>%
  dplyr::rename(
    TecCode17 = "TechCode",
    Consumr17 = "Consumer",
    MaxAdDn17 = "MaxAdDown",
    MaxAdUp17 = "MaxAdUp",
    Busines17 = "Business",
    MxCIRDn17 = "MaxCIRDown",
    MxCIRUp17 = "MaxCIRUp"
  ) %>%
  select(BlockCode:MxCIRUp17) %>%
  mutate(
    GEOIDBlck = as.character(BlockCode)
  ) %>%
  select(TecCode17:GEOIDBlck) %>%
  relocate(GEOIDBlck)

FCC_TRI_16_sub <- FCC_TRI_16 %>%
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
  mutate(
    GEOIDBlck = as.character(BlockCode)
  ) %>%
  select(TecCode16:GEOIDBlck) %>%
  relocate(GEOIDBlck)

FCC_TRI_15_sub <- FCC_TRI_15 %>%
  dplyr::rename(
    TecCode15 = "TechCode",
    Consumr15 = "Consumer",
    MaxAdDn15 = "MaxAdDown",
    MaxAdUp15 = "MaxAdUp",
    Busines15 = "Business",
    MxCIRDn15 = "MaxCIRDown",
    MxCIRUp15 = "MaxCIRUp"
  ) %>%
  select(BlockCode:MxCIRUp15) %>%
  mutate(
    GEOIDBlck = as.character(BlockCode)
  ) %>%
  select(TecCode15:GEOIDBlck) %>%
  relocate(GEOIDBlck)

head(FCC_TRI_15_sub)
head(FCC_TRI_16_sub)
head(FCC_TRI_17_sub)
head(FCC_TRI_18_sub)
head(FCC_TRI_19_sub)
head(FCC_TRI_20_sub)














