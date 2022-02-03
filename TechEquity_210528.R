
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

# vignette("value-labels", package = "ipumsr")
# vignette("ipums-geography", package = "ipumsr")
# vignette("ipums-nhgis", package = "ipumsr")
# vignette("ipums-terra", package = "ipumsr")

# Set file path
setwd("G:/Shared drives/Projects/5035_Tech Equity/Data")

#################################################################################

# Importing FCC data

# Importing goedatasets
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

# Read the Combined FCC csv
FCC_TRI_20 <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/Tri-2020.csv')
FCC_TRI_19 <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/Tri-2019.csv')
FCC_TRI_18 <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/Tri-2018.csv')
FCC_TRI_17 <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/Tri-2017.csv')
FCC_TRI_16 <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/Tri-2016.csv')
FCC_TRI_15 <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/Tri-2015.csv')

View(FCC_TRI_15)
View(FCC_TRI_16)
colnames(FCC_TRI_15)
colnames(FCC_TRI_20)

# Cbind FCC data by years
# Rename
colnames(FCC_TRI_20)
colnames(FCC_TRI_19)
colnames(FCC_TRI_18)
colnames(FCC_TRI_17)
colnames(FCC_TRI_16)
colnames(FCC_TRI_15)

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

# Read Geography of Census Block
geo_Blocks = st_read("G:/Shared drives/Projects/5035_Tech Equity/Geographies/Census_Block/Tri_Block_Trimmed1.shp")
colnames(geo_Blocks)

geoFCC20_Blocks_inner <- merge(x = geo_Blocks, y = FCC_TRI_20_sub, by.x = "GEOID", by.y = "GEOIDBlck", all.x = FALSE, all.y = FALSE)
geoFCC19_Blocks_inner <- merge(x = geo_Blocks, y = FCC_TRI_19_sub, by.x = "GEOID", by.y = "GEOIDBlck", all.x = FALSE, all.y = FALSE)
geoFCC18_Blocks_inner <- merge(x = geo_Blocks, y = FCC_TRI_18_sub, by.x = "GEOID", by.y = "GEOIDBlck", all.x = FALSE, all.y = FALSE)
geoFCC17_Blocks_inner <- merge(x = geo_Blocks, y = FCC_TRI_17_sub, by.x = "GEOID", by.y = "GEOIDBlck", all.x = FALSE, all.y = FALSE)
geoFCC16_Blocks_inner <- merge(x = geo_Blocks, y = FCC_TRI_16_sub, by.x = "GEOID", by.y = "GEOIDBlck", all.x = FALSE, all.y = FALSE)
geoFCC15_Blocks_inner <- merge(x = geo_Blocks, y = FCC_TRI_15_sub, by.x = "GEOID", by.y = "GEOIDBlck", all.x = FALSE, all.y = FALSE)

st_write(geoFCC20_Blocks_inner,
         "G:/Shared drives/Projects/5035_Tech Equity/Data/A_Shapefile/Broadband_Data/FCC/FCC20_Blocks_shp.shp", 
         driver = "ESRI Shapefile")

st_write(geoFCC19_Blocks_inner,
         "G:/Shared drives/Projects/5035_Tech Equity/Data/A_Shapefile/Broadband_Data/FCC/FCC19_Blocks_shp.shp", 
         driver = "ESRI Shapefile")

st_write(geoFCC18_Blocks_inner,
         "G:/Shared drives/Projects/5035_Tech Equity/Data/A_Shapefile/Broadband_Data/FCC/FCC18_Blocks_shp.shp", 
         driver = "ESRI Shapefile")

st_write(geoFCC17_Blocks_inner,
         "G:/Shared drives/Projects/5035_Tech Equity/Data/A_Shapefile/Broadband_Data/FCC/FCC17_Blocks_shp.shp", 
         driver = "ESRI Shapefile")

st_write(geoFCC16_Blocks_inner,
         "G:/Shared drives/Projects/5035_Tech Equity/Data/A_Shapefile/Broadband_Data/FCC/FCC16_Blocks_shp.shp", 
         driver = "ESRI Shapefile")

st_write(geoFCC15_Blocks_inner,
         "G:/Shared drives/Projects/5035_Tech Equity/Data/A_Shapefile/Broadband_Data/FCC/FCC15_Blocks_shp.shp", 
         driver = "ESRI Shapefile")



# geo_Blocks - 243,883
# FCC_TRI_20_sub - 3.983.073
# geoFCC2-_Blocks - 1,265,076


la = readOGR(".","la")












