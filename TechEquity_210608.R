

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

# 1. Trim down the 100 Mbps data and make the column of 100 Mbps
FCC_DPL20_TRI_MAX <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/FCC_DPL20_TRI_MAX.csv')
FCC_DPL16_TRI_MAX <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/FCC_DPL16_TRI_MAX.csv')

View(FCC_DPL20_TRI_MAX)
View(FCC_DPL16_TRI_MAX)
colnames(FCC_DPL20_TRI_MAX)
colnames(FCC_DPL16_TRI_MAX)

FCC_DPL20_TRI_MAX$GEOID10 = stri_pad_left(FCC_DPL20_TRI_MAX$GEOID10, 15, "0")
FCC_DPL16_TRI_MAX$GEOID10 = stri_pad_left(FCC_DPL16_TRI_MAX$GEOID10, 15, "0")

FCC_DPL20_TRI_MAX_SUB <-
  FCC_DPL20_TRI_MAX %>%
  select(GEOID10, TecCode20, MaxAdDn20) %>%
  mutate(
    MaxAdDn20_100OV = case_when(
      MaxAdDn20 >= 100 ~ 1,
      MaxAdDn20 < 100 ~ 0
    ))

FCC_DPL16_TRI_MAX_SUB <-
  FCC_DPL16_TRI_MAX %>%
  select(GEOID10, TecCode16, MaxAdDn16) %>%
  mutate(
    MaxAdDn16_100OV = case_when(
      MaxAdDn16 >= 100 ~ 1,
      MaxAdDn16 < 100 ~ 0
    ))

# 2. Download Census block data
census_api_key("5e8ebd0bdf69ccee12f6a6be2cdc51acae7b2339", overwrite = TRUE, install = TRUE)
?load_variables

sf1_variable_list.10 <- load_variables(2010, 
                                       "sf1", 
                                       cache = TRUE)
  # P001001     Total Population

sf1_10_ny <- get_decennial(geography = "block", 
                     variables = c("P001001" # Total Population
                                   ),
                     state = "NY", county = c("005", "027", "047", "059", "061",
                                              "071", "079", "081", "085", "087",
                                              "103", "105", "111", "119"),
                     survey = "sf1", year = 2010)

sf1_10_nj <- get_decennial(geography = "block",
                           variables = c("P001001" # Total Population
                                         ),
                           state = "NJ", county = c("003", "013", "017", "019", "021",
                                                    "023", "025", "027", "029", "031", 
                                                    "035", "037", "039", "041"),
                           survey = "sf1", year = 2010)

sf1_10_ct <- get_decennial(geography = "block",
                           variables = c("P001001" # Total Population
                           ),
                           state = "CT", county = c("001", "005", "009"),
                           survey = "sf1", year = 2010)

  # rbind of three states
SF1_10_TRI <- rbind(sf1_10_ny, sf1_10_nj, sf1_10_ct)

  # Trim the data
SF1_10_TRI_POP <-
  SF1_10_TRI %>% 
  cast(GEOID~variable) %>%
  dplyr::rename(
    GEOID10 = "GEOID",
    SF1_POP10 = "P001001"
  ) 

  # Read census block csv reference
geo_Blocks_as_sp <- as_Spatial(geo_Blocks)
write.csv(geo_Blocks_as_sp@data, file = 'G:/Shared drives/Projects/5035_Tech Equity/Geographies/RPA_Blocks_ref.csv')
csv_Blocks <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Geographies/RPA_Blocks_ref.csv')
csv_Blocks$GEOID10 = stri_pad_left(csv_Blocks$GEOID10, 15, "0")
View(csv_Blocks)

csvSF1_POP10 <- merge(csv_Blocks, SF1_10_TRI_POP, by = "GEOID10", all.x = TRUE)
colnames(csvSF1_POP10)

  # Make Census Tract Column
csvSF1_POP10_2 <- 
csvSF1_POP10 %>%
  select(GEOID10, SF1_POP10) %>%
  mutate(
    TRACTID = GEOID10
  ) %>%
  relocate(TRACTID)

  # Delete the last four characters to make census tract ID
csvSF1_POP10_2$TRACTID <- gsub(".{4}$", "", csvSF1_POP10_2$TRACTID)


# 3. Merge the census block population data with Max Mbps data
View(csvSF1_POP10_2)
View(FCC_DPL20_TRI_MAX_SUB)
View(FCC_DPL16_TRI_MAX_SUB)

csvFCC_POP10_100OV = list (csvSF1_POP10_2, FCC_DPL20_TRI_MAX_SUB, FCC_DPL16_TRI_MAX_SUB) %>%
  reduce(left_join, by = "GEOID10", all.x = TRUE)
View(csvFCC_POP10_100OV)

# 4. Aggregate the Population Data to census tract level
colnames(csvFCC_POP10_100OV)

# SUM TOTAL
csvFCC_POP10_SUM1 <-
csvFCC_POP10_100OV %>%
  group_by(TRACTID) %>%
  summarise(sum_POP10 = sum(SF1_POP10))

# 2020 OVER 100MBPS
csvFCC_POP10_SUM2 <-
csvFCC_POP10_100OV %>%
  filter(MaxAdDn20_100OV == 1) %>%
  group_by(TRACTID) %>%
  summarise(sum_POP10_20 = sum(SF1_POP10))

# 2016 OVER 100MPBS
csvFCC_POP10_SUM3 <-
csvFCC_POP10_100OV %>%
  filter(MaxAdDn16_100OV == 1) %>%
  group_by(TRACTID) %>%
  summarise(sum_POP10_16 = sum(SF1_POP10))

csvFCC_POP10_SUM_TTL = list (csvFCC_POP10_SUM1, csvFCC_POP10_SUM2, csvFCC_POP10_SUM3) %>%
  reduce(left_join, by = "TRACTID", all.x = TRUE)

View(csvFCC_POP10_SUM_TTL)
colnames(csvFCC_POP10_SUM_TTL)

csvFCC_POP2016_100OV <-
csvFCC_POP10_SUM_TTL %>%
  dplyr::rename(
    GEOID = "TRACTID",
    POP_TTL = "sum_POP10",
    POP20_100OV = "sum_POP10_20",
    POP16_100OV = "sum_POP10_16"
  ) %>%
  mutate( 
    PCT20_100OV = (POP20_100OV/POP_TTL)*100, 
    PCT16_100OV = (POP16_100OV/POP_TTL)*100
  ) %>%
  mutate_at(vars(PCT20_100OV, PCT16_100OV), 
            funs(round(., 1)))

# Merge with Census Tracts
geoFCC_POP2016_100OV <- merge(geo_Tracts, csvFCC_POP2016_100OV, by = "GEOID", all.x = TRUE)

st_write(geoFCC_POP2016_100OV,
         "G:/Shared drives/Projects/5035_Tech Equity/Data/A_Shapefile/Broadband_Data/FCC/FCC_POP2016_100OV.shp", 
         driver = "ESRI Shapefile")

#################################################################################

# Step 3. Census Broadband Map - ACS 2016 & 2019


# Needs to be Changed to ACS1 B28002_001 & B28002_004

census_api_key("5e8ebd0bdf69ccee12f6a6be2cdc51acae7b2339", overwrite = TRUE, install = TRUE)

# ACS 5 YEAR ESTIMATES -  2019, 2017, 2016
# but, 5 year Estimates in 2016 does not include B28002 broadband data

acs_variable_list.5.19 <- load_variables(2019, 
                                         "acs5", 
                                         cache = TRUE)
acs_variable_list.5.17 <- load_variables(2017, 
                                         "acs5", 
                                         cache = TRUE)
acs_variable_list.5.16 <- load_variables(2016, 
                                         "acs5", 
                                         cache = TRUE)

# ACS 1 YEAR ESTIMATES -  2019, 2016 
# But, 1 Year Estimates only include geographic areas with over 65,000 population
acs_variable_list.1.19 <- load_variables(2019, 
                                         "acs1", 
                                         cache = TRUE)
acs_variable_list.1.16 <- load_variables(2016, 
                                         "acs1", 
                                         cache = TRUE)


# 2019 - FL
acs19_5yr_NY <- get_acs(geography = "tract", 
                        variables = c("B28002_001",   # Total Population
                                      "B28002_004"    # Broadband of any type
                        ),
                        state = "NY", county = c("005", "027", "047", "059", "061", 
                                                 "071", "079", "081", "085", "087", 
                                                 "103", "105", "111", "119"),
                        survey = "acs5", year = 2019)

acs19_5yr_NJ <- get_acs(geography = "tract", 
                        variables = c("B28002_001",   # Total Population
                                      "B28002_004"    # Broadband of any type
                        ),
                        state = "NJ", county = c("003", "013", "017", "019", "021", 
                                                 "023", "025", "027", "029", "031", 
                                                 "035", "037", "039", "041"),
                        survey = "acs5", year = 2019)

acs19_5yr_CT <- get_acs(geography = "tract", 
                        variables = c("B28002_001",   # Total Population
                                      "B28002_004"    # Broadband of any type
                        ),
                        state = "CT", county = c("001", "005", "009"),
                        survey = "acs5", year = 2019)

ACS19_5YR_NY <- 
acs19_5yr_NY %>%
  select(GEOID, variable, estimate) %>%
  cast(GEOID ~ variable) %>%
  dplyr::rename(
    POP19_TTL = "B28002_001",
    POP19_BRDBD = "B28002_004"
  )

ACS19_5YR_NJ <-
acs19_5yr_NJ %>%
  select(GEOID, variable, estimate) %>%
  cast(GEOID ~ variable) %>%
  dplyr::rename(
    POP19_TTL = "B28002_001",
    POP19_BRDBD = "B28002_004"
  )

ACS19_5YR_CT <-
acs19_5yr_CT %>%
  select(GEOID, variable, estimate) %>%
  cast(GEOID ~ variable) %>%
  dplyr::rename(
    POP19_TTL = "B28002_001",
    POP19_BRDBD = "B28002_004"
  ) 

ACS19_BRDBD_TRI <- rbind(ACS19_5YR_NY, ACS19_5YR_NJ, ACS19_5YR_CT)

ACS19_BRDBD_PCT <-
ACS19_BRDBD_TRI %>%
  mutate(
    PCT19_BRDBD = (POP19_BRDBD/POP19_TTL)*100
  ) %>%
  mutate_at(vars(PCT19_BRDBD), funs(round(., 1))
  ) 


# 2017 - NY, NJ, CT
acs17_5yr_NY <- get_acs(geography = "tract", 
                        variables = c("B28002_001",   # Total Population
                                      "B28002_004"    # Broadband of any type
                        ),
                        state = "NY", county = c("005", "027", "047", "059", "061", 
                                                 "071", "079", "081", "085", "087", 
                                                 "103", "105", "111", "119"),
                        survey = "acs5", year = 2017)

acs17_5yr_NJ <- get_acs(geography = "tract", 
                        variables = c("B28002_001",   # Total Population
                                      "B28002_004"    # Broadband of any type
                        ),
                        state = "NJ", county = c("003", "013", "017", "019", "021", 
                                                 "023", "025", "027", "029", "031", 
                                                 "035", "037", "039", "041"),
                        survey = "acs5", year = 2017)

acs17_5yr_CT <- get_acs(geography = "tract", 
                        variables = c("B28002_001",   # Total Population
                                      "B28002_004"    # Broadband of any type
                        ),
                        state = "CT", county = c("001", "005", "009"),
                        survey = "acs5", year = 2017)

ACS17_5YR_NY <- 
  acs17_5yr_NY %>%
  select(GEOID, variable, estimate) %>%
  cast(GEOID ~ variable) %>%
  dplyr::rename(
    POP17_TTL = "B28002_001",
    POP17_BRDBD = "B28002_004"
  )

ACS17_5YR_NJ <-
  acs17_5yr_NJ %>%
  select(GEOID, variable, estimate) %>%
  cast(GEOID ~ variable) %>%
  dplyr::rename(
    POP17_TTL = "B28002_001",
    POP17_BRDBD = "B28002_004"
  )

ACS17_5YR_CT <-
  acs17_5yr_CT %>%
  select(GEOID, variable, estimate) %>%
  cast(GEOID ~ variable) %>%
  dplyr::rename(
    POP17_TTL = "B28002_001",
    POP17_BRDBD = "B28002_004"
  ) 

ACS17_BRDBD_TRI <- rbind(ACS17_5YR_NY, ACS17_5YR_NJ, ACS17_5YR_CT)

ACS17_BRDBD_PCT <-
  ACS17_BRDBD_TRI %>%
  mutate(
    PCT17_BRDBD = (POP17_BRDBD/POP17_TTL)*100
  ) %>%
  mutate_at(vars(PCT17_BRDBD), funs(round(., 1))
  ) 

# CSV FILE FOR BROADBAND % IN 2017, 2019
ACS1917_BRDBD_PCT <- merge(ACS19_BRDBD_PCT, ACS17_BRDBD_PCT, by = "GEOID", all.x = TRUE)
write.csv(ACS1917_BRDBD_PCT, file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/csvACS_BRDBD1917_PCT.csv')
ACS1917_BRDBD_PCT <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/FCC/csvACS_BRDBD1917_PCT.csv')

# Merge with Census tract file and save the layer
geoACS_BRDBD1917_Tract <- merge(geo_Tracts, ACS1917_BRDBD_PCT, by = "GEOID", all.x = TRUE)
st_write(geoACS_BRDBD1917_Tract,
         "G:/Shared drives/Projects/5035_Tech Equity/Data/A_Shapefile/Broadband_Data/FCC/ACS1917_BRDPCT.shp", 
         driver = "ESRI Shapefile")
geoACS_BRDBD1917_Tract = st_read("G:/Shared drives/Projects/5035_Tech Equity/Data/A_Shapefile/Broadband_Data/FCC/ACS1917_BRDPCT.shp")


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



