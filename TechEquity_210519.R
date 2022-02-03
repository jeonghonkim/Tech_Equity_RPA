
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

# Importing goedatasets
csv_Counties <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/data_Counties_csv_csv.csv')
csv_PUMAs <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/data_PUMAs_csv.csv')

csv_Counties$FIPS = stri_pad_left(csv_Counties$FIPS, 5, "0")
csv_PUMAs$PUMACE10 = stri_pad_left(csv_PUMAs$PUMACE10, 5, "0")

view(csv_Counties)
head(csv_PUMAs)


#################################################################################

# 1. Combine Finished Data
data_Counties <- list (data5_age_Counties, data5_age_cinethh_pct, data5_age_cismrtphn_pct, data5_age_cidatapln_pct,
      data5_age_cihispeed_pct, data4_race_hisp_pct, data4_educ_pct, data4_emps_pct, data4_labf_pct,
      data4_hhic_med, data4_ownc_rent_med_Counties) %>%
  reduce(left_join, by="CountyFIPS")
View(data_Counties) # 48

data_PUMAs <- list(data5_age_PUMAs, data5_age_cinethh_pct2, data5_age_cismrtphn_pct2, data5_age_cidatapln_pct2, 
                   data5_age_cihispeed_pct2, data4_race_hisp_pct2, data4_educ_pct2, data4_emps_pct2, 
                   data4_labf_pct2, data4_hhic_med2, data4_ownc_rent_med_PUMAs) %>%
  reduce(left_join, by="PUMACE10")
view(data_PUMAs) # 199

write.csv(data_Counties, file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/CSV/IPUMS_Counties_Untrimmed_csv.csv')
write.csv(data_PUMAs, file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/CSV/IPUMS_PUMAs_Untrimmed_csv.csv')

# 2. import shape files & csv files of GEOGRAPHIES 
geo_Counties = st_read("G:/Shared drives/Projects/5035_Tech Equity/Geographies/RPA_counties_shp.shp")
geo_PUMAs = st_read("G:/Shared drives/Projects/5035_Tech Equity/Geographies/RPA_PUMAs_shp.shp")
geo_Tracts = st_read("G:/Shared drives/Projects/5035_Tech Equity/Geographies/RPA_tracts_shp.shp")

data_Counties <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/CSV/IPUMS_Counties_Untrimmed_csv.csv')
data_PUMAs <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/CSV/IPUMS_PUMAs_Untrimmed_csv.csv')

csv_Counties <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Geographies/RPA_counties_ref.csv')
csv_PUMAs <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Geographies/RPA_PUMAs_ref.csv')

# 3. Merge csv file & data file to export combined data
head(csv_Counties)
head(data_Counties)
head(csv_PUMAs)
head(data_PUMAs)

csv_data_Counties <- list (csv_Counties, data_Counties) %>%
  reduce(left_join, by="FIPS")
csv_data_PUMAs <- list (csv_PUMAs, data_PUMAs) %>%
  reduce(left_join, by="PUMACE10")

csv_data_Counties$FIPS = stri_pad_left(csv_data_Counties$FIPS, 5, "0")
csv_data_PUMAs$PUMACE10 = stri_pad_left(csv_data_PUMAs$PUMACE10, 5, "0")

head(csv_data_Counties)
head(csv_data_PUMAs)

csv_data_Counties <- csv_data_Counties %>%
  select(
    OBJECTID:ACRES, AGE_MED:OWNC_RENT_MED
  )

csv_data_PUMAs <- csv_data_PUMAs %>%
  select(
    OBJECTID:Shape_Area, AGE_MED:OWNC_RENT_MED
  )

write.csv(csv_data_Counties, file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/CSV/IPUMS_Counties_Trimmed_csv.csv')
write.csv(csv_data_PUMAs, file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/CSV/IPUMS_PUMAs_Trimmed_csv.csv')

csv_Counties <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/CSV/IPUMS_Counties_Trimmed_csv.csv')
csv_PUMAs <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/CSV/IPUMS_PUMAs_Trimmed_csv.csv')

# 4. Merge Geographies with Finished Data
head(geo_Counties)
head(csv_Counties)
head(geo_PUMAs)
head(csv_PUMAs)
csv_PUMAs$PUMACE10 = stri_pad_left(csv_PUMAs$PUMACE10, 5, "0")
SpatialPolygons(csv_data_Counties)

geoData_Counties <- merge(geo_Counties, csv_Counties, by.x = "FIPS", by.y = "FIPS") # 31
geoData_PUMAs <- merge(geo_PUMAs, csv_PUMAs, by.x = "PUMACE10", by.y = "PUMACE10") # 170
geoData_Tracts <- merge(geo_Tracts, data_Tracts, by.x = "GEOID", by.y = "GEOID") # geo_Tracts: 5296 vs data_Tracts: 5305

head(geoData_Counties)
head(geoData_PUMAs)
head(geo_Tracts)
head(data_Tracts)

geoData_Counties <- geoData_Counties %>%
  select(
    FIPS:ACRES.x, AGE_MED:geometry
  ) %>%
  dplyr::rename(
    OBJECTID = "OBJECTID.x",
    ALAND = "ALAND.x",
    AWATER = "AWATER.x",
    INTPTLAT = "INTPTLAT.x",
    INTPTLON = "INTPTLON.x",
    CountyFIPS = "CountyFIPS.x",
    Counties = "Counties.x",
    State = "State.x",
    Subregion = "Subregion.x",
    SubRgnCode = "SubRgnCode.x",
    Shape_Length = "Shape_Leng",
    Shape_Area = "Shape_Area.x",
    area2 = "area2.x",
    ACRES = "ACRES.x",
  )

geoData_PUMAs <- geoData_PUMAs %>%
  select(
    PUMACE10:Shape_Area.x, AGE_MED:geometry
  ) %>%
  dplyr::rename(
    OBJECTID = "OBJECTID.x",
    STATEFP10 = "STATEFP10.x",
    GEOID10 = "GEOID10.x",
    NAMELSAD10 = "NAMELSAD10.x",
    MTFCC10 = "MTFCC10.x",
    FUNCSTAT10 = "FUNCSTAT10.x",
    ALAND10 = "ALAND10.x",
    AWATER10 = "AWATER10.x",
    INTPTLAT10 = "INTPTLAT10.x",
    INTPTLON10 = "INTPTLON10.x",
    Shape_Length = "Shape_Leng",
    Shape_Area = "Shape_Area.x"
  )

st_write(geoData_Counties,
         "G:/Shared drives/Projects/5035_Tech Equity/Data/New_Shapefile/IPUMS_Counties_shp.shp", 
         driver = "ESRI Shapefile")
st_write(geoData_PUMAs,
         "G:/Shared drive/Projects/5035_Tech Equity/Data/New_Shapefile/IPUMS_PUMAs_shp.shp", 
         driver = "ESRI Shapefile")

geoData_Counties = st_read("G:/Shared drives/Projects/5035_Tech Equity/Data/Shapefile/IPUMS_Counties_shp.shp")
geoData_PUMAs = st_read("G:/Shared drives/Projects/5035_Tech Equity/Data/Shapefile/IPUMS_PUMAs_shp.shp")



# 05/05/2021
# ACS
# 1. GETTING DATA
# Census API
census_api_key("5e8ebd0bdf69ccee12f6a6be2cdc51acae7b2339", overwrite = TRUE, install = TRUE)

# Get variables
acs_variable_list.5.19 <- load_variables(2019, 
                                         "acs5", 
                                         cache = TRUE)
acs_variable_list.1.19 <- load_variables(2019, 
                                         "acs1", 
                                         cache = TRUE)
view(acs_variable_list.5.19)
view(acs_variable_list.1.19)

# Target Variables
# 1. Access to Internet
# 2. Access to Smart Phone
# 3. Access to Phone with Data Plan
# 4. Access to Broadband

# VARIABLES IN ACS-5
# B25043  TENURE BY TELEPHONE SERVICE AVAILABLE BY AGE OF HOUSEHOLDER
# B28001  TYPES OF COMPUTERS IN HOUSEHOLD
# B28002  PRESENCE AND TYPES OF INTERNET SUBSCRIPTIONS IN HOUSEHOLD
# B28003  PRESENCE OF A COMPUTER AND TYPE OF INTERNET SUBSCRIPTION IN HOUSEHOLD
# B28004  HOUSEHOLD INCOME IN THE LAST 12 MONTHS (IN 2019 INFLATION-ADJUSTED DOLLARS) BY PRESENCE AND TYPE OF INTERNET SUBSCRIPTION IN HOUSEHOLD
# B28005  AGE BY PRESENCE OF A COMPUTER AND TYPES OF INTERNET SUBSCRIPTION IN HOUSEHOLD
# B28006  EDUCATIONAL ATTAINMENT BY PRESENCE OF A COMPUTER AND TYPES OF INTERNET SUBSCRIPTION IN HOUSEHOLD
# B28007  LABOR FORCE STATUS BY PRESENCE OF A COMPUTER AND TYPES OF INTERNET SUBSCRIPTION IN HOUSEHOLD
# B28008  PRESENCE OF A COMPUTER AND TYPE OF INTERNET SUBSCRIPTION IN HOUSEHOLD
# B28009  PRESENCE OF A COMPUTER AND TYPE OF INTERNET SUBSCRIPTION IN HOUSEHOLD | RACE
# B28010  COMPUTERS IN HOUSEHOLD
# B28011  INTERNET SUBSCRIPTIONS IN HOUSEHOLD


# 1) Tech Variables - ALL
# A. NY
acs19_5yr_ny <- get_acs(geography = "tract", 
                      variables = c("B28001_001", # TYPES OF COMPUTERS - Total
                                    "B28001_005", # TYPES OF COMPUTERS - Smart phone
                                    "B28002_001", # PRESENCE AND TYPES OF INTERNET - Total
                                    "B28002_002", # PRESENCE AND TYPES OF INTERNET - INT O, Subscription O
                                    "B28002_005", # PRESENCE AND TYPES OF INTERNET - INT O, Subscription O: Cellular Data Plan
                                    "B28002_007", # PRESENCE AND TYPES OF INTERNET - INT O, Subscription O: Broadband
                                    "B28002_012", # PRESENCE AND TYPES OF INTERNET - INT O, Subscription x
                                    "B01002_001", # MEDIAN AGE - Total
                                    #"B02001_001", # RACE - Total
                                    #"B02001_002", # RACE - White
                                    #"B02001_003", # RACE - Black or African-American
                                    #"B02001_004", # RACE - American Indian and Alaska Native
                                    #"B02001_005", # RACE - Asian
                                    #"B02001_006", # RACE - Native Hawaiian and Other Pacific Islander
                                    #"B02001_007", # RACE - Some other race alone
                                    #"B02001_008", # RACE - Two or more races
                                    "B03002_001", # RACE & HISP - TTL
                                    "B03002_002", # RACE & HISP - NOT - TTL
                                    "B03002_003", # RACE & HISP - NOT - WHT
                                    "B03002_004", # RACE & HISP - NOT - AFA
                                    "B03002_005", # RACE & HISP - NOT - AIN
                                    "B03002_006", # RACE & HISP - NOT - ASN
                                    "B03002_007", # RACE & HISP - NOT - HPI
                                    "B03002_008", # RACE & HISP - NOT - OT1
                                    "B03002_009", # RACE & HISP - NOT - OT2
                                    "B03002_012", # RACE & HISP - YES - TTL
                                    "B03002_013", # RACE & HISP - YES - WHT
                                    "B03002_014", # RACE & HISP - YES - AFA
                                    "B03002_015", # RACE & HISP - YES - AIN
                                    "B03002_016", # RACE & HISP - YES - ASN
                                    "B03002_017", # RACE & HISP - YES - HPI
                                    "B03002_018", # RACE & HISP - YES - OT1
                                    "B03002_019", # RACE & HISP - YES - OT2
                                    "B14001_001", # EDUCATION - Total
                                    "B14001_003", # EDUCATION - Nursery school, Preschool
                                    "B14001_004", # EDUCATION - Kindergarten
                                    "B14001_005", # EDUCATION - Grade 1 to Grade 4
                                    "B14001_006", # EDUCATION - Grade 5 to Grade 8
                                    "B14001_007", # EDUCATION - Grade 9 to Grade 12
                                    "B14001_008", # EDUCATION - College, Undergraduate years
                                    "B14001_009", # EDUCATION - Graduate or Professional school
                                    "B14001_010", # EDUCATION - Not enrolled in school
                                    "B23025_001", # LABOR FORCE - Total
                                    "B23025_002", # LABOR FORCE - In
                                    "B23025_007", # LABOR FORCE - Not
                                    "B23025_003", # EMPLOYMENT STATUS - Total
                                    "B23025_004", # EMPLOYMENT STATUS - Employed
                                    "B23025_005", # EMPLOYMENT STATUS - Unemployed
                                    "B25099_001", # Median Household Income - Total
                                    "B25088_001", # Median selected monthly owner costs
                                    "B25058_001", # Median Contract Rent
                                    "B25064_001", # Median Gross Rent
                                    "B17001_001", # Total
                                    "B17001_002" # Below Poverty Total
                                    ),
                      state = "NY", county = c("005", "027", "047", "059", "061", 
                                               "071", "079", "081", "085", "087", 
                                               "103", "105", "111", "119"),
                      survey = "acs5", year = 2019)
colnames(acs19_5yr_ny)

data_Tracts_ny <-
acs19_5yr_ny %>%
  select(GEOID:estimate) %>%
  cast(GEOID~variable) %>%
  dplyr::rename(
    SMRT_TTL = "B28001_001",
    SMRT_Y = "B28001_005",
    INT_TTL = "B28002_001",
    INT_Y_SBO_TTL = "B28002_002",
    INT_Y_SBO_CDP = "B28002_005",
    INT_Y_SBO_BRD = "B28002_007",
    INT_Y_SBX_TTL = "B28002_012",
    AGE_MED = "B01002_001", # MEDIAN AGE - Total ##########
    #RAC_TTL = "B02001_001", # RACE - Total
    #RAC_WHT = "B02001_002", # RACE - White
    #RAC_AFA = "B02001_003", # RACE - Black or African-American
    #RAC_AIN = "B02001_004", # RACE - American Indian and Alaska Native
    #RAC_ASN = "B02001_005", # RACE - Asian
    #RAC_HPI = "B02001_006", # RACE - Native Hawaiian and Other Pacific Islander
    #RAC_OT1 = "B02001_007", # RACE - Some other race alone
    #RAC_OT2 = "B02001_008", # RACE - Two or more races
    RAC_TTL = "B03002_001", # RACE & HISP - TTL
    RAC_NHS_TTL = "B03002_002", # RACE & HISP - NOT - TTL
    RAC_NHS_WHT = "B03002_003", # RACE & HISP - NOT - WHT
    RAC_NHS_AFA = "B03002_004", # RACE & HISP - NOT - AFA
    RAC_NHS_AIN = "B03002_005", # RACE & HISP - NOT - AIN
    RAC_NHS_ASN = "B03002_006", # RACE & HISP - NOT - ASN
    RAC_NHS_HPI = "B03002_007", # RACE & HISP - NOT - HPI
    RAC_NHS_OT1 = "B03002_008", # RACE & HISP - NOT - OT1
    RAC_NHS_OT2 = "B03002_009", # RACE & HISP - NOT - OT2
    RAC_YHS_TTL = "B03002_012", # RACE & HISP - YES - TTL
    RAC_YHS_WHT = "B03002_013", # RACE & HISP - YES - WHT
    RAC_YHS_AFA = "B03002_014", # RACE & HISP - YES - AFA
    RAC_YHS_AIN = "B03002_015", # RACE & HISP - YES - AIN
    RAC_YHS_ASN = "B03002_016", # RACE & HISP - YES - ASN
    RAC_YHS_HPI = "B03002_017", # RACE & HISP - YES - HPI
    RAC_YHS_OT1 = "B03002_018", # RACE & HISP - YES - OT1
    RAC_YHS_OT2 = "B03002_019", # RACE & HISP - YES - OT2
    EDU_TTL = "B14001_001", # EDUCATION - Total
    EDU_PRE = "B14001_003", # EDUCATION - Nursery school, Preschool
    EDU_KIN = "B14001_004", # EDUCATION - Kindergarten
    EDU_104 = "B14001_005", # EDUCATION - Grade 1 to Grade 4
    EDU_508 = "B14001_006", # EDUCATION - Grade 5 to Grade 8
    EDU_912 = "B14001_007", # EDUCATION - Grade 9 to Grade 12
    EDU_COL = "B14001_008", # EDUCATION - College, Undergraduate years
    EDU_GRA = "B14001_009", # EDUCATION - Graduate or Professional school
    EDU_NIN = "B14001_010", # EDUCATION - Not enrolled in school
    LAB_TTL = "B23025_001", # LABOR FORCE - Total
    LAB_YES = "B23025_002", # LABOR FORCE - In
    LAB_NOT = "B23025_007", # LABOR FORCE - Not
    EMP_TTL = "B23025_003", # EMPLOYMENT STATUS - Total
    EMP_YES = "B23025_004", # EMPLOYMENT STATUS - Employed
    EMP_NOT = "B23025_005", # EMPLOYMENT STATUS - Unemployed
    INC_MED = "B25099_001", # Median Household Income - Total ##########
    OWC_MED = "B25088_001", # Median selected monthly owner costs ##########
    CRT_MED = "B25058_001", # Median Contract Rent ##########
    GRT_MED = "B25064_001",  # Median Gross Rent ##########
    PVT_TTL = "B17001_001", # Poverty Total
    PVT_BLW_TTL = "B17001_002" # Below Poverty Total
  ) %>%
  mutate(
    RAC_NHS_OTH = RAC_NHS_HPI + RAC_NHS_OT1 + RAC_NHS_OT2,
    RAC_NHS_OTH2 = RAC_NHS_OTH + RAC_NHS_AIN,
    EDU_LV1 = EDU_PRE + EDU_KIN + EDU_104 + EDU_508 + EDU_NIN,
    EDU_LV2 = EDU_912,
    EDU_LV3 = EDU_COL,
    EDU_LV4 = EDU_GRA,
    INT_Y = INT_Y_SBO_TTL + INT_Y_SBX_TTL,
    TEC_INT_PCT = (INT_Y / INT_TTL) * 100,
    TEC_BRD_PCT = (INT_Y_SBO_BRD / INT_Y_SBO_TTL) * 100,
    TEC_SMR_PCT = (SMRT_Y / SMRT_TTL) * 100,
    TEC_CDP_PCT = (INT_Y_SBO_CDP / INT_Y_SBO_TTL) * 100,
    RAC_WHT_PCT = (RAC_NHS_WHT / RAC_TTL) * 100,
    RAC_AFA_PCT = (RAC_NHS_AFA / RAC_TTL) * 100,
    RAC_ASN_PCT = (RAC_NHS_ASN / RAC_TTL) * 100,
    RAC_HSP_PCT = (RAC_YHS_TTL / RAC_TTL) * 100,
    RAC_OTH_PCT = (RAC_NHS_OTH2 / RAC_TTL) * 100,
    EDU_LV1_PCT = (EDU_LV1 / EDU_TTL) * 100,
    EDU_LV2_PCT = (EDU_LV2 / EDU_TTL) * 100,
    EDU_LV3_PCT = (EDU_LV3 / EDU_TTL) * 100,
    EDU_LV4_PCT = (EDU_LV4 / EDU_TTL) * 100,
    LAB_YES_PCT = (LAB_YES / LAB_TTL) * 100,
    EMP_NOT_PCT = (EMP_NOT / EMP_TTL) * 100,
    PVT_BLW_PCT = (PVT_BLW_TTL / PVT_TTL) * 100
  ) %>%
  mutate_at(vars(TEC_INT_PCT, TEC_BRD_PCT, TEC_SMR_PCT, TEC_CDP_PCT, RAC_WHT_PCT, RAC_AFA_PCT, 
                 RAC_ASN_PCT, RAC_HSP_PCT, RAC_OTH_PCT, EDU_LV1_PCT, EDU_LV2_PCT, EDU_LV3_PCT, EDU_LV4_PCT,
                 LAB_YES_PCT, EMP_NOT_PCT, PVT_BLW_PCT), 
            funs(round(., 1))) %>%
  select(GEOID, TEC_INT_PCT:TEC_CDP_PCT, AGE_MED, RAC_NHS_WHT, RAC_NHS_AFA, RAC_NHS_ASN, RAC_YHS_TTL, RAC_NHS_OTH2,
         RAC_WHT_PCT:RAC_OTH_PCT, EDU_LV1_PCT:EDU_LV4_PCT, EMP_NOT_PCT, 
         LAB_YES_PCT, INC_MED, OWC_MED, CRT_MED, GRT_MED, PVT_BLW_PCT) %>%
  mutate(TEC_INT_PCT  = na_if(TEC_INT_PCT, 0.0),
         TEC_BRD_PCT  = na_if(TEC_BRD_PCT, 0.0),
         TEC_SMR_PCT  = na_if(TEC_SMR_PCT, 0.0),
         TEC_CDP_PCT  = na_if(TEC_CDP_PCT, 0.0))

replace_na(list(TEC_INT_PCT = 0, TEC_BRD_PCT = 0, TEC_SMR_PCT = 0, TEC_CDP_PCT = 0, EMP_NOT_PCT = 0)) %>%
replace_na(list(RAC_WHT_PCT = 0, RAC_AFA_PCT = 0, RAC_AIN_PCT = 0, RAC_ASN_PCT = 0,
                RAC_HSP_PCT = 0, RAC_OTH_PCT = 0))
head(data_Tracts_ny)



# 2) NJ
acs19_5yr_nj <- get_acs(geography = "tract", 
                             variables = c("B28001_001", # TYPES OF COMPUTERS - Total
                                           "B28001_005", # TYPES OF COMPUTERS - Smart phone
                                           "B28002_001", # PRESENCE AND TYPES OF INTERNET - Total
                                           "B28002_002", # PRESENCE AND TYPES OF INTERNET - INT O, Subscription O
                                           "B28002_005", # PRESENCE AND TYPES OF INTERNET - INT O, Subscription O: Cellular Data Plan
                                           "B28002_007", # PRESENCE AND TYPES OF INTERNET - INT O, Subscription O: Broadband
                                           "B28002_012", # PRESENCE AND TYPES OF INTERNET - INT O, Subscription x
                                           "B01002_001", # MEDIAN AGE - Total
                                           #"B02001_001", # RACE - Total
                                           #"B02001_002", # RACE - White
                                           #"B02001_003", # RACE - Black or African-American
                                           #"B02001_004", # RACE - American Indian and Alaska Native
                                           #"B02001_005", # RACE - Asian
                                           #"B02001_006", # RACE - Native Hawaiian and Other Pacific Islander
                                           #"B02001_007", # RACE - Some other race alone
                                           #"B02001_008", # RACE - Two or more races
                                           "B03002_001", # RACE & HISP - TTL
                                           "B03002_002", # RACE & HISP - NOT - TTL
                                           "B03002_003", # RACE & HISP - NOT - WHT
                                           "B03002_004", # RACE & HISP - NOT - AFA
                                           "B03002_005", # RACE & HISP - NOT - AIN
                                           "B03002_006", # RACE & HISP - NOT - ASN
                                           "B03002_007", # RACE & HISP - NOT - HPI
                                           "B03002_008", # RACE & HISP - NOT - OT1
                                           "B03002_009", # RACE & HISP - NOT - OT2
                                           "B03002_012", # RACE & HISP - YES - TTL
                                           "B03002_013", # RACE & HISP - YES - WHT
                                           "B03002_014", # RACE & HISP - YES - AFA
                                           "B03002_015", # RACE & HISP - YES - AIN
                                           "B03002_016", # RACE & HISP - YES - ASN
                                           "B03002_017", # RACE & HISP - YES - HPI
                                           "B03002_018", # RACE & HISP - YES - OT1
                                           "B03002_019", # RACE & HISP - YES - OT2
                                           "B14001_001", # EDUCATION - Total
                                           "B14001_003", # EDUCATION - Nursery school, Preschool
                                           "B14001_004", # EDUCATION - Kindergarten
                                           "B14001_005", # EDUCATION - Grade 1 to Grade 4
                                           "B14001_006", # EDUCATION - Grade 5 to Grade 8
                                           "B14001_007", # EDUCATION - Grade 9 to Grade 12
                                           "B14001_008", # EDUCATION - College, Undergraduate years
                                           "B14001_009", # EDUCATION - Graduate or Professional school
                                           "B14001_010", # EDUCATION - Not enrolled in school
                                           "B23025_001", # LABOR FORCE - Total
                                           "B23025_002", # LABOR FORCE - In
                                           "B23025_007", # LABOR FORCE - Not
                                           "B23025_003", # EMPLOYMENT STATUS - Total
                                           "B23025_004", # EMPLOYMENT STATUS - Employed
                                           "B23025_005", # EMPLOYMENT STATUS - Unemployed
                                           "B25099_001", # Median Household Income - Total
                                           "B25088_001", # Median selected monthly owner costs
                                           "B25058_001", # Median Contract Rent
                                           "B25064_001", # Median Gross Rent
                                           "B17001_001", # Total
                                           "B17001_002" # Below Poverty Total
                             ),
                             state = "NJ", county = c("003", "013", "017", "019", "021", 
                                                       "023", "025", "027", "029", "031", 
                                                       "035", "037", "039", "041"),
                             survey = "acs5", year = 2019)
colnames(acs19_5yr_nj)

data_Tracts_nj <- acs19_5yr_nj %>%
  select(GEOID:estimate) %>%
  cast(GEOID~variable) %>%
  dplyr::rename(
    SMRT_TTL = "B28001_001",
    SMRT_Y = "B28001_005",
    INT_TTL = "B28002_001",
    INT_Y_SBO_TTL = "B28002_002",
    INT_Y_SBO_CDP = "B28002_005",
    INT_Y_SBO_BRD = "B28002_007",
    INT_Y_SBX_TTL = "B28002_012",
    AGE_MED = "B01002_001", # MEDIAN AGE - Total ##########
    #RAC_TTL = "B02001_001", # RACE - Total
    #RAC_WHT = "B02001_002", # RACE - White
    #RAC_AFA = "B02001_003", # RACE - Black or African-American
    #RAC_AIN = "B02001_004", # RACE - American Indian and Alaska Native
    #RAC_ASN = "B02001_005", # RACE - Asian
    #RAC_HPI = "B02001_006", # RACE - Native Hawaiian and Other Pacific Islander
    #RAC_OT1 = "B02001_007", # RACE - Some other race alone
    #RAC_OT2 = "B02001_008", # RACE - Two or more races
    RAC_TTL = "B03002_001", # RACE & HISP - TTL
    RAC_NHS_TTL = "B03002_002", # RACE & HISP - NOT - TTL
    RAC_NHS_WHT = "B03002_003", # RACE & HISP - NOT - WHT
    RAC_NHS_AFA = "B03002_004", # RACE & HISP - NOT - AFA
    RAC_NHS_AIN = "B03002_005", # RACE & HISP - NOT - AIN
    RAC_NHS_ASN = "B03002_006", # RACE & HISP - NOT - ASN
    RAC_NHS_HPI = "B03002_007", # RACE & HISP - NOT - HPI
    RAC_NHS_OT1 = "B03002_008", # RACE & HISP - NOT - OT1
    RAC_NHS_OT2 = "B03002_009", # RACE & HISP - NOT - OT2
    RAC_YHS_TTL = "B03002_012", # RACE & HISP - YES - TTL
    RAC_YHS_WHT = "B03002_013", # RACE & HISP - YES - WHT
    RAC_YHS_AFA = "B03002_014", # RACE & HISP - YES - AFA
    RAC_YHS_AIN = "B03002_015", # RACE & HISP - YES - AIN
    RAC_YHS_ASN = "B03002_016", # RACE & HISP - YES - ASN
    RAC_YHS_HPI = "B03002_017", # RACE & HISP - YES - HPI
    RAC_YHS_OT1 = "B03002_018", # RACE & HISP - YES - OT1
    RAC_YHS_OT2 = "B03002_019", # RACE & HISP - YES - OT2
    EDU_TTL = "B14001_001", # EDUCATION - Total
    EDU_PRE = "B14001_003", # EDUCATION - Nursery school, Preschool
    EDU_KIN = "B14001_004", # EDUCATION - Kindergarten
    EDU_104 = "B14001_005", # EDUCATION - Grade 1 to Grade 4
    EDU_508 = "B14001_006", # EDUCATION - Grade 5 to Grade 8
    EDU_912 = "B14001_007", # EDUCATION - Grade 9 to Grade 12
    EDU_COL = "B14001_008", # EDUCATION - College, Undergraduate years
    EDU_GRA = "B14001_009", # EDUCATION - Graduate or Professional school
    EDU_NIN = "B14001_010", # EDUCATION - Not enrolled in school
    LAB_TTL = "B23025_001", # LABOR FORCE - Total
    LAB_YES = "B23025_002", # LABOR FORCE - In
    LAB_NOT = "B23025_007", # LABOR FORCE - Not
    EMP_TTL = "B23025_003", # EMPLOYMENT STATUS - Total
    EMP_YES = "B23025_004", # EMPLOYMENT STATUS - Employed
    EMP_NOT = "B23025_005", # EMPLOYMENT STATUS - Unemployed
    INC_MED = "B25099_001", # Median Household Income - Total ##########
    OWC_MED = "B25088_001", # Median selected monthly owner costs ##########
    CRT_MED = "B25058_001", # Median Contract Rent ##########
    GRT_MED = "B25064_001",  # Median Gross Rent ##########
    PVT_TTL = "B17001_001", # Poverty Total
    PVT_BLW_TTL = "B17001_002" # Below Poverty Total
  ) %>%
  mutate(
    RAC_NHS_OTH = RAC_NHS_HPI + RAC_NHS_OT1 + RAC_NHS_OT2,
    RAC_NHS_OTH2 = RAC_NHS_OTH + RAC_NHS_AIN,
    EDU_LV1 = EDU_PRE + EDU_KIN + EDU_104 + EDU_508 + EDU_NIN,
    EDU_LV2 = EDU_912,
    EDU_LV3 = EDU_COL,
    EDU_LV4 = EDU_GRA,
    INT_Y = INT_Y_SBO_TTL + INT_Y_SBX_TTL,
    TEC_INT_PCT = (INT_Y / INT_TTL) * 100,
    TEC_BRD_PCT = (INT_Y_SBO_BRD / INT_Y_SBO_TTL) * 100,
    TEC_SMR_PCT = (SMRT_Y / SMRT_TTL) * 100,
    TEC_CDP_PCT = (INT_Y_SBO_CDP / INT_Y_SBO_TTL) * 100,
    RAC_WHT_PCT = (RAC_NHS_WHT / RAC_TTL) * 100,
    RAC_AFA_PCT = (RAC_NHS_AFA / RAC_TTL) * 100,
    RAC_ASN_PCT = (RAC_NHS_ASN / RAC_TTL) * 100,
    RAC_HSP_PCT = (RAC_YHS_TTL / RAC_TTL) * 100,
    RAC_OTH_PCT = (RAC_NHS_OTH2 / RAC_TTL) * 100,
    EDU_LV1_PCT = (EDU_LV1 / EDU_TTL) * 100,
    EDU_LV2_PCT = (EDU_LV2 / EDU_TTL) * 100,
    EDU_LV3_PCT = (EDU_LV3 / EDU_TTL) * 100,
    EDU_LV4_PCT = (EDU_LV4 / EDU_TTL) * 100,
    LAB_YES_PCT = (LAB_YES / LAB_TTL) * 100,
    EMP_NOT_PCT = (EMP_NOT / EMP_TTL) * 100,
    PVT_BLW_PCT = (PVT_BLW_TTL / PVT_TTL) * 100
  ) %>%
  mutate_at(vars(TEC_INT_PCT, TEC_BRD_PCT, TEC_SMR_PCT, TEC_CDP_PCT, RAC_WHT_PCT, RAC_AFA_PCT, 
                 RAC_ASN_PCT, RAC_HSP_PCT, RAC_OTH_PCT, EDU_LV1_PCT, EDU_LV2_PCT, EDU_LV3_PCT, EDU_LV4_PCT,
                 LAB_YES_PCT, EMP_NOT_PCT, PVT_BLW_PCT), 
            funs(round(., 1))) %>%
  select(GEOID, TEC_INT_PCT:TEC_CDP_PCT, AGE_MED, RAC_NHS_WHT, RAC_NHS_AFA, RAC_NHS_ASN, RAC_YHS_TTL, RAC_NHS_OTH2,
         RAC_WHT_PCT:RAC_OTH_PCT, EDU_LV1_PCT:EDU_LV4_PCT, EMP_NOT_PCT, 
         LAB_YES_PCT, INC_MED, OWC_MED, CRT_MED, GRT_MED, PVT_BLW_PCT) %>%
  mutate(TEC_INT_PCT  = na_if(TEC_INT_PCT, 0.0),
         TEC_BRD_PCT  = na_if(TEC_BRD_PCT, 0.0),
         TEC_SMR_PCT  = na_if(TEC_SMR_PCT, 0.0),
         TEC_CDP_PCT  = na_if(TEC_CDP_PCT, 0.0))

replace_na(list(TEC_INT_PCT = 0, TEC_BRD_PCT = 0, TEC_SMR_PCT = 0, TEC_CDP_PCT = 0, EMP_NOT_PCT = 0)) %>%
replace_na(list(RAC_WHT_PCT = 0, RAC_AFA_PCT = 0, RAC_AIN_PCT = 0, RAC_ASN_PCT = 0,
                  RAC_HSP_PCT = 0, RAC_OTH_PCT = 0))
head(data_Tracts_nj)



# 3) CT
acs19_5yr_ct <- get_acs(geography = "tract", 
                             variables = c("B28001_001", # TYPES OF COMPUTERS - Total
                                           "B28001_005", # TYPES OF COMPUTERS - Smart phone
                                           "B28002_001", # PRESENCE AND TYPES OF INTERNET - Total
                                           "B28002_002", # PRESENCE AND TYPES OF INTERNET - INT O, Subscription O
                                           "B28002_005", # PRESENCE AND TYPES OF INTERNET - INT O, Subscription O: Cellular Data Plan
                                           "B28002_007", # PRESENCE AND TYPES OF INTERNET - INT O, Subscription O: Broadband
                                           "B28002_012", # PRESENCE AND TYPES OF INTERNET - INT O, Subscription x
                                           "B01002_001", # MEDIAN AGE - Total
                                           #"B02001_001", # RACE - Total
                                           #"B02001_002", # RACE - White
                                           #"B02001_003", # RACE - Black or African-American
                                           #"B02001_004", # RACE - American Indian and Alaska Native
                                           #"B02001_005", # RACE - Asian
                                           #"B02001_006", # RACE - Native Hawaiian and Other Pacific Islander
                                           #"B02001_007", # RACE - Some other race alone
                                           #"B02001_008", # RACE - Two or more races
                                           "B03002_001", # RACE & HISP - TTL
                                           "B03002_002", # RACE & HISP - NOT - TTL
                                           "B03002_003", # RACE & HISP - NOT - WHT
                                           "B03002_004", # RACE & HISP - NOT - AFA
                                           "B03002_005", # RACE & HISP - NOT - AIN
                                           "B03002_006", # RACE & HISP - NOT - ASN
                                           "B03002_007", # RACE & HISP - NOT - HPI
                                           "B03002_008", # RACE & HISP - NOT - OT1
                                           "B03002_009", # RACE & HISP - NOT - OT2
                                           "B03002_012", # RACE & HISP - YES - TTL
                                           "B03002_013", # RACE & HISP - YES - WHT
                                           "B03002_014", # RACE & HISP - YES - AFA
                                           "B03002_015", # RACE & HISP - YES - AIN
                                           "B03002_016", # RACE & HISP - YES - ASN
                                           "B03002_017", # RACE & HISP - YES - HPI
                                           "B03002_018", # RACE & HISP - YES - OT1
                                           "B03002_019", # RACE & HISP - YES - OT2
                                           "B14001_001", # EDUCATION - Total
                                           "B14001_003", # EDUCATION - Nursery school, Preschool
                                           "B14001_004", # EDUCATION - Kindergarten
                                           "B14001_005", # EDUCATION - Grade 1 to Grade 4
                                           "B14001_006", # EDUCATION - Grade 5 to Grade 8
                                           "B14001_007", # EDUCATION - Grade 9 to Grade 12
                                           "B14001_008", # EDUCATION - College, Undergraduate years
                                           "B14001_009", # EDUCATION - Graduate or Professional school
                                           "B14001_010", # EDUCATION - Not enrolled in school
                                           "B23025_001", # LABOR FORCE - Total
                                           "B23025_002", # LABOR FORCE - In
                                           "B23025_007", # LABOR FORCE - Not
                                           "B23025_003", # EMPLOYMENT STATUS - Total
                                           "B23025_004", # EMPLOYMENT STATUS - Employed
                                           "B23025_005", # EMPLOYMENT STATUS - Unemployed
                                           "B25099_001", # Median Household Income - Total
                                           "B25088_001", # Median selected monthly owner costs
                                           "B25058_001", # Median Contract Rent
                                           "B25064_001", # Median Gross Rent
                                           "B17001_001", # Total
                                           "B17001_002" # Below Poverty Total
                             ),
                             state = "CT", county = c("001", "005", "009"),
                             survey = "acs5", year = 2019)
colnames(acs19_5yr_ct)

data_Tracts_ct <- acs19_5yr_ct %>%
  select(GEOID:estimate) %>%
  cast(GEOID~variable) %>%
  dplyr::rename(
    SMRT_TTL = "B28001_001",
    SMRT_Y = "B28001_005",
    INT_TTL = "B28002_001",
    INT_Y_SBO_TTL = "B28002_002",
    INT_Y_SBO_CDP = "B28002_005",
    INT_Y_SBO_BRD = "B28002_007",
    INT_Y_SBX_TTL = "B28002_012",
    AGE_MED = "B01002_001", # MEDIAN AGE - Total ##########
    #RAC_TTL = "B02001_001", # RACE - Total
    #RAC_WHT = "B02001_002", # RACE - White
    #RAC_AFA = "B02001_003", # RACE - Black or African-American
    #RAC_AIN = "B02001_004", # RACE - American Indian and Alaska Native
    #RAC_ASN = "B02001_005", # RACE - Asian
    #RAC_HPI = "B02001_006", # RACE - Native Hawaiian and Other Pacific Islander
    #RAC_OT1 = "B02001_007", # RACE - Some other race alone
    #RAC_OT2 = "B02001_008", # RACE - Two or more races
    RAC_TTL = "B03002_001", # RACE & HISP - TTL
    RAC_NHS_TTL = "B03002_002", # RACE & HISP - NOT - TTL
    RAC_NHS_WHT = "B03002_003", # RACE & HISP - NOT - WHT
    RAC_NHS_AFA = "B03002_004", # RACE & HISP - NOT - AFA
    RAC_NHS_AIN = "B03002_005", # RACE & HISP - NOT - AIN
    RAC_NHS_ASN = "B03002_006", # RACE & HISP - NOT - ASN
    RAC_NHS_HPI = "B03002_007", # RACE & HISP - NOT - HPI
    RAC_NHS_OT1 = "B03002_008", # RACE & HISP - NOT - OT1
    RAC_NHS_OT2 = "B03002_009", # RACE & HISP - NOT - OT2
    RAC_YHS_TTL = "B03002_012", # RACE & HISP - YES - TTL
    RAC_YHS_WHT = "B03002_013", # RACE & HISP - YES - WHT
    RAC_YHS_AFA = "B03002_014", # RACE & HISP - YES - AFA
    RAC_YHS_AIN = "B03002_015", # RACE & HISP - YES - AIN
    RAC_YHS_ASN = "B03002_016", # RACE & HISP - YES - ASN
    RAC_YHS_HPI = "B03002_017", # RACE & HISP - YES - HPI
    RAC_YHS_OT1 = "B03002_018", # RACE & HISP - YES - OT1
    RAC_YHS_OT2 = "B03002_019", # RACE & HISP - YES - OT2
    EDU_TTL = "B14001_001", # EDUCATION - Total
    EDU_PRE = "B14001_003", # EDUCATION - Nursery school, Preschool
    EDU_KIN = "B14001_004", # EDUCATION - Kindergarten
    EDU_104 = "B14001_005", # EDUCATION - Grade 1 to Grade 4
    EDU_508 = "B14001_006", # EDUCATION - Grade 5 to Grade 8
    EDU_912 = "B14001_007", # EDUCATION - Grade 9 to Grade 12
    EDU_COL = "B14001_008", # EDUCATION - College, Undergraduate years
    EDU_GRA = "B14001_009", # EDUCATION - Graduate or Professional school
    EDU_NIN = "B14001_010", # EDUCATION - Not enrolled in school
    LAB_TTL = "B23025_001", # LABOR FORCE - Total
    LAB_YES = "B23025_002", # LABOR FORCE - In
    LAB_NOT = "B23025_007", # LABOR FORCE - Not
    EMP_TTL = "B23025_003", # EMPLOYMENT STATUS - Total
    EMP_YES = "B23025_004", # EMPLOYMENT STATUS - Employed
    EMP_NOT = "B23025_005", # EMPLOYMENT STATUS - Unemployed
    INC_MED = "B25099_001", # Median Household Income - Total ##########
    OWC_MED = "B25088_001", # Median selected monthly owner costs ##########
    CRT_MED = "B25058_001", # Median Contract Rent ##########
    GRT_MED = "B25064_001",  # Median Gross Rent ##########
    PVT_TTL = "B17001_001", # Poverty Total
    PVT_BLW_TTL = "B17001_002" # Below Poverty Total
  ) %>%
  mutate(
    RAC_NHS_OTH = RAC_NHS_HPI + RAC_NHS_OT1 + RAC_NHS_OT2,
    RAC_NHS_OTH2 = RAC_NHS_OTH + RAC_NHS_AIN,
    EDU_LV1 = EDU_PRE + EDU_KIN + EDU_104 + EDU_508 + EDU_NIN,
    EDU_LV2 = EDU_912,
    EDU_LV3 = EDU_COL,
    EDU_LV4 = EDU_GRA,
    INT_Y = INT_Y_SBO_TTL + INT_Y_SBX_TTL,
    TEC_INT_PCT = (INT_Y / INT_TTL) * 100,
    TEC_BRD_PCT = (INT_Y_SBO_BRD / INT_Y_SBO_TTL) * 100,
    TEC_SMR_PCT = (SMRT_Y / SMRT_TTL) * 100,
    TEC_CDP_PCT = (INT_Y_SBO_CDP / INT_Y_SBO_TTL) * 100,
    RAC_WHT_PCT = (RAC_NHS_WHT / RAC_TTL) * 100,
    RAC_AFA_PCT = (RAC_NHS_AFA / RAC_TTL) * 100,
    RAC_ASN_PCT = (RAC_NHS_ASN / RAC_TTL) * 100,
    RAC_HSP_PCT = (RAC_YHS_TTL / RAC_TTL) * 100,
    RAC_OTH_PCT = (RAC_NHS_OTH2 / RAC_TTL) * 100,
    EDU_LV1_PCT = (EDU_LV1 / EDU_TTL) * 100,
    EDU_LV2_PCT = (EDU_LV2 / EDU_TTL) * 100,
    EDU_LV3_PCT = (EDU_LV3 / EDU_TTL) * 100,
    EDU_LV4_PCT = (EDU_LV4 / EDU_TTL) * 100,
    LAB_YES_PCT = (LAB_YES / LAB_TTL) * 100,
    EMP_NOT_PCT = (EMP_NOT / EMP_TTL) * 100,
    PVT_BLW_PCT = (PVT_BLW_TTL / PVT_TTL) * 100
  ) %>%
  mutate_at(vars(TEC_INT_PCT, TEC_BRD_PCT, TEC_SMR_PCT, TEC_CDP_PCT, RAC_WHT_PCT, RAC_AFA_PCT, 
                 RAC_ASN_PCT, RAC_HSP_PCT, RAC_OTH_PCT, EDU_LV1_PCT, EDU_LV2_PCT, EDU_LV3_PCT, EDU_LV4_PCT,
                 LAB_YES_PCT, EMP_NOT_PCT, PVT_BLW_PCT), 
            funs(round(., 1))) %>%
  select(GEOID, TEC_INT_PCT:TEC_CDP_PCT, AGE_MED, RAC_NHS_WHT, RAC_NHS_AFA, RAC_NHS_ASN, RAC_YHS_TTL, RAC_NHS_OTH2,
         RAC_WHT_PCT:RAC_OTH_PCT, EDU_LV1_PCT:EDU_LV4_PCT, EMP_NOT_PCT, 
         LAB_YES_PCT, INC_MED, OWC_MED, CRT_MED, GRT_MED, PVT_BLW_PCT) %>%
  mutate(TEC_INT_PCT  = na_if(TEC_INT_PCT, 0.0),
         TEC_BRD_PCT  = na_if(TEC_BRD_PCT, 0.0),
         TEC_SMR_PCT  = na_if(TEC_SMR_PCT, 0.0),
         TEC_CDP_PCT  = na_if(TEC_CDP_PCT, 0.0))

replace_na(list(TEC_INT_PCT = 0, TEC_BRD_PCT = 0, TEC_SMR_PCT = 0, TEC_CDP_PCT = 0, EMP_NOT_PCT = 0)) %>%
replace_na(list(RAC_WHT_PCT = 0, RAC_AFA_PCT = 0, RAC_AIN_PCT = 0, RAC_ASN_PCT = 0,
                  RAC_HSP_PCT = 0, RAC_OTH_PCT = 0))
head(data_Tracts_ct)

# Bind three states with rbind
data_Tracts_tri <- rbind(data_Tracts_ny, data_Tracts_nj, data_Tracts_ct)

# Export to csv file
write.csv(data_Tracts_tri, file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/geodata_Tracts_csv2.csv')
data_Tracts <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/geodata_Tracts_csv2.csv')


# Combine with census tracts shape file
geoData_Tracts <- merge(geo_Tracts, data_Tracts_tri, by.x = "GEOID", by.y = "GEOID")
st_write(geoData_Tracts,
         "G:/Shared drives/Projects/5035_Tech Equity/Data/csv_Tracts_shp.shp", 
         driver = "ESRI Shapefile")
View(geoData_Tracts)

#################################################################################

# Create Charts 
csvData_Counties <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/CSV/data_Counties_csv.csv')
csvData_PUMAs <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/CSV/data_PUMAs_csv.csv')
csvData_Tracts <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/CSV/data_Tracts_csv.csv')
csvData_Counties$FIPS = stri_pad_left(csvData_Counties$FIPS, 5, "0")
csvData_PUMAs$PUMACE10 = stri_pad_left(csvData_PUMAs$PUMACE10, 5, "0")
head(csvData_Counties)
head(csvData_PUMAs)
head(csvData_Tracts)

# Counties 
# 1) Access to Internet
#   A. Median Houshold Income
csvData_Counties_chart1A <- csvData_Counties %>%
  select(HHIC_MED, CINETHH_PCT_AGEG1, CINETHH_PCT_AGEG2, CINETHH_PCT_AGEG3, CINETHH_PCT_AGEG4) %>%
  dplyr::rename(Age_Under18 = "CINETHH_PCT_AGEG1",
                Age_18to39 = "CINETHH_PCT_AGEG2",
                Age_40to64 = "CINETHH_PCT_AGEG3",
                Age_65over = "CINETHH_PCT_AGEG4")

csvData_Counties_chart1A_L <- gather(csvData_Counties_chart1, Condition, Percentage, 
                                      Age_Under18:Age_65over, factor_key=TRUE)

csvData_Counties_chart1A_L %>%
  hchart(.,
         type = "scatter",
         hcaes(x = HHIC_MED,
               y = Percentage,
               group = Condition)) %>%
  hc_yAxis(opposite = FALSE,
           labels = list(format = "{value}%")) %>%
  hc_tooltip(pointFormat = '{point.x:.0f}$, 
                            {point.y:.1f}%') %>%
  hc_title(text = "Percentage of Population with Access to Internet by Age Group") %>% 
  hc_subtitle(text = "Relationship with Median Household Income by Counties in Tri-State Region")

#   B. Education
csvData_Counties_chart1B <- csvData_Counties %>%
  select(EDUC_NHS_PCT, CINETHH_PCT_AGEG1, CINETHH_PCT_AGEG2, CINETHH_PCT_AGEG3, CINETHH_PCT_AGEG4) %>%
  dplyr::rename(Age_Under18 = "CINETHH_PCT_AGEG1",
                Age_18to39 = "CINETHH_PCT_AGEG2",
                Age_40to64 = "CINETHH_PCT_AGEG3",
                Age_65over = "CINETHH_PCT_AGEG4")

csvData_Counties_chart1B_L <- gather(csvData_Counties_chart1B, Condition, Percentage, 
                                     Age_Under18:Age_65over, factor_key=TRUE)

csvData_Counties_chart1B_L %>%
  hchart(.,
         type = "scatter",
         hcaes(x = EDUC_NHS_PCT,
               y = Percentage,
               group = Condition)) %>%
  hc_yAxis(opposite = FALSE,
           labels = list(format = "{value}%")) %>%
  hc_tooltip(pointFormat = '{point.x:.0f}$, 
                            {point.y:.1f}%') %>%
  hc_title(text = "Percentage of Population with Access to Internet by Age Group") %>% 
  hc_subtitle(text = "Relationship with Education Level without High School Degree by Counties in Tri-State Region")

#   C. Race
csvData_Counties_chart1C <- csvData_Counties %>%
  select(RACE_AFA_PCT, CINETHH_PCT_AGEG1, CINETHH_PCT_AGEG2, CINETHH_PCT_AGEG3, CINETHH_PCT_AGEG4) %>%
  dplyr::rename(Age_Under18 = "CINETHH_PCT_AGEG1",
                Age_18to39 = "CINETHH_PCT_AGEG2",
                Age_40to64 = "CINETHH_PCT_AGEG3",
                Age_65over = "CINETHH_PCT_AGEG4")

csvData_Counties_chart1C_L <- gather(csvData_Counties_chart1C, Condition, Percentage, 
                                     Age_Under18:Age_65over, factor_key=TRUE)

csvData_Counties_chart1C_L %>%
  hchart(.,
         type = "scatter",
         hcaes(x = RACE_AFA_PCT,
               y = Percentage,
               group = Condition)) %>%
  hc_yAxis(opposite = FALSE,
           labels = list(format = "{value}%")) %>%
  hc_tooltip(pointFormat = '{point.x:.0f}$, 
                            {point.y:.1f}%') %>%
  hc_title(text = "Percentage of Population with Access to Internet by Age Group") %>% 
  hc_subtitle(text = "Relationship with African-American by Counties in Tri-State Region")


# PUMAs 
# 1) Access to Internet
#   A. Median Houshold Income
csvData_PUMAs_chart1A <- csvData_PUMAs %>%
  select(HHIC_MED, CINETHH_PCT_AGEG1, CINETHH_PCT_AGEG2, CINETHH_PCT_AGEG3, CINETHH_PCT_AGEG4) %>%
  dplyr::rename(Age_Under18 = "CINETHH_PCT_AGEG1",
                Age_18to39 = "CINETHH_PCT_AGEG2",
                Age_40to64 = "CINETHH_PCT_AGEG3",
                Age_65over = "CINETHH_PCT_AGEG4")

csvData_PUMAs_chart1A_L <- gather(csvData_PUMAs_chart1A, Condition, Percentage, 
                                     Age_Under18:Age_65over, factor_key=TRUE)

csvData_PUMAs_chart1A_L %>%
  hchart(.,
         type = "scatter",
         hcaes(x = HHIC_MED,
               y = Percentage,
               group = Condition)) %>%
  hc_yAxis(opposite = FALSE,
           labels = list(format = "{value}%")) %>%
  hc_tooltip(pointFormat = '{point.x:.0f}$, 
                            {point.y:.1f}%') %>%
  hc_title(text = "Percentage of Population with Access to Internet by Age Group") %>% 
  hc_subtitle(text = "Relationship with Median Household Income by PUMAs in Tri-State Region")

#   B. Education
csvData_PUMAs_chart1B <- csvData_PUMAs %>%
  select(EDUC_NHS_PCT, CINETHH_PCT_AGEG1, CINETHH_PCT_AGEG2, CINETHH_PCT_AGEG3, CINETHH_PCT_AGEG4) %>%
  dplyr::rename(Age_Under18 = "CINETHH_PCT_AGEG1",
                Age_18to39 = "CINETHH_PCT_AGEG2",
                Age_40to64 = "CINETHH_PCT_AGEG3",
                Age_65over = "CINETHH_PCT_AGEG4")

csvData_PUMAs_chart1B_L <- gather(csvData_PUMAs_chart1B, Condition, Percentage, 
                                  Age_Under18:Age_65over, factor_key=TRUE)

csvData_PUMAs_chart1B_L %>%
  hchart(.,
         type = "scatter",
         hcaes(x = EDUC_NHS_PCT,
               y = Percentage,
               group = Condition)) %>%
  hc_yAxis(opposite = FALSE,
           labels = list(format = "{value}%")) %>%
  hc_tooltip(pointFormat = '{point.x:.0f}$, 
                            {point.y:.1f}%') %>%
  hc_title(text = "Percentage of Population with Access to Internet by Age Group") %>% 
  hc_subtitle(text = "Relationship with Education Level without High School Degree by PUMAs in Tri-State Region")

csvData_PUMAs_chart1B2 <- csvData_PUMAs %>%
  select(EDUC_CLD_PCT, CINETHH_PCT_AGEG1, CINETHH_PCT_AGEG2, CINETHH_PCT_AGEG3, CINETHH_PCT_AGEG4) %>%
  dplyr::rename(Age_Under18 = "CINETHH_PCT_AGEG1",
                Age_18to39 = "CINETHH_PCT_AGEG2",
                Age_40to64 = "CINETHH_PCT_AGEG3",
                Age_65over = "CINETHH_PCT_AGEG4")

csvData_PUMAs_chart1B2_L <- gather(csvData_PUMAs_chart1B2, Condition, Percentage, 
                                   Age_Under18:Age_65over, factor_key=TRUE)

csvData_PUMAs_chart1B2_L %>%
  hchart(.,
         type = "scatter",
         hcaes(x = EDUC_CLD_PCT,
               y = Percentage,
               group = Condition)) %>%
  hc_yAxis(opposite = FALSE,
           labels = list(format = "{value}%")) %>%
  hc_tooltip(pointFormat = '{point.x:.0f}$, 
                            {point.y:.1f}%') %>%
  hc_title(text = "Percentage of Population with Access to Internet by Age Group") %>% 
  hc_subtitle(text = "Relationship with Education Level with College Degree by PUMAs in Tri-State Region")

#   C. Race
csvData_PUMAs_chart1C <- csvData_PUMAs %>%
  select(RACE_AFA_PCT, CINETHH_PCT_AGEG1, CINETHH_PCT_AGEG2, CINETHH_PCT_AGEG3, CINETHH_PCT_AGEG4) %>%
  dplyr::rename(Age_Under18 = "CINETHH_PCT_AGEG1",
                Age_18to39 = "CINETHH_PCT_AGEG2",
                Age_40to64 = "CINETHH_PCT_AGEG3",
                Age_65over = "CINETHH_PCT_AGEG4")

csvData_PUMAs_chart1C_L <- gather(csvData_PUMAs_chart1C, Condition, Percentage, 
                                  Age_Under18:Age_65over, factor_key=TRUE)

csvData_PUMAs_chart1C_L %>%
  hchart(.,
         type = "scatter",
         hcaes(x = RACE_AFA_PCT,
               y = Percentage,
               group = Condition)) %>%
  hc_yAxis(opposite = FALSE,
           labels = list(format = "{value}%")) %>%
  hc_tooltip(pointFormat = '{point.x:.0f}$, 
                            {point.y:.1f}%') %>%
  hc_title(text = "Percentage of Population with Access to Internet by Age Group") %>% 
  hc_subtitle(text = "Relationship with African-American by PUMAs in Tri-State Region")

colnames(csvData_PUMAs)
csvData_PUMAs_chart1C2 <- csvData_PUMAs %>%
  select(RACE_WHT_PCT, CINETHH_PCT_AGEG1, CINETHH_PCT_AGEG2, CINETHH_PCT_AGEG3, CINETHH_PCT_AGEG4) %>%
  dplyr::rename(Age_Under18 = "CINETHH_PCT_AGEG1",
                Age_18to39 = "CINETHH_PCT_AGEG2",
                Age_40to64 = "CINETHH_PCT_AGEG3",
                Age_65over = "CINETHH_PCT_AGEG4")

csvData_PUMAs_chart1C2_L <- gather(csvData_PUMAs_chart1C2, Condition, Percentage, 
                                   Age_Under18:Age_65over, factor_key=TRUE)

csvData_PUMAs_chart1C2_L %>%
  hchart(.,
         type = "scatter",
         hcaes(x = RACE_WHT_PCT,
               y = Percentage,
               group = Condition)) %>%
  hc_yAxis(opposite = FALSE,
           labels = list(format = "{value}%")) %>%
  hc_tooltip(pointFormat = '{point.x:.0f}$, 
                            {point.y:.1f}%') %>%
  hc_title(text = "Percentage of Population with Access to Internet by Age Group") %>% 
  hc_subtitle(text = "Relationship with White by PUMAs in Tri-State Region")


# 3) Access to Smartphone
#   A. Median Houshold Income
csvData_PUMAs_chart3A <- csvData_PUMAs %>%
  select(HHIC_MED, CISMRTPHN_PCT_AGEG1, CISMRTPHN_PCT_AGEG2, CISMRTPHN_PCT_AGEG3, CISMRTPHN_PCT_AGEG4) %>%
  dplyr::rename(Age_Under18 = "CISMRTPHN_PCT_AGEG1",
                Age_18to39 = "CISMRTPHN_PCT_AGEG2",
                Age_40to64 = "CISMRTPHN_PCT_AGEG3",
                Age_65over = "CISMRTPHN_PCT_AGEG4")

csvData_PUMAs_chart3A_L <- gather(csvData_PUMAs_chart3A, Condition, Percentage, 
                                  Age_Under18:Age_65over, factor_key=TRUE)

csvData_PUMAs_chart3A_L %>%
  hchart(.,
         type = "scatter",
         hcaes(x = HHIC_MED,
               y = Percentage,
               group = Condition)) %>%
  hc_yAxis(opposite = FALSE,
           labels = list(format = "{value}%")) %>%
  hc_tooltip(pointFormat = '{point.x:.0f}$, 
                            {point.y:.1f}%') %>%
  hc_title(text = "Percentage of Population with Access to Smartphone by Age Group") %>% 
  hc_subtitle(text = "Relationship with Median Household Income by PUMAs in Tri-State Region")

#   B. Education
csvData_PUMAs_chart3B <- csvData_PUMAs %>%
  select(EDUC_NHS_PCT, CISMRTPHN_PCT_AGEG1, CISMRTPHN_PCT_AGEG2, CISMRTPHN_PCT_AGEG3, CISMRTPHN_PCT_AGEG4) %>%
  dplyr::rename(Age_Under18 = "CISMRTPHN_PCT_AGEG1",
                Age_18to39 = "CISMRTPHN_PCT_AGEG2",
                Age_40to64 = "CISMRTPHN_PCT_AGEG3",
                Age_65over = "CISMRTPHN_PCT_AGEG4")

csvData_PUMAs_chart3B_L <- gather(csvData_PUMAs_chart3B, Condition, Percentage, 
                                  Age_Under18:Age_65over, factor_key=TRUE)

csvData_PUMAs_chart3B_L %>%
  hchart(.,
         type = "scatter",
         hcaes(x = EDUC_NHS_PCT,
               y = Percentage,
               group = Condition)) %>%
  hc_yAxis(opposite = FALSE,
           labels = list(format = "{value}%")) %>%
  hc_tooltip(pointFormat = '{point.x:.0f}$, 
                            {point.y:.1f}%') %>%
  hc_title(text = "Percentage of Population with Access to Smartphone by Age Group") %>% 
  hc_subtitle(text = "Relationship with Education Level without High School Degree by PUMAs in Tri-State Region")

csvData_PUMAs_chart3B2 <- csvData_PUMAs %>%
  select(EDUC_CLD_PCT, CISMRTPHN_PCT_AGEG1, CISMRTPHN_PCT_AGEG2, CISMRTPHN_PCT_AGEG3, CISMRTPHN_PCT_AGEG4) %>%
  dplyr::rename(Age_Under18 = "CISMRTPHN_PCT_AGEG1",
                Age_18to39 = "CISMRTPHN_PCT_AGEG2",
                Age_40to64 = "CISMRTPHN_PCT_AGEG3",
                Age_65over = "CISMRTPHN_PCT_AGEG4")

csvData_PUMAs_chart3B2_L <- gather(csvData_PUMAs_chart3B2, Condition, Percentage, 
                                   Age_Under18:Age_65over, factor_key=TRUE)

csvData_PUMAs_chart3B2_L %>%
  hchart(.,
         type = "scatter",
         hcaes(x = EDUC_CLD_PCT,
               y = Percentage,
               group = Condition)) %>%
  hc_yAxis(opposite = FALSE,
           labels = list(format = "{value}%")) %>%
  hc_tooltip(pointFormat = '{point.x:.0f}$, 
                            {point.y:.1f}%') %>%
  hc_title(text = "Percentage of Population with Access to Smartphone by Age Group") %>% 
  hc_subtitle(text = "Relationship with Education Level with College Degree by PUMAs in Tri-State Region")

#   C. Race
csvData_PUMAs_chart3C <- csvData_PUMAs %>%
  select(RACE_AFA_PCT, CISMRTPHN_PCT_AGEG1, CISMRTPHN_PCT_AGEG2, CISMRTPHN_PCT_AGEG3, CISMRTPHN_PCT_AGEG4) %>%
  dplyr::rename(Age_Under18 = "CISMRTPHN_PCT_AGEG1",
                Age_18to39 = "CISMRTPHN_PCT_AGEG2",
                Age_40to64 = "CISMRTPHN_PCT_AGEG3",
                Age_65over = "CISMRTPHN_PCT_AGEG4")

csvData_PUMAs_chart3C_L <- gather(csvData_PUMAs_chart3C, Condition, Percentage, 
                                  Age_Under18:Age_65over, factor_key=TRUE)

csvData_PUMAs_chart3C_L %>%
  hchart(.,
         type = "scatter",
         hcaes(x = RACE_AFA_PCT,
               y = Percentage,
               group = Condition)) %>%
  hc_yAxis(opposite = FALSE,
           labels = list(format = "{value}%")) %>%
  hc_tooltip(pointFormat = '{point.x:.0f}$, 
                            {point.y:.1f}%') %>%
  hc_title(text = "Percentage of Population with Access to Smartphone by Age Group") %>% 
  hc_subtitle(text = "Relationship with African-American by PUMAs in Tri-State Region")

colnames(csvData_PUMAs)
csvData_PUMAs_chart3C2 <- csvData_PUMAs %>%
  select(RACE_WHT_PCT, CISMRTPHN_PCT_AGEG1, CISMRTPHN_PCT_AGEG2, CISMRTPHN_PCT_AGEG3, CISMRTPHN_PCT_AGEG4) %>%
  dplyr::rename(Age_Under18 = "CISMRTPHN_PCT_AGEG1",
                Age_18to39 = "CISMRTPHN_PCT_AGEG2",
                Age_40to64 = "CISMRTPHN_PCT_AGEG3",
                Age_65over = "CISMRTPHN_PCT_AGEG4")

csvData_PUMAs_chart3C2_L <- gather(csvData_PUMAs_chart3C2, Condition, Percentage, 
                                   Age_Under18:Age_65over, factor_key=TRUE)

csvData_PUMAs_chart3C2_L %>%
  hchart(.,
         type = "scatter",
         hcaes(x = RACE_WHT_PCT,
               y = Percentage,
               group = Condition)) %>%
  hc_yAxis(opposite = FALSE,
           labels = list(format = "{value}%")) %>%
  hc_tooltip(pointFormat = '{point.x:.0f}$, 
                            {point.y:.1f}%') %>%
  hc_title(text = "Percentage of Population with Access to Smartphone by Age Group") %>% 
  hc_subtitle(text = "Relationship with White by PUMAs in Tri-State Region")


# 2) Access to Broadband
#   A. Median Houshold Income
csvData_PUMAs_chart2A <- csvData_PUMAs %>%
  select(HHIC_MED, CIHISPEED_PCT_AGEG1, CIHISPEED_PCT_AGEG2, CIHISPEED_PCT_AGEG3, CIHISPEED_PCT_AGEG4) %>%
  dplyr::rename(Age_Under18 = "CIHISPEED_PCT_AGEG1",
                Age_18to39 = "CIHISPEED_PCT_AGEG2",
                Age_40to64 = "CIHISPEED_PCT_AGEG3",
                Age_65over = "CIHISPEED_PCT_AGEG4")

csvData_PUMAs_chart2A_L <- gather(csvData_PUMAs_chart2A, Condition, Percentage, 
                                  Age_Under18:Age_65over, factor_key=TRUE)

csvData_PUMAs_chart2A_L %>%
  hchart(.,
         type = "scatter",
         hcaes(x = HHIC_MED,
               y = Percentage,
               group = Condition)) %>%
  hc_yAxis(opposite = FALSE,
           labels = list(format = "{value}%")) %>%
  hc_tooltip(pointFormat = '{point.x:.0f}$, 
                            {point.y:.1f}%') %>%
  hc_title(text = "Percentage of Population with Access to Broadband by Age Group") %>% 
  hc_subtitle(text = "Relationship with Median Household Income by PUMAs in Tri-State Region")

#   B. Education
csvData_PUMAs_chart2B <- csvData_PUMAs %>%
  select(EDUC_NHS_PCT, CIHISPEED_PCT_AGEG1, CIHISPEED_PCT_AGEG2, CIHISPEED_PCT_AGEG3, CIHISPEED_PCT_AGEG4) %>%
  dplyr::rename(Age_Under18 = "CIHISPEED_PCT_AGEG1",
                Age_18to39 = "CIHISPEED_PCT_AGEG2",
                Age_40to64 = "CIHISPEED_PCT_AGEG3",
                Age_65over = "CIHISPEED_PCT_AGEG4")

csvData_PUMAs_chart2B_L <- gather(csvData_PUMAs_chart2B, Condition, Percentage, 
                                  Age_Under18:Age_65over, factor_key=TRUE)

csvData_PUMAs_chart2B_L %>%
  hchart(.,
         type = "scatter",
         hcaes(x = EDUC_NHS_PCT,
               y = Percentage,
               group = Condition)) %>%
  hc_yAxis(opposite = FALSE,
           labels = list(format = "{value}%")) %>%
  hc_tooltip(pointFormat = '{point.x:.0f}$, 
                            {point.y:.1f}%') %>%
  hc_title(text = "Percentage of Population with Access to Broadband by Age Group") %>% 
  hc_subtitle(text = "Relationship with Education Level without High School Degree by PUMAs in Tri-State Region")

csvData_PUMAs_chart2B2 <- csvData_PUMAs %>%
  select(EDUC_CLD_PCT, CIHISPEED_PCT_AGEG1, CIHISPEED_PCT_AGEG2, CIHISPEED_PCT_AGEG3, CIHISPEED_PCT_AGEG4) %>%
  dplyr::rename(Age_Under18 = "CIHISPEED_PCT_AGEG1",
                Age_18to39 = "CIHISPEED_PCT_AGEG2",
                Age_40to64 = "CIHISPEED_PCT_AGEG3",
                Age_65over = "CIHISPEED_PCT_AGEG4")

csvData_PUMAs_chart2B2_L <- gather(csvData_PUMAs_chart2B2, Condition, Percentage, 
                                   Age_Under18:Age_65over, factor_key=TRUE)

csvData_PUMAs_chart2B2_L %>%
  hchart(.,
         type = "scatter",
         hcaes(x = EDUC_CLD_PCT,
               y = Percentage,
               group = Condition)) %>%
  hc_yAxis(opposite = FALSE,
           labels = list(format = "{value}%")) %>%
  hc_tooltip(pointFormat = '{point.x:.0f}$, 
                            {point.y:.1f}%') %>%
  hc_title(text = "Percentage of Population with Access to Broadband by Age Group") %>% 
  hc_subtitle(text = "Relationship with Education Level with College Degree by PUMAs in Tri-State Region")

#   C. Race
csvData_PUMAs_chart2C <- csvData_PUMAs %>%
  select(RACE_AFA_PCT, CIHISPEED_PCT_AGEG1, CIHISPEED_PCT_AGEG2, CIHISPEED_PCT_AGEG3, CIHISPEED_PCT_AGEG4) %>%
  dplyr::rename(Age_Under18 = "CIHISPEED_PCT_AGEG1",
                Age_18to39 = "CIHISPEED_PCT_AGEG2",
                Age_40to64 = "CIHISPEED_PCT_AGEG3",
                Age_65over = "CIHISPEED_PCT_AGEG4")

csvData_PUMAs_chart2C_L <- gather(csvData_PUMAs_chart2C, Condition, Percentage, 
                                  Age_Under18:Age_65over, factor_key=TRUE)

csvData_PUMAs_chart2C_L %>%
  hchart(.,
         type = "scatter",
         hcaes(x = RACE_AFA_PCT,
               y = Percentage,
               group = Condition)) %>%
  hc_yAxis(opposite = FALSE,
           labels = list(format = "{value}%")) %>%
  hc_tooltip(pointFormat = '{point.x:.0f}$, 
                            {point.y:.1f}%') %>%
  hc_title(text = "Percentage of Population with Access to Broadband by Age Group") %>% 
  hc_subtitle(text = "Relationship with African-American by PUMAs in Tri-State Region")

colnames(csvData_PUMAs)
csvData_PUMAs_chart2C2 <- csvData_PUMAs %>%
  select(RACE_WHT_PCT, CIHISPEED_PCT_AGEG1, CIHISPEED_PCT_AGEG2, CIHISPEED_PCT_AGEG3, CIHISPEED_PCT_AGEG4) %>%
  dplyr::rename(Age_Under18 = "CIHISPEED_PCT_AGEG1",
                Age_18to39 = "CIHISPEED_PCT_AGEG2",
                Age_40to64 = "CIHISPEED_PCT_AGEG3",
                Age_65over = "CIHISPEED_PCT_AGEG4")

csvData_PUMAs_chart2C2_L <- gather(csvData_PUMAs_chart2C2, Condition, Percentage, 
                                   Age_Under18:Age_65over, factor_key=TRUE)

csvData_PUMAs_chart2C2_L %>%
  hchart(.,
         type = "scatter",
         hcaes(x = RACE_WHT_PCT,
               y = Percentage,
               group = Condition)) %>%
  hc_yAxis(opposite = FALSE,
           labels = list(format = "{value}%")) %>%
  hc_tooltip(pointFormat = '{point.x:.0f}$, 
                            {point.y:.1f}%') %>%
  hc_title(text = "Percentage of Population with Access to Broadband by Age Group") %>% 
  hc_subtitle(text = "Relationship with White by PUMAs in Tri-State Region")


colnames(csvData_PUMAs)
# 4) Access to Phone with Data Plan
#   A. Median Houshold Income
csvData_PUMAs_chart4A <- csvData_PUMAs %>%
  select(HHIC_MED, CIDATAPLN_PCT_AGEG1, CIDATAPLN_PCT_AGEG2, CIDATAPLN_PCT_AGEG3, CIDATAPLN_PCT_AGEG4) %>%
  dplyr::rename(Age_Under18 = "CIDATAPLN_PCT_AGEG1",
                Age_18to39 = "CIDATAPLN_PCT_AGEG2",
                Age_40to64 = "CIDATAPLN_PCT_AGEG3",
                Age_65over = "CIDATAPLN_PCT_AGEG4")

csvData_PUMAs_chart4A_L <- gather(csvData_PUMAs_chart4A, Condition, Percentage, 
                                  Age_Under18:Age_65over, factor_key=TRUE)

csvData_PUMAs_chart4A_L %>%
  hchart(.,
         type = "scatter",
         hcaes(x = HHIC_MED,
               y = Percentage,
               group = Condition)) %>%
  hc_yAxis(opposite = FALSE,
           labels = list(format = "{value}%")) %>%
  hc_tooltip(pointFormat = '{point.x:.0f}$, 
                            {point.y:.1f}%') %>%
  hc_title(text = "Percentage of Population with Access to Phone with Data Plan by Age Group") %>% 
  hc_subtitle(text = "Relationship with Median Household Income by PUMAs in Tri-State Region")

#   B. Education
csvData_PUMAs_chart4B <- csvData_PUMAs %>%
  select(EDUC_NHS_PCT, CIDATAPLN_PCT_AGEG1, CIDATAPLN_PCT_AGEG2, CIDATAPLN_PCT_AGEG3, CIDATAPLN_PCT_AGEG4) %>%
  dplyr::rename(Age_Under18 = "CIDATAPLN_PCT_AGEG1",
                Age_18to39 = "CIDATAPLN_PCT_AGEG2",
                Age_40to64 = "CIDATAPLN_PCT_AGEG3",
                Age_65over = "CIDATAPLN_PCT_AGEG4")

csvData_PUMAs_chart4B_L <- gather(csvData_PUMAs_chart4B, Condition, Percentage, 
                                  Age_Under18:Age_65over, factor_key=TRUE)

csvData_PUMAs_chart4B_L %>%
  hchart(.,
         type = "scatter",
         hcaes(x = EDUC_NHS_PCT,
               y = Percentage,
               group = Condition)) %>%
  hc_yAxis(opposite = FALSE,
           labels = list(format = "{value}%")) %>%
  hc_tooltip(pointFormat = '{point.x:.0f}$, 
                            {point.y:.1f}%') %>%
  hc_title(text = "Percentage of Population with Access to Phone with Data Plan by Age Group") %>% 
  hc_subtitle(text = "Relationship with Education Level without High School Degree by PUMAs in Tri-State Region")

csvData_PUMAs_chart4B2 <- csvData_PUMAs %>%
  select(EDUC_CLD_PCT, CIDATAPLN_PCT_AGEG1, CIDATAPLN_PCT_AGEG2, CIDATAPLN_PCT_AGEG3, CIDATAPLN_PCT_AGEG4) %>%
  dplyr::rename(Age_Under18 = "CIDATAPLN_PCT_AGEG1",
                Age_18to39 = "CIDATAPLN_PCT_AGEG2",
                Age_40to64 = "CIDATAPLN_PCT_AGEG3",
                Age_65over = "CIDATAPLN_PCT_AGEG4")

csvData_PUMAs_chart4B2_L <- gather(csvData_PUMAs_chart4B2, Condition, Percentage, 
                                   Age_Under18:Age_65over, factor_key=TRUE)

csvData_PUMAs_chart4B2_L %>%
  hchart(.,
         type = "scatter",
         hcaes(x = EDUC_CLD_PCT,
               y = Percentage,
               group = Condition)) %>%
  hc_yAxis(opposite = FALSE,
           labels = list(format = "{value}%")) %>%
  hc_tooltip(pointFormat = '{point.x:.0f}$, 
                            {point.y:.1f}%') %>%
  hc_title(text = "Percentage of Population with Access to Phone with Data Plan by Age Group") %>% 
  hc_subtitle(text = "Relationship with Education Level with College Degree by PUMAs in Tri-State Region")

#   C. Race
csvData_PUMAs_chart4C <- csvData_PUMAs %>%
  select(RACE_AFA_PCT, CIDATAPLN_PCT_AGEG1, CIDATAPLN_PCT_AGEG2, CIDATAPLN_PCT_AGEG3, CIDATAPLN_PCT_AGEG4) %>%
  dplyr::rename(Age_Under18 = "CIDATAPLN_PCT_AGEG1",
                Age_18to39 = "CIDATAPLN_PCT_AGEG2",
                Age_40to64 = "CIDATAPLN_PCT_AGEG3",
                Age_65over = "CIDATAPLN_PCT_AGEG4")

csvData_PUMAs_chart4C_L <- gather(csvData_PUMAs_chart4C, Condition, Percentage, 
                                  Age_Under18:Age_65over, factor_key=TRUE)

csvData_PUMAs_chart4C_L %>%
  hchart(.,
         type = "scatter",
         hcaes(x = RACE_AFA_PCT,
               y = Percentage,
               group = Condition)) %>%
  hc_yAxis(opposite = FALSE,
           labels = list(format = "{value}%")) %>%
  hc_tooltip(pointFormat = '{point.x:.0f}$, 
                            {point.y:.1f}%') %>%
  hc_title(text = "Percentage of Population with Access to Phone with Data Plan by Age Group") %>% 
  hc_subtitle(text = "Relationship with African-American by PUMAs in Tri-State Region")

colnames(csvData_PUMAs)
csvData_PUMAs_chart4C2 <- csvData_PUMAs %>%
  select(RACE_WHT_PCT, CIDATAPLN_PCT_AGEG1, CIDATAPLN_PCT_AGEG2, CIDATAPLN_PCT_AGEG3, CIDATAPLN_PCT_AGEG4) %>%
  dplyr::rename(Age_Under18 = "CIDATAPLN_PCT_AGEG1",
                Age_18to39 = "CIDATAPLN_PCT_AGEG2",
                Age_40to64 = "CIDATAPLN_PCT_AGEG3",
                Age_65over = "CIDATAPLN_PCT_AGEG4")

csvData_PUMAs_chart4C2_L <- gather(csvData_PUMAs_chart4C2, Condition, Percentage, 
                                   Age_Under18:Age_65over, factor_key=TRUE)

csvData_PUMAs_chart4C2_L %>%
  hchart(.,
         type = "scatter",
         hcaes(x = RACE_WHT_PCT,
               y = Percentage,
               group = Condition)) %>%
  hc_yAxis(opposite = FALSE,
           labels = list(format = "{value}%")) %>%
  hc_tooltip(pointFormat = '{point.x:.0f}$, 
                            {point.y:.1f}%') %>%
  hc_title(text = "Percentage of Population with Access to Phone with Data Plan by Age Group") %>% 
  hc_subtitle(text = "Relationship with White by PUMAs in Tri-State Region")





# Tracts
# 2) Access to Smartphone



# Create Maps
# Counties 
mapview(geoData_Counties, zcol = c("CINETHH_PCT_AGEG1", "CINETHH_PCT_AGEG2", "CINETHH_PCT_AGEG3", "CINETHH_PCT_AGEG4"), legend = TRUE, col.regions=brewer.pal(9, "RdYlGn"))
mapview(geoData_Counties, zcol = c("CISMRTPHN_PCT_AGEG1", "CISMRTPHN_PCT_AGEG2", "CISMRTPHN_PCT_AGEG3", "CISMRTPHN_PCT_AGEG4"), legend = TRUE, col.regions=brewer.pal(9, "RdYlGn"))
mapview(geoData_Counties, zcol = c("CIDATAPLN_PCT_AGEG1", "CIDATAPLN_PCT_AGEG2", "CIDATAPLN_PCT_AGEG3", "CIDATAPLN_PCT_AGEG4"), legend = TRUE, col.regions=brewer.pal(9, "RdYlGn"))
mapview(geoData_Counties, zcol = c("CIHISPEED_PCT_AGEG1", "CIHISPEED_PCT_AGEG2", "CIHISPEED_PCT_AGEG3", "CIHISPEED_PCT_AGEG4"), legend = TRUE, col.regions=brewer.pal(9, "RdYlGn"))

# PUMAs
mapview(geoData_PUMAs, zcol = c("CINETHH_PCT_AGEG1", "CINETHH_PCT_AGEG2", "CINETHH_PCT_AGEG3", "CINETHH_PCT_AGEG4"), legend = TRUE, col.regions=brewer.pal(9, "RdYlGn"))
mapview(geoData_PUMAs, zcol = c("CISMRTPHN_PCT_AGEG1", "CISMRTPHN_PCT_AGEG2", "CISMRTPHN_PCT_AGEG3", "CISMRTPHN_PCT_AGEG4"), legend = TRUE, col.regions=brewer.pal(9, "RdYlGn"))
mapview(geoData_PUMAs, zcol = c("CIDATAPLN_PCT_AGEG1", "CIDATAPLN_PCT_AGEG2", "CIDATAPLN_PCT_AGEG3", "CIDATAPLN_PCT_AGEG4"), legend = TRUE, col.regions=brewer.pal(9, "RdYlGn"))
mapview(geoData_PUMAs, zcol = c("CIHISPEED_PCT_AGEG1", "CIHISPEED_PCT_AGEG2", "CIHISPEED_PCT_AGEG3", "CIHISPEED_PCT_AGEG4"), legend = TRUE, col.regions=brewer.pal(9, "RdYlGn"))

# Tracts
mapview(geoData_Tracts, zcol = c("TEC_INT_PCT", "TEC_BRD_PCT", "TEC_SMR_PCT", "TEC_CDP_PCT"), legend = TRUE, col.regions=brewer.pal(9, "RdYlGn"))
mapview(geoData_Tracts, zcol = c("TEC_INT_PCT"), legend = TRUE, col.regions=brewer.pal(9, "RdYlGn"))
colnames(geoData_Tracts)



csvData_Tracts %>%
  drop_na(TEC_INT_PCT) %>%
  drop_na(TEC_BRD_PCT) %>%
  drop_na(TEC_SMR_PCT) %>%
  drop_na(TEC_CDP_PCT) %>%
  summarise(across(TEC_INT_PCT:TEC_CDP_PCT , mean))






#################################################################################

# B.Owner Cost & Rent - Median
ipums_val_labels(cps_data4$OWNCOST)
ipums_val_labels(cps_data4$RENT)

data4_ownc_rent_variables <- cps_data4 %>%
  select(YEAR, SAMPLE:GQ, PERNUM:PERWT, OWNCOST, RENT) %>%
  filter(PERNUM == 1) %>%
  filter(OWNCOST != 99999 | RENT != 0) %>%
  group_by(OWNCOST, RENT) %>%
  summarize(n())
View(data4_ownc_rent_variables)

# Counties
data4_ownc_rent_med_Counties <- cps_data4 %>% 
  select(YEAR, SAMPLE:GQ, PERNUM:PERWT, OWNCOST, RENT) %>%
  filter(PERNUM == 1) %>%
  filter(OWNCOST != 99999 | RENT != 0) %>%
  mutate(OWNC_RENT = case_when(
    OWNCOST == 99999 ~ RENT, 
    OWNCOST < 99999 ~ OWNCOST,
    RENT == 0 ~ OWNCOST, 
    RENT > 0 ~ RENT
  )) %>%
  group_by(CountyFIPS) %>%
  summarize(OWNC_RENT_MED = median(OWNC_RENT))
View(data4_ownc_rent_med_Counties)


# PUMAs
data4_ownc_rent_med_PUMAs <- cps_data4 %>% 
  select(YEAR, SAMPLE:GQ, PERNUM:PERWT, OWNCOST, RENT) %>%
  filter(PERNUM == 1) %>%
  filter(OWNCOST != 99999 | RENT != 0) %>%
  mutate(OWNC_RENT = case_when(
    OWNCOST == 99999 ~ RENT, 
    OWNCOST < 99999 ~ OWNCOST,
    RENT == 0 ~ OWNCOST, 
    RENT > 0 ~ RENT
  )) %>%
  group_by(PUMACE10) %>%
  summarize(OWNC_RENT_MED = median(OWNC_RENT))
View(data4_ownc_rent_med_PUMAs)



# Join with geographies
# Counties
geodata_age_tech_Counties = list (geo_Counties, data5_age_Counties, data5_age_cinethh_pct, 
                                  data5_age_cismrtphn_pct, data5_age_cidatapln_pct, data5_age_cihispeed_pct,
                                  data4_race_hisp_pct, data4_educ_pct, data4_emps_pct, data4_labf_pct,
                                  data4_hhic_med, data4_ownc_med, data4_rent_med) %>%
  reduce(left_join, by = "CountyFIPS")
# PUMAs
geodata_age_tech_PUMAs = list (geo_PUMAs, data5_age_PUMAs, data5_age_cinethh_pct2, 
                               data5_age_cismrtphn_pct2, data5_age_cidatapln_pct2, data5_age_cihispeed_pct2,
                               data4_race_hisp_pct2, data4_educ_pct2, data4_emps_pct2, data4_labf_pct2,
                               data4_hhic_med2, data4_ownc_med2, data4_rent_med2) %>%
  reduce(left_join, by = "PUMACE10")





