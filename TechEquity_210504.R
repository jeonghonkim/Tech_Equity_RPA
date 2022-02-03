
# Tech Equity Initiative
#   1) Creating IPUMS layers
#       https://usa.ipums.org/usa/
#       Sample: USA, ACS 2019 1-YEAR AND ACS 2014-2019 5-YEAR
#       Variables: CINETHH, CISMRTPHN, CIDATAPLN, and CIHISPEED

# rm(list=ls())
# install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", "ggspatial", "libwgeom", 
#                     "rnaturalearth", "rnaturalearthdata", "devtools", "purrr", "highcharter"))
# install_github("r-spatial/sf")

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
# vignette("ipums-cps", package = "ipumsr")
# vignette("ipums-nhgis", package = "ipumsr")
# vignette("ipums-terra", package = "ipumsr")

# Set file path
setwd("G:/Shared drives/Projects/5035_Tech Equity/Data")

#################################################################################

# 05/04/2021
# Importing goedatasets
csv_Counties <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/CSV/IPUMS_Counties_Trimmed_csv.csv')
csv_PUMAs <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/CSV/IPUMS_PUMAs_Trimmed_csv.csv')

csv_Counties$FIPS = stri_pad_left(csv_Counties$FIPS, 5, "0")
csv_PUMAs$PUMACE10 = stri_pad_left(csv_PUMAs$PUMACE10, 5, "0")

view(csv_Counties)
head(csv_PUMAs)

# Finished creating layers
# 1. COUNTIES
# - There are four counties with NA values
#     County FIPS code: 36079 Putnam, NY
#                       36105 Sullivan, NY
#                       36111 Ulster, NY
#                       36119 Westchester, NY

# 2. PUMAs
# - There are not NA values in PUMAs. (170 PUMAs)



#################################################################################


# 05/04/2021

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

  
# 4. Merge Geographies with Finished Data
head(geo_Counties)
head(csv_Counties)
head(geo_PUMAs)
head(csv_PUMAs)

SpatialPolygons(csv_data_Counties)

geoData_Counties <- merge(geo_Counties, csv_Counties, by.x = "FIPS", by.y = "FIPS") # 27
geoData_PUMAs <- merge(geo_PUMAs, csv_PUMAs, by.x = "PUMACE10", by.y = "PUMACE10") # 170

head(geoData_Counties)
head(geoData_PUMAs)

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
         "G:/Shared drives/Projects/5035_Tech Equity/Data/New_Shapefile/IPUMS_PUMAs_shp.shp", 
         driver = "ESRI Shapefile")


colnames(geoData_PUMAs)


# Created Maps
# 1. County Level
mapview(geoData_Counties, zcol = c("CINETHH_PCT_AGEG1", "CINETHH_PCT_AGEG2", "CINETHH_PCT_AGEG3", "CINETHH_PCT_AGEG4"), legend = TRUE, col.regions=brewer.pal(9, "RdYlGn"))
mapview(geoData_Counties, zcol = c("CISMRTPHN_PCT_AGEG1", "CISMRTPHN_PCT_AGEG2", "CISMRTPHN_PCT_AGEG3", "CISMRTPHN_PCT_AGEG4"), legend = TRUE, col.regions=brewer.pal(9, "RdYlGn"))
mapview(geoData_Counties, zcol = c("CIDATAPLN_PCT_AGEG1", "CIDATAPLN_PCT_AGEG2", "CIDATAPLN_PCT_AGEG3", "CIDATAPLN_PCT_AGEG4"), legend = TRUE, col.regions=brewer.pal(9, "RdYlGn"))
mapview(geoData_Counties, zcol = c("CIHISPEED_PCT_AGEG1", "CIHISPEED_PCT_AGEG2", "CIHISPEED_PCT_AGEG3", "CIHISPEED_PCT_AGEG4"), legend = TRUE, col.regions=brewer.pal(9, "RdYlGn"))

# 2. PUMAs Level
mapview(geoData_PUMAs, zcol = c("CINETHH_PCT_AGEG1", "CINETHH_PCT_AGEG2", "CINETHH_PCT_AGEG3", "CINETHH_PCT_AGEG4"), legend = TRUE, col.regions=brewer.pal(9, "RdYlGn"))
mapview(geoData_PUMAs, zcol = c("CISMRTPHN_PCT_AGEG1", "CISMRTPHN_PCT_AGEG2", "CISMRTPHN_PCT_AGEG3", "CISMRTPHN_PCT_AGEG4"), legend = TRUE, col.regions=brewer.pal(9, "RdYlGn"))
mapview(geoData_PUMAs, zcol = c("CIDATAPLN_PCT_AGEG1", "CIDATAPLN_PCT_AGEG2", "CIDATAPLN_PCT_AGEG3", "CIDATAPLN_PCT_AGEG4"), legend = TRUE, col.regions=brewer.pal(9, "RdYlGn"))
mapview(geoData_PUMAs, zcol = c("CIHISPEED_PCT_AGEG1", "CIHISPEED_PCT_AGEG2", "CIHISPEED_PCT_AGEG3", "CIHISPEED_PCT_AGEG4"), legend = TRUE, col.regions=brewer.pal(9, "RdYlGn"))




# ACS FILES

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

# VARIABLES IN ACS-5, ACS-1
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


# 1) NY - Smartphone <O>, Internet Access <>, 
acs19_5_ny <- get_acs(geography = "tract", 
                              variables = c("B28001_001", 
                                            "B28001_005",
                                            "B28002_001", # Total
                                            "B28002_002", # With an Internet subscription
                                            "B28002_004", # With an Internet subscription - Broadband of any type
                                            "B28002_005", # With an Internet subscription - Cellular data plan
                                            "B28002_007", # With an Internet subscription - Broadband
                                            "B28002_012", # Internet access without a subscription
                                            'B28002_013'  # No Internet access
                                            ),
                              state = "NY", county = c("005", "027", "047", "059", "061", 
                                                       "071", "079", "081", "085", "087", 
                                                       "103", "105", "111", "119"),
                              survey = "acs5", year = 2019)
colnames(acs19_5_ny)

acs19_5_ny %>%
  select(GEOID:estimate) %>%
  cast(GEOID~variable)
  mutate(
    smrtphn_pct = (B28001_005 / B28001_001) *100
  ) %>%
  replace_na(list(smrtphn_pct = 0)) %>%
  dplyr::rename(
    smrtphn_all = "B28001_001",
    Smrtphn_yes = "B28001_005"
  ) %>%
  mutate_at(vars(smrtphn_pct), funs(round(., 1)))

# 2) NJ
acs19_5_smrtphn_nj <- get_acs(geography = "tract", 
                              variables = c("B28001_001", "B28001_005"),
                              state = "NJ", county = c("003", "013", "017", "019", "021", 
                                                       "023", "025", "027", "029", "031", 
                                                       "035", "037", "039", "041"),
                              survey = "acs5", year = 2019)
colnames(acs19_5_smrtphn_ny)

acs19_5_smrtphn_nj %>%
  select(GEOID:estimate) %>%
  cast(GEOID~variable) %>%
  mutate(
    smrtphn_pct = (B28001_005 / B28001_001) *100
  ) %>%
  replace_na(list(smrtphn_pct = 0)) %>%
  dplyr::rename(
    smrtphn_all = "B28001_001",
    Smrtphn_yes = "B28001_005"
  ) %>%
  mutate_at(vars(smrtphn_pct), funs(round(., 1)))



# 3) CT
acs19_5_smrtphn_ct <- get_acs(geography = "tract", 
                              variables = c("B28001_001", "B28001_005"),
                              state = "CT", county = c("001", "005", "009"),
                              survey = "acs5", year = 2019)
colnames(acs19_5_smrtphn_ct)

acs19_5_smrtphn_ct %>%
  select(GEOID:estimate) %>%
  cast(GEOID~variable) %>%
  mutate(
    smrtphn_pct = (B28001_005 / B28001_001) *100
  ) %>%
  replace_na(list(smrtphn_pct = 0)) %>%
  dplyr::rename(
    smrtphn_all = "B28001_001",
    Smrtphn_yes = "B28001_005"
  ) %>%
  mutate_at(vars(smrtphn_pct), funs(round(., 1)))

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





