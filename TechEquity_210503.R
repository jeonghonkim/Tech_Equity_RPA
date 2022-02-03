
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
  require(quantmod)
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

# 05/03/2021

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

csv_Counties <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Geographies/RPA_counties_ref.csv')
csv_PUMAs <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Geographies/RPA_PUMAs_ref.csv')

csv_Counties$FIPS = stri_pad_left(csv_Counties$FIPS, 5, "0")
csv_PUMAs$PUMACE10 = stri_pad_left(csv_PUMAs$PUMACE10, 5, "0")

# 3. Merge csv file & data file to export combined data
head(csv_Counties)
head(data_Counties)
head(csv_PUMAs)
head(data_PUMAs)

data_Counties <- data_Counties %>%
  mutate(
    FIPS = as.character(factor(CountyFIPS))
  )

csv_data_Counties <- list (csv_Counties, data_Counties) %>%
  reduce(left_join, by="FIPS")
csv_data_PUMAs <- list (csv_PUMAs, data_PUMAs) %>%
  reduce(left_join, by="PUMACE10")

write.csv(csv_data_Counties, file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/CSV/IPUMS_Counties_Trimmed_csv.csv')
write.csv(csv_data_PUMAs, file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/CSV/IPUMS_PUMAs_Trimmed_csv.csv')
  
# 4. Merge Geographies with Finished Data
head(geo_Counties)
head(data_Counties)
head(geo_PUMAs)
head(data_PUMAs)

geoData_Counties <- merge(geo_Counties, data_Counties, by.x = "FIPS", by.y = "FIPS") # 27
geoData_PUMAs <- merge(geo_PUMAs, data_PUMAs, by.x = "PUMACE10", by.y = "PUMACE10") # 170

st_write(geoData_Counties, 'G:/Shared drives/Projects/5035_Tech Equity/Data/New_Shapefile/IPUMS_Counties_shp.shp')
st_write(geoData_PUMAs, 'G:/Shared drives/Projects/5035_Tech Equity/Data/New_Shapefile/IPUMS_PUMAs_shp.shp')



# 1. Import 
# Finished CSV Files

View(data_IPUMS_Counties)
View(data_IPUMS_PUMAs)

# PUMAs are same, but Counties are different
# Data_IPUMS_Counties: 37 / geo_Counties: 33 / geoData_Counties: 27



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





