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
  library(readr)
  library(RCurl)
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

# Research Other Tech Equity Projects
# Explore data sets

# 1. FCC - New York States
fcc_ny <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/B_Others/FCC/NY-Fixed-Jun2020-v1.csv')
fcc_nj <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/B_Others/FCC/NJ-Fixed-Jun2020-v1.csv')
fcc_ct <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/B_Others/FCC/CT-Fixed-Jun2020-v1.csv')
colnames(fcc_ny)
View(fcc_ny)

fcc_tri <- rbind(fcc_ny, fcc_nj, fcc_ct)
View(fcc_tri)
write.csv(fcc_tri, file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/B_Others/FCC/FCC_TRI_csv.csv')
# fcc_tri <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/B_FCC/FCC_TRI_csv.csv')



# 2. GBDI
gbdi <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/B_Others/GBDI/GBDI_Unserved_CB_Jun2020.csv')
gbdi_shp = st_read("G:/Shared drives/Projects/5035_Tech Equity/Data/B_Others/GBDI/GBDI_Unserved_CB_Jun2020.shp")
view(gbdi)
view(gbdi_shp)
mapview(gbdi_shp, zcol = c("ex1", "ex2"), legend = TRUE, col.regions=brewer.pal(9, "RdYlGn"))



# 3. Microsoft
# 1) County  Level
temp_url_mcsf <- "https://raw.githubusercontent.com/microsoft/USBroadbandUsagePercentages/master/dataset/broadband_data.csv"
mcsf <- read.csv(url(temp_url_mcsf))
View(mcsf)
# It has "Percentage of Broadband Usage" by County  Level
# It compares with the FCC Data

# 2) Zip-code Level
temp_url_mcsf_zip <- "https://raw.githubusercontent.com/microsoft/USBroadbandUsagePercentages/master/dataset/broadband_data_zipcode.csv"
mcsf_zip <- read.csv(url(temp_url_mcsf_zip))
colnames(mcsf_zip)

mcsf_zip$COUNTY.ID = stri_pad_left(mcsf_zip$COUNTY.ID, 5, "0")
mcsf_zip$POSTAL.CODE = stri_pad_left(mcsf_zip$POSTAL.CODE, 5, "0")
View(mcsf_zip)

mcsf_zipcode <- mcsf_zip %>%
  select(ST:BROADBAND.USAGE) %>%
  dplyr::rename(
    BRDB_USE_PCT = "BROADBAND.USAGE"
  ) %>%
  filter(ST == "CT" | ST == "NY" | ST =="NJ")
View(mcsf_zipcode)
 
write.csv(mcsf_zipcode, file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/B_Others/Microsoft/Microsoft_TRI_csv.csv')
# mcsf_zipcode <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/B_Others/Microsoft/Microsoft_TRI_csv.csv')













