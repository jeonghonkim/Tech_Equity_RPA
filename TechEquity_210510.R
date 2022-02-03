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
fcc_ny <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/B_FCC/NY-Fixed-Jun2020-v1.csv')
fcc_nj <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/B_FCC/NJ-Fixed-Jun2020-v1.csv')
fcc_ct <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/B_FCC/CT-Fixed-Jun2020-v1.csv')
colnames(fcc_ny)
View(fcc_ny)

fcc_tri <- rbind(fcc_ny, fcc_nj, fcc_ct)
View(fcc_tri)
write.csv(fcc_tri, file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/B_FCC/FCC_TRI_csv.csv')
# fcc_tri <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/B_FCC/FCC_TRI_csv.csv')



# 2. GBDI
gbdi <- read.csv(file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/B_GBDI/GBDI_Unserved_CB_Jun2020.csv')
st_read()
view(gbdi)


# 3. Microsoft
mcsf_url <- "https://raw.githubusercontent.com/microsoft/USBroadbandUsagePercentages/master/dataset/broadband_data.csv"
mcsf_brbd <- read.csv(url(mcsf_url))
View(mcsf_brbd)

mcsf_zip_url <- "https://raw.githubusercontent.com/microsoft/USBroadbandUsagePercentages/master/dataset/broadband_data_zipcode.csv"
mcsf_zip_brbd <- read.csv(url(mcsf_zip_url))
head(mcsf_zip_brbd)

mcsf_zip_brbd$POSTAL.CODE = stri_pad_left(mcsf_zip_brbd$POSTAL.CODE, 5, "0")
View(mcsf_zip_brbd)

  # Select Only Tri-State
mcsf_zip_brbd_TRI <- mcsf_zip_brbd %>%
  select(ST:BROADBAND.USAGE) %>%
  group_by(ST) %>%
  filter(ST == "CT" | ST == "NY" | ST =="NJ")
view(mcsf_zip_brbd_TRI)

















