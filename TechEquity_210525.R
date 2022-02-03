
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



