
# Tech Equity Initiative
#   1) Creating IPUMS layers
#       https://usa.ipums.org/usa/
#       Sample: USA, ACS 2019 1-YEAR AND ACS 2014-2019 5-YEAR
#       Variables: CINETHH, CISMRTPHN, CIDATAPLN, and CIHISPEED

# rm(list=ls())
# install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", "ggspatial", "libwgeom", 
#                     "rnaturalearth", "rnaturalearthdata", "devtools", "purrr"))
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
})
theme_set(theme_bw())

# vignette("value-labels", package = "ipumsr")
# vignette("ipums-geography", package = "ipumsr")
# vignette("ipums-cps", package = "ipumsr")
# vignette("ipums-nhgis", package = "ipumsr")
# vignette("ipums-terra", package = "ipumsr")

# Set file path
setwd("G:/Shared drives/Projects/5035_Tech Equity/Data")


##############################################################################

# 04/28/21

# IPUMS_DATA
cps_ddi5 <- read_ipums_ddi("usa_00005.xml")
cps_data5 <- read_ipums_micro(cps_ddi5, verbose = FALSE)

# GEOGRAPHIES
geo_Counties = st_read("G:/Shared drives/Projects/5035_Tech Equity/Geographies/RPA_counties_shp.shp")
geo_PUMAs = st_read("G:/Shared drives/Projects/5035_Tech Equity/Geographies/RPA_PUMAs_shp.shp")

# Creating leading zeros in cps_data$, and change column names same as Geo_Counties, Geo_PUMAs
headTail(cps_data5)
str(cps_data5)
summary(cps_data5)

cps_data5$COUNTYFIP = stri_pad_left(cps_data5$COUNTYFIP, 3, "0")
cps_data5$PUMA = stri_pad_left(cps_data5$PUMA, 5, "0")
names(cps_data5)[9] <- "CountyFIPS"
names(cps_data5)[10] <- "PUMACE10"


# 1) Personal-level
# A.AGE
#  1. MEIDAN
#   a. Make a aggregated subset data for new variables with Personal-level: AGE
data5_age_Counties <- aggregate(cps_data5$AGE,
                                by = list(cps_data5$CountyFIPS),
                                FUN = median, na.rm = TRUE) %>% 
  dplyr::rename (CountyFIPS = Group.1,
                 AGE = x)

data5_age_PUMAs <- aggregate(cps_data5$AGE,
                             by = list(cps_data5$PUMACE10),
                             FUN = median, na.rm = TRUE) %>%
  dplyr::rename (PUMACE10 = Group.1,
                 AGE = x)

geodata_age_Counties = merge(Geo_Counties, data5_age_Counties, by = "CountyFIPS", all.x = TRUE)
geodata_age_PUMAs = merge(Geo_PUMAs, data5_age_PUMAs, by = "PUMACE10", all.x = TRUE)

#     2. CATEGORIES
data5_age_categories <- cps_data5 %>%
  select(YEAR, SAMPLE:AGE) %>%
  mutate(
    AGE_CATE = case_when(
      AGE < 18 ~ 1,
      AGE < 40 ~ 2,
      AGE < 65 ~ 3,
      AGE > 64 ~ 4
    ),
    AGE_CATE_DES = case_when(
      AGE_CATE == 1 ~ "Under 18",
      AGE_CATE == 2 ~ "18 ~ 39",
      AGE_CATE == 3 ~ "40 ~ 64",
      AGE_CATE == 4 ~ "65 and Older"
    )
  )

# 3. AGE WITH TECH VARIABLES
#  1) CINETHH
# Counties
ipums_val_labels(cps_data5$CINETHH)
ipums_var_desc(cps_data5, "CINETHH")

data5_age_cinethh_yes <- data5_age_categories %>%
  select(YEAR:CINETHH, PERNUM:AGE_CATE_DES) %>%
  filter(CINETHH != 0) %>%
  mutate(CINETHH_SMPL = case_when(
    CINETHH == 1 ~ 1,
    CINETHH == 2 ~ 1,
    CINETHH == 3 ~ 0)) %>%
  filter(CINETHH_SMPL != 0) %>%
  group_by(CountyFIPS, AGE_CATE) %>%
  summarize(n())%>%
  dplyr::rename(CINETHH_Y = "n()")

data5_age_cinethh_no <- data5_age_categories %>%
  select(YEAR:CINETHH, PERNUM:AGE_CATE_DES) %>%
  filter(CINETHH != 0) %>%
  mutate(CINETHH_SMPL = case_when(
    CINETHH == 1 ~ 1,
    CINETHH == 2 ~ 1,
    CINETHH == 3 ~ 0)) %>%
  filter(CINETHH_SMPL != 1) %>%
  group_by(CountyFIPS, AGE_CATE) %>%
  summarize(n())%>%
  dplyr::rename(CINETHH_N = "n()")

data5_age_cinethh_all = merge(data5_age_cinethh_yes, data5_age_cinethh_no, by = c("CountyFIPS", "AGE_CATE"), all.x = TRUE) %>%
  mutate(CINETHH_ALL = CINETHH_Y + CINETHH_N,
         CINETHH_Y_PER = (CINETHH_Y / CINETHH_ALL)*100) %>%
  mutate_at(vars(CINETHH_Y_PER), funs(round(., 1)))

data5_age_cinethh_pct <- data5_age_cinethh_all %>%
  select(CountyFIPS, AGE_CATE, CINETHH_Y_PER) %>%
  cast(CountyFIPS~AGE_CATE) %>%
  dplyr::rename(CINETHH_PCT_AGEG1 = "1",
                CINETHH_PCT_AGEG2 = "2",
                CINETHH_PCT_AGEG3 = "3",
                CINETHH_PCT_AGEG4 = "4"
  )
View(data5_age_cinethh_pct)

# PUMAs
ipums_val_labels(cps_data5$CINETHH)
ipums_var_desc(cps_data5, "CINETHH")

data5_age_cinethh_yes2 <- data5_age_categories %>%
  select(YEAR:CINETHH, PERNUM:AGE_CATE_DES) %>%
  filter(CINETHH != 0) %>%
  mutate(CINETHH_SMPL = case_when(
    CINETHH == 1 ~ 1,
    CINETHH == 2 ~ 1,
    CINETHH == 3 ~ 0)) %>%
  filter(CINETHH_SMPL != 0) %>%
  group_by(PUMACE10, AGE_CATE) %>%
  summarize(n())%>%
  dplyr::rename(CINETHH_Y = "n()")

data5_age_cinethh_no2 <- data5_age_categories %>%
  select(YEAR:CINETHH, PERNUM:AGE_CATE_DES) %>%
  filter(CINETHH != 0) %>%
  mutate(CINETHH_SMPL = case_when(
    CINETHH == 1 ~ 1,
    CINETHH == 2 ~ 1,
    CINETHH == 3 ~ 0)) %>%
  filter(CINETHH_SMPL != 1) %>%
  group_by(PUMACE10, AGE_CATE) %>%
  summarize(n())%>%
  dplyr::rename(CINETHH_N = "n()")

data5_age_cinethh_all2 = merge(data5_age_cinethh_yes2, data5_age_cinethh_no2, by = c("PUMACE10", "AGE_CATE"), all.x = TRUE) %>%
  mutate(CINETHH_ALL = CINETHH_Y + CINETHH_N,
         CINETHH_Y_PER = (CINETHH_Y / CINETHH_ALL)*100) %>%
  mutate_at(vars(CINETHH_Y_PER), funs(round(., 1)))

data5_age_cinethh_pct2 <- data5_age_cinethh_all2 %>%
  select(PUMACE10, AGE_CATE, CINETHH_Y_PER) %>%
  cast(PUMACE10~AGE_CATE) %>%
  dplyr::rename(CINETHH_PCT_AGEG1 = "1",
                CINETHH_PCT_AGEG2 = "2",
                CINETHH_PCT_AGEG3 = "3",
                CINETHH_PCT_AGEG4 = "4"
  )

#  2) CISMRTPHN
# Counties
ipums_val_labels(cps_data5$CISMRTPHN)
ipums_var_desc(cps_data5, "CISMRTPHN")

data5_age_cismrtphn_yes <- data5_age_categories %>%
  select(YEAR:CISMRTPHN, PERNUM:AGE_CATE_DES) %>%
  filter(CISMRTPHN != 0) %>%
  filter(CISMRTPHN == 1) %>%
  group_by(CountyFIPS, AGE_CATE) %>%
  summarize(n())%>%
  dplyr::rename(CISMRTPHN_Y = "n()")

data5_age_cismrtphn_no <- data5_age_categories %>%
  select(YEAR:CISMRTPHN, PERNUM:AGE_CATE_DES) %>%
  filter(CISMRTPHN != 0) %>%
  filter(CISMRTPHN == 2) %>%
  group_by(CountyFIPS, AGE_CATE) %>%
  summarize(n())%>%
  dplyr::rename(CISMRTPHN_N = "n()")

data5_age_cismrtphn_all = merge(data5_age_cismrtphn_yes, data5_age_cismrtphn_no, by = c("CountyFIPS", "AGE_CATE"), all.x = TRUE) %>%
  mutate(CISMRTPHN_ALL = CISMRTPHN_Y + CISMRTPHN_N,
         CISMRTPHN_Y_PCT = (CISMRTPHN_Y / CISMRTPHN_ALL)*100) %>%
  mutate_at(vars(CISMRTPHN_Y_PCT), funs(round(., 1)))

data5_age_cismrtphn_pct <- data5_age_cismrtphn_all %>%
  select(CountyFIPS, AGE_CATE, CISMRTPHN_Y_PCT) %>%
  cast(CountyFIPS~AGE_CATE) %>%
  dplyr::rename(CISMRTPHN_PCT_AGEG1 = "1",
                CISMRTPHN_PCT_AGEG2 = "2",
                CISMRTPHN_PCT_AGEG3 = "3",
                CISMRTPHN_PCT_AGEG4 = "4"
  )

# PUMAs
data5_age_cismrtphn_yes2 <- data5_age_categories %>%
  select(YEAR:CISMRTPHN, PERNUM:AGE_CATE_DES) %>%
  filter(CISMRTPHN != 0) %>%
  filter(CISMRTPHN == 1) %>%
  group_by(PUMACE10, AGE_CATE) %>%
  summarize(n())%>%
  dplyr::rename(CISMRTPHN_Y = "n()")

data5_age_cismrtphn_no2 <- data5_age_categories %>%
  select(YEAR:CISMRTPHN, PERNUM:AGE_CATE_DES) %>%
  filter(CISMRTPHN != 0) %>%
  filter(CISMRTPHN == 2) %>%
  group_by(PUMACE10, AGE_CATE) %>%
  summarize(n())%>%
  dplyr::rename(CISMRTPHN_N = "n()")

data5_age_cismrtphn_all2 = merge(data5_age_cismrtphn_yes2, data5_age_cismrtphn_no2, by = c("PUMACE10", "AGE_CATE"), all.x = TRUE) %>%
  mutate(CISMRTPHN_ALL = CISMRTPHN_Y + CISMRTPHN_N,
         CISMRTPHN_Y_PCT = (CISMRTPHN_Y / CISMRTPHN_ALL)*100) %>%
  mutate_at(vars(CISMRTPHN_Y_PCT), funs(round(., 1)))

data5_age_cismrtphn_pct2 <- data5_age_cismrtphn_all2 %>%
  select(PUMACE10, AGE_CATE, CISMRTPHN_Y_PCT) %>%
  cast(PUMACE10~AGE_CATE) %>%
  dplyr::rename(CISMRTPHN_PCT_AGEG1 = "1",
                CISMRTPHN_PCT_AGEG2 = "2",
                CISMRTPHN_PCT_AGEG3 = "3",
                CISMRTPHN_PCT_AGEG4 = "4"
  )

View(data5_age_cismrtphn_pct2)

#  3) CIDATAPLN
# Counties
ipums_val_labels(cps_data5$CIDATAPLN)
ipums_var_desc(cps_data5, "CIDATAPLN")
colnames(data5_age_categories)

data5_age_cidatapln_yes <- data5_age_categories %>%
  select(YEAR:GQ, CIDATAPLN, PERNUM:AGE_CATE_DES) %>%
  filter(CIDATAPLN != 0) %>%
  filter(CIDATAPLN == 1) %>%
  group_by(CountyFIPS, AGE_CATE) %>%
  summarize(n())%>%
  dplyr::rename(CIDATAPLN_Y = "n()")

data5_age_cidatapln_no <- data5_age_categories %>%
  select(YEAR:GQ, CIDATAPLN, PERNUM:AGE_CATE_DES) %>%
  filter(CIDATAPLN != 0) %>%
  filter(CIDATAPLN == 2) %>%
  group_by(CountyFIPS, AGE_CATE) %>%
  summarize(n())%>%
  dplyr::rename(CIDATAPLN_N = "n()")

data5_age_cidatapln_all = merge(data5_age_cidatapln_yes, data5_age_cidatapln_no, by = c("CountyFIPS", "AGE_CATE"), all.x = TRUE) %>%
  mutate(CIDATAPLN_ALL = CIDATAPLN_Y + CIDATAPLN_N,
         CIDATAPLN_Y_PCT = (CIDATAPLN_Y / CIDATAPLN_ALL)*100) %>%
  mutate_at(vars(CIDATAPLN_Y_PCT), funs(round(., 1)))

data5_age_cidatapln_pct <- data5_age_cidatapln_all %>%
  select(CountyFIPS, AGE_CATE, CIDATAPLN_Y_PCT) %>%
  cast(CountyFIPS~AGE_CATE) %>%
  dplyr::rename(CIDATAPLN_PCT_AGEG1 = "1",
                CIDATAPLN_PCT_AGEG2 = "2",
                CIDATAPLN_PCT_AGEG3 = "3",
                CIDATAPLN_PCT_AGEG4 = "4"
  )

# PUMAS
data5_age_cidatapln_yes2 <- data5_age_categories %>%
  select(YEAR:GQ, CIDATAPLN, PERNUM:AGE_CATE_DES) %>%
  filter(CIDATAPLN != 0) %>%
  filter(CIDATAPLN == 1) %>%
  group_by(PUMACE10, AGE_CATE) %>%
  summarize(n())%>%
  dplyr::rename(CIDATAPLN_Y = "n()")

data5_age_cidatapln_no2 <- data5_age_categories %>%
  select(YEAR:GQ, CIDATAPLN, PERNUM:AGE_CATE_DES) %>%
  filter(CIDATAPLN != 0) %>%
  filter(CIDATAPLN == 2) %>%
  group_by(PUMACE10, AGE_CATE) %>%
  summarize(n())%>%
  dplyr::rename(CIDATAPLN_N = "n()")

data5_age_cidatapln_pct2 = merge(data5_age_cidatapln_yes2, data5_age_cidatapln_no2, by = c("PUMACE10", "AGE_CATE"), all.x = TRUE) %>%
  mutate(CIDATAPLN_ALL = CIDATAPLN_Y + CIDATAPLN_N,
         CIDATAPLN_Y_PCT = (CIDATAPLN_Y / CIDATAPLN_ALL)*100) %>%
  mutate_at(vars(CIDATAPLN_Y_PCT), funs(round(., 1))) %>%
  select(PUMACE10, AGE_CATE, CIDATAPLN_Y_PCT) %>%
  cast(PUMACE10~AGE_CATE) %>%
  dplyr::rename(CIDATAPLN_PCT_AGEG1 = "1",
                CIDATAPLN_PCT_AGEG2 = "2",
                CIDATAPLN_PCT_AGEG3 = "3",
                CIDATAPLN_PCT_AGEG4 = "4"
  )
view(data5_age_cidatapln_pct2)

#  4) CIHISPEED
# Counties
ipums_val_labels(cps_data5$CIHISPEED)
ipums_var_desc(cps_data5, "CIHISPEED")
colnames(data5_age_categories)

data5_age_cihispeed_yes <- data5_age_categories %>%
  select(YEAR:GQ, CIHISPEED, PERNUM:AGE_CATE_DES) %>%
  filter(CIHISPEED != 0) %>%
  mutate(CIHISPEED_SMPL = case_when(
    CIHISPEED == 10 ~ 1,
    CIHISPEED == 11 ~ 1,
    CIHISPEED == 12 ~ 1,
    CIHISPEED == 13 ~ 1,
    CIHISPEED == 14 ~ 1,
    CIHISPEED == 15 ~ 1,
    CIHISPEED == 16 ~ 1,
    CIHISPEED == 17 ~ 1,
    CIHISPEED == 20 ~ 2)) %>%
  filter(CIHISPEED_SMPL == 1) %>%
  group_by(CountyFIPS, AGE_CATE) %>%
  summarize(n())%>%
  dplyr::rename(CIHISPEED_Y = "n()")

data5_age_cihispeed_no <- data5_age_categories %>%
  select(YEAR:GQ, CIHISPEED, PERNUM:AGE_CATE_DES) %>%
  filter(CIHISPEED != 0) %>%
  mutate(CIHISPEED_SMPL = case_when(
    CIHISPEED == 10 ~ 1,
    CIHISPEED == 11 ~ 1,
    CIHISPEED == 12 ~ 1,
    CIHISPEED == 13 ~ 1,
    CIHISPEED == 14 ~ 1,
    CIHISPEED == 15 ~ 1,
    CIHISPEED == 16 ~ 1,
    CIHISPEED == 17 ~ 1,
    CIHISPEED == 20 ~ 2)) %>%
  filter(CIHISPEED_SMPL == 2) %>%
  group_by(CountyFIPS, AGE_CATE) %>%
  summarize(n())%>%
  dplyr::rename(CIHISPEED_N = "n()")

data5_age_cihispeed_all = merge(data5_age_cihispeed_yes, data5_age_cihispeed_no, by = c("CountyFIPS", "AGE_CATE"), all.x = TRUE) %>%
  mutate(CIHISPEED_ALL = CIHISPEED_Y + CIHISPEED_N,
         CIHISPEED_Y_PCT = (CIHISPEED_Y / CIHISPEED_ALL)*100) %>%
  mutate_at(vars(CIHISPEED_Y_PCT), funs(round(., 1)))

data5_age_cihispeed_pct <- data5_age_cihispeed_all %>%
  select(CountyFIPS, AGE_CATE, CIHISPEED_Y_PCT) %>%
  cast(CountyFIPS~AGE_CATE) %>%
  dplyr::rename(CIHISPEED_PCT_AGEG1 = "1",
                CIHISPEED_PCT_AGEG2 = "2",
                CIHISPEED_PCT_AGEG3 = "3",
                CIHISPEED_PCT_AGEG4 = "4"
  )

View(data5_age_cihispeed_pct)

# PUMAs
data5_age_cihispeed_yes2 <- data5_age_categories %>%
  select(YEAR:GQ, CIHISPEED, PERNUM:AGE_CATE_DES) %>%
  filter(CIHISPEED != 0) %>%
  mutate(CIHISPEED_SMPL = case_when(
    CIHISPEED == 10 ~ 1,
    CIHISPEED == 11 ~ 1,
    CIHISPEED == 12 ~ 1,
    CIHISPEED == 13 ~ 1,
    CIHISPEED == 14 ~ 1,
    CIHISPEED == 15 ~ 1,
    CIHISPEED == 16 ~ 1,
    CIHISPEED == 17 ~ 1,
    CIHISPEED == 20 ~ 2)) %>%
  filter(CIHISPEED_SMPL == 1) %>%
  group_by(PUMACE10, AGE_CATE) %>%
  summarize(n())%>%
  dplyr::rename(CIHISPEED_Y = "n()")

data5_age_cihispeed_no2 <- data5_age_categories %>%
  select(YEAR:GQ, CIHISPEED, PERNUM:AGE_CATE_DES) %>%
  filter(CIHISPEED != 0) %>%
  mutate(CIHISPEED_SMPL = case_when(
    CIHISPEED == 10 ~ 1,
    CIHISPEED == 11 ~ 1,
    CIHISPEED == 12 ~ 1,
    CIHISPEED == 13 ~ 1,
    CIHISPEED == 14 ~ 1,
    CIHISPEED == 15 ~ 1,
    CIHISPEED == 16 ~ 1,
    CIHISPEED == 17 ~ 1,
    CIHISPEED == 20 ~ 2)) %>%
  filter(CIHISPEED_SMPL == 2) %>%
  group_by(PUMACE10, AGE_CATE) %>%
  summarize(n())%>%
  dplyr::rename(CIHISPEED_N = "n()")

data5_age_cihispeed_all2 = merge(data5_age_cihispeed_yes2, data5_age_cihispeed_no2, by = c("PUMACE10", "AGE_CATE"), all.x = TRUE) %>%
  mutate(CIHISPEED_ALL = CIHISPEED_Y + CIHISPEED_N,
         CIHISPEED_Y_PCT = (CIHISPEED_Y / CIHISPEED_ALL)*100) %>%
  mutate_at(vars(CIHISPEED_Y_PCT), funs(round(., 1)))

data5_age_cihispeed_pct2 <- data5_age_cihispeed_all2 %>%
  select(PUMACE10, AGE_CATE, CIHISPEED_Y_PCT) %>%
  cast(PUMACE10~AGE_CATE) %>%
  dplyr::rename(CIHISPEED_PCT_AGEG1 = "1",
                CIHISPEED_PCT_AGEG2 = "2",
                CIHISPEED_PCT_AGEG3 = "3",
                CIHISPEED_PCT_AGEG4 = "4"
  )

View(data5_age_cihispeed_pct2)

# B.RACE
# IPUMS_DATA
cps_ddi4 <- read_ipums_ddi("usa_00004.xml")
cps_data4 <- read_ipums_micro(cps_ddi4, verbose = FALSE)

cps_data4$COUNTYFIP = stri_pad_left(cps_data4$COUNTYFIP, 3, "0")
cps_data4$PUMA = stri_pad_left(cps_data4$PUMA, 5, "0")
names(cps_data4)[9] <- "CountyFIPS"
names(cps_data4)[10] <- "PUMACE10"

ipums_val_labels(cps_data4$RACE)
ipums_val_labels(cps_data4$HISPAN)

# First, check the relationship between Race and Hispan
colnames(cps_data4)

cps_data4 %>%
  group_by(RACE, HISPAN) %>%
  summarize(n()) %>%
  cast(HISPAN~RACE)

data4_race_hispan_yes <- cps_data4 %>%
  select(YEAR, SAMPLE:GQ, PERNUM:PERWT, RACE, HISPAN) %>%
    mutate(RACE_SMPL = case_when(
      RACE == 1 | HISPAN == 1 ~ 1,
      RACE == 2 ~ 2,
      RACE == 3 ~ 0,
      )) %>%


View(cps_data4)
colnames(cps_data4)
  
  
data5_age_cidatapln_no2 <- data5_age_categories %>%
  select(YEAR:GQ, CIDATAPLN, PERNUM:AGE_CATE_DES) %>%
  filter(CIDATAPLN != 0) %>%
  filter(CIDATAPLN == 2) %>%
  group_by(PUMACE10, AGE_CATE) %>%
  summarize(n())%>%
  dplyr::rename(CIDATAPLN_N = "n()")





# Join with geographies
# Counties
geodata_age_tech_Counties = list (geo_Counties, data5_age_Counties, data5_age_cinethh_pct, 
                                  data5_age_cismrtphn_pct, data5_age_cidatapln_pct, data5_age_cihispeed_pct) %>%
  reduce(left_join, by = "CountyFIPS")
# PUMAs
geodata_age_tech_PUMAs = list (geo_PUMAs, data5_age_PUMAs, data5_age_cinethh_pct2, 
                               data5_age_cismrtphn_pct2, data5_age_cidatapln_pct2, data5_age_cihispeed_pct2) %>%
  reduce(left_join, by = "PUMACE10")


# Create shapefile
str(geodata_age_tech_Counties)
new_geodata_age_tech_Counties <- st_zm(geodata_age_tech_Counties, drop = T, what = 'ZM')
st_write(new_geodata_age_tech_Counties, 'IPUMS_Age_Tech_Counties_shp.shp')









##############################################################################


# 1. Read IPUMS ddi file
cps_ddi3 <- read_ipums_ddi("usa_00003.xml")
cps_data3 <- read_ipums_micro(cps_ddi3, verbose = FALSE)
cps_ddi4 <- read_ipums_ddi("usa_00004.xml")
cps_data4 <- read_ipums_micro(cps_ddi4, verbose = FALSE)
cps_ddi5 <- read_ipums_ddi("usa_00005.xml")
cps_data5 <- read_ipums_micro(cps_ddi5, verbose = FALSE)



# 2. Explores data
View(cps_data3)
View(cps_data4)
View(cps_data5)

#   2-1. Variable Description
colnames(cps_data3)
ipums_var_desc(cps_data3, "YEAR")
ipums_var_desc(cps_data3, "MULTYEAR")
ipums_var_desc(cps_data3, "SAMPLE")
ipums_var_desc(cps_data3, "SERIAL")
ipums_var_desc(cps_data3, "CBSERIAL")
ipums_var_desc(cps_data3, "HHWT")
ipums_var_desc(cps_data3, "CLUSTER")
ipums_var_desc(cps_data3, "STATEFIP")
ipums_var_desc(cps_data3, "COUNTYFIP")
ipums_var_desc(cps_data3, "PUMA")
ipums_var_desc(cps_data3, "STRATA")
ipums_var_desc(cps_data3, "GQ")
ipums_var_desc(cps_data3, "CINETHH")
ipums_var_desc(cps_data3, "CISMRTPHN")
ipums_var_desc(cps_data3, "CIDATAPLN")
ipums_var_desc(cps_data3, "CIHISPEED")
ipums_var_desc(cps_data3, "PERNUM")
ipums_var_desc(cps_data3, "PERWT")
colnames(cps_data4)
ipums_var_desc(cps_data4, "OWNCOST")
ipums_var_desc(cps_data4, "RENT")
ipums_var_desc(cps_data4, "HHINCOME")
ipums_var_desc(cps_data4, "RELATE")
ipums_var_desc(cps_data4, "RELATED")
ipums_var_desc(cps_data4, "AGE")
ipums_var_desc(cps_data4, "RACE")
ipums_var_desc(cps_data4, "RACED")
ipums_var_desc(cps_data4, "HISPAN")
ipums_var_desc(cps_data4, "HISPAND")
ipums_var_desc(cps_data5, "EDUC")
ipums_var_desc(cps_data4, "EDUCD")
ipums_var_desc(cps_data4, "EMPSTAT")
ipums_var_desc(cps_data4, "EMPSTATD")
ipums_var_desc(cps_data4, "LABFORCE")

#   2-2. Show which variables have labels
cps_data3 %>%
  select_if(is.labelled)
cps_data4 %>%
  select_if(is.labelled)

#   2-3. Investigate variables labels
ipums_val_labels(cps_data3$SAMPLE)
ipums_val_labels(cps_data3$STATEFIP)
ipums_val_labels(cps_data3$COUNTYFIP)
ipums_val_labels(cps_data3$PUMA)
ipums_val_labels(cps_data3$GQ)
ipums_val_labels(cps_data3$CINETHH)
ipums_val_labels(cps_data3$CISMRTPHN)
ipums_val_labels(cps_data3$CIDATAPLN)
ipums_val_labels(cps_data3$CIHISPEED)
ipums_val_labels(cps_data4$OWNCOST)
ipums_val_labels(cps_data4$RENT)
ipums_val_labels(cps_data4$HHINCOME)
ipums_val_labels(cps_data4$RELATE)
ipums_val_labels(cps_data4$RELATED)
ipums_val_labels(cps_data4$AGE)
ipums_val_labels(cps_data4$RACE)
ipums_val_labels(cps_data4$RACED)
ipums_val_labels(cps_data4$HISPAN)
ipums_val_labels(cps_data4$HISPAND)
ipums_val_labels(cps_data4$EDUC)
ipums_val_labels(cps_data4$EDUCD)
ipums_val_labels(cps_data4$EMPSTAT)
ipums_val_labels(cps_data4$EMPSTATD)
ipums_val_labels(cps_data4$LABFORCE)
  
# (Optional) 2-4. If it needs to be created 
cps_data3 <- cps_data3 %>%
  mutate(CINETHH_factor =  as_factor(lbl_clean(CINETHH)),
         CISMRTPHN_factor = as_factor(lbl_clean(CISMRTPHN)),
         CIDATAPLN_factor = as_factor(lbl_clean(CIDATAPLN)),
         CIHISPEED_factor = as_factor(lbl_clean(CIHISPEED))
         )
table(cps_data$STATE_factor, useNA = "always")



# 3. Geographies

#   3-1. Importing Geographies
Geo_counties = st_read("G:/Shared drives/Projects/5035_Tech Equity/Geographies/RPA_counties_shp.shp")
Geo_PUMAs = st_read("G:/Shared drives/Projects/5035_Tech Equity/Geographies/RPA_PUMAs_shp.shp")

#   3-2. Check geometry
plot(st_geometry(Geo_counties))
plot(st_geometry(Geo_PUMAs))


    
# 4. Join

#   4-1. cps_data cleaning
#     1) Counties
# Creating leadning zeros in cps_data$COUNTYFIP, and change column names same as Geo_counties
cps_data3$COUNTYFIP = stri_pad_left(cps_data3$COUNTYFIP, 3, "0") 
cps_data4$COUNTYFIP = stri_pad_left(cps_data4$COUNTYFIP, 3, "0")
names(cps_data3)[9] <- "CountyFIPS"
names(cps_data4)[9] <- "CountyFIPS"

#     2) PUMAs
# Creating leadning zeros in cps_data$, and change column names same as Geo_PUMAs
cps_data3$PUMA = stri_pad_left(cps_data3$PUMA, 5, "0")
cps_data4$PUMA = stri_pad_left(cps_data4$PUMA, 5, "0")
names(cps_data3)[10] <- "PUMACE10"
names(cps_data4)[10] <- "PUMACE10"

#   4-2. Aggregate based on the geographies
#     1) Counties
cps_data3_agg_Counties_tech <- aggregate(list(cps_data3$CINETHH, cps_data3$CISMRTPHN, cps_data3$CIDATAPLN, cps_data3$CIHISPEED), 
            by = list(cps_data3$CountyFIPS),
            FUN = sum, na.rm = TRUE) 
NewNames_Counties_tech <- c('CountyFIPS', 'CINETHH', 'CISMRTPHN', 'CIDATAPLN', 'CIHISPEED')
names(cps_data3_agg_Counties_tech) <- NewNames_Counties
View(cps_data3_agg_Counties_tech)








#     2) PUMAs
cps_data3_agg_PUMAs <- aggregate(list(cps_data3$CINETHH, cps_data3$CISMRTPHN, cps_data3$CIDATAPLN, cps_data3$CIHISPEED), 
                                 by = list(cps_data3$PUMACE10),
                                 FUN = sum, na.rm = TRUE) 
NewNames_PUMAs <- c('PUMACE10', 'CINETHH', 'CISMRTPHN', 'CIDATAPLN', 'CIHISPEED')
names(cps_data3_agg_PUMAs) <- NewNames_PUMAs
View(cps_data3_agg_PUMAs)

#         a. Make a aggregated subset data for new variables with Personal-level: AGE
cps_data4_subset_personal_PUMAs <- aggregate(list(cps_data4$AGE), 
                                                by = list(cps_data4$PUMACE10),
                                                FUN = median, na.rm = TRUE) 
NewNames_personal_PUMAs <- c('PUMACE10', 'AGE')
names(cps_data4_subset_personal_PUMAs) <- NewNames_personal_PUMAs
cps_data4_subset_personal_PUMAs[2] <- lapply(cps_data4_subset_personal_PUMAs[2], round, 0)
View(cps_data4_subset_personal_PUMAs)


#   4-3. Joining the aggregated cps data to gegraphy with geography code
#     1) Counties
cps_data_Counties = merge(Geo_counties, cps_data3_agg_Counties, by = "CountyFIPS", all.x = TRUE)
#         Add Meidan age to the joined table
cps_data_Counties2 = merge(cps_data_Counties, cps_data4_subset_personal_Counties, by = "CountyFIPS", all.x = TRUE)
View(cps_data_Counties2)

#     2)  PUMAs
cps_data_PUMAs = merge(Geo_PUMAs, cps_data3_agg_PUMAs, by = "PUMACE10", all.x = TRUE)
#         Add Meidan age to the joined table
cps_data_PUMAs2 = merge(cps_data_PUMAs, cps_data4_subset_personal_PUMAs, by = "PUMACE10", all.x = TRUE)
View(cps_data_PUMAs2)



# 5. Check & Export
#   5-1. Plotting the outcomes
#     1) Counties
plot(cps_data_Counties[, "CINETHH"], nbreaks = 5, breaks = "pretty")
plot(cps_data_Counties[, "CISMRTPHN"], nbreaks = 5, breaks = "pretty")
plot(cps_data_Counties2[, "AGE"], nbreaks = 5, breaks = "pretty")
#     2) PUMAs
plot(cps_data_PUMAs[, "CIDATAPLN"], nbreaks = 5, breaks = "pretty")
plot(cps_data_PUMAs[, "CIHISPEED"], nbreaks = 5, breaks = "pretty")
plot(cps_data_PUMAs2[, "AGE"], nbreaks = 5, breaks = "pretty")

#   5-2. Exporting data and layers
write.csv(cps_data, file = 'cps_data.csv', color = Species)
write.csv(cps_data_Counties, file = 'IPUMS_Counties_ref.csv')
write.csv(cps_data_PUMAs, file = 'IPUMS_PUMAs_ref.csv')
write.csv(cps_data_Counties2, file = 'IPUMS_Counties2_ref.csv')
write.csv(cps_data_PUMAs2, file = 'IPUMS_PUMAs2_ref.csv')
st_write(cps_data_Counties, "IPUMS_Counties_shp.shp")
st_write(cps_data_PUMAs, "IPUMS_PUMAs_shp.shp")




#################################################################################

colnames(cps_data5_subset_age_hh_cinethh_per)
colnames(cps_data5_subset_age_hh_cinethh_all)
View(cps_data5_subset_age_hh_cinethh_yes)
View(cps_data5_subset_age_hh_cinethh_no)
View(cps_data5_subset_age_hh_cinethh_per)



#     3. TECH VARIABLES
cps_ddi5 <- read_ipums_ddi("usa_00005.xml")
cps_data5 <- read_ipums_micro(cps_ddi5, verbose = FALSE)
view(cps_data5)



# write.csv(cps_data4_subset_personal, file = 'cps_data_person_age.csv')

# 2) Household-level
ipums_val_labels(cps_data4$OWNCOST)
ipums_var_desc(cps_data4, "RENT")
ipums_val_labels(cps_data4$RENT)
ipums_val_labels(cps_data4$HHINCOME)
count_household_owncost <- cps_data4 %>%
  count(OWNCOST)
count_household_rent <- cps_data4 %>%
  count(RENT)
count_household_hhincome <- cps_data4 %>%
  count(HHINCOME)
View(count_household_hhincome)

#   A. Owner Cost 
#   Select only the household (PERNUM = 1) & Remove NA value(99999) in OWNCOST
cps_data4_subset_household_owncost <- cps_data4 %>% 
  select(YEAR,  SAMPLE:OWNCOST, PERNUM, PERWT) %>%
  filter( PERNUM == 1 &
            OWNCOST != 99999
  )
#   1. COUNTIES
#     Aggregate with Counties
cps_data4_agg_Counties_owncost <- aggregate(cps_data4_subset_household_owncost$OWNCOST,
                                            by = list(cps_data4_subset_household_owncost$CountyFIPS),
                                            FUN = median, na.rm = TRUE) %>%
  rename (CountyFIPS = Group.1,
          OWNCOST_MED = x)
#     Join with Counties Geography
cps_data4_Counties_household_owncost = merge(Geo_counties, cps_data4_agg_Counties_owncost, by = "CountyFIPS", all.x = TRUE)
#   2.PUMAs 
#     Aggregate with PUMAs
cps_data4_agg_PUMAs_owncost <- aggregate(cps_data4_subset_household_owncost$OWNCOST,
                                            by = list(cps_data4_subset_household_owncost$PUMACE10),
                                            FUN = median, na.rm = TRUE) %>%
  dplyr::rename (PUMACE10 = Group.1,
          OWNCOST_MED = x)
#     Join with PUMAs Geography
cps_data4_PUMAs_household_owncost = merge(Geo_PUMAs, cps_data4_agg_PUMAs_owncost, by = "PUMACE10", all.x = TRUE)
View(cps_data4_PUMAs_household_owncost)

#   B. Rent 
# Select only the household (PERNUM = 1) & Remove NA value(0) in RENT
cps_data4_subset_household_rent <- cps_data4 %>% 
  select(YEAR,  SAMPLE:GQ, RENT, PERNUM, PERWT) %>%
  filter( PERNUM == 1 &
            RENT != 0
  )
#   1. COUNTIES
#     Aggregate with Counties
cps_data4_agg_Counties_rent <- aggregate(cps_data4_subset_household_rent$RENT,
                                            by = list(cps_data4_subset_household_rent$CountyFIPS),
                                            FUN = median, na.rm = TRUE) %>%
  dplyr::rename (CountyFIPS = Group.1,
          RENT_MED = x)
#     Join with Counties Geography
cps_data4_Counties_household_rent = merge(cps_data4_Counties_household_owncost, cps_data4_agg_Counties_rent, by = "CountyFIPS", all.x = TRUE)
#   2.PUMAs 
#     Aggregate with PUMAs
cps_data4_agg_PUMAs_rent <- aggregate(cps_data4_subset_household_rent$RENT,
                                         by = list(cps_data4_subset_household_rent$PUMACE10),
                                         FUN = median, na.rm = TRUE) %>%
  dplyr::rename (PUMACE10 = Group.1,
          RENT_MED = x)
#     Join with PUMAs Geography
cps_data4_PUMAs_household_rent = merge(cps_data4_PUMAs_household_owncost, cps_data4_agg_PUMAs_rent, by = "PUMACE10", all.x = TRUE)
View(cps_data4_PUMAs_household_rent)


#   C. HHINCOME 
# Select only the household (PERNUM = 1) & Remove NA value(9999999) in HHINCOME
cps_data4_subset_household_hhincome <- cps_data4 %>% 
  select(YEAR,  SAMPLE:GQ, HHINCOME, PERNUM, PERWT) %>%
  filter( PERNUM == 1 &
            HHINCOME != 9999999
  )
#   1. COUNTIES
#     Aggregate with Counties
cps_data4_agg_Counties_hhincome <- aggregate(cps_data4_subset_household_hhincome$HHINCOME,
                                         by = list(cps_data4_subset_household_hhincome$CountyFIPS),
                                         FUN = median, na.rm = TRUE) %>%
  dplyr::rename (CountyFIPS = Group.1,
          HHINCOME_MED = x)
#     Join with Counties Geography
cps_data4_Counties_household_hhincome = merge(cps_data4_Counties_household_rent, cps_data4_agg_Counties_hhincome, by = "CountyFIPS", all.x = TRUE)
View(cps_data4_Counties_household_hhincome)
#   2.PUMAs 
#     Aggregate with PUMAs
cps_data4_agg_PUMAs_hhincome <- aggregate(cps_data4_subset_household_hhincome$HHINCOME,
                                      by = list(cps_data4_subset_household_hhincome$PUMACE10),
                                      FUN = median, na.rm = TRUE) %>%
  rename (PUMACE10 = Group.1,
          HHINCOME_MED = x)
#     Join with PUMAs Geography
cps_data4_PUMAs_household_hhincome = merge(cps_data4_PUMAs_household_rent, cps_data4_agg_PUMAs_hhincome, by = "PUMACE10", all.x = TRUE)
View(cps_data4_PUMAs_household_hhincome)


# 3) Categorical Level

ipums_val_labels(cps_data4$RELATE)
ipums_var_desc(cps_data4, "RELATE")
ipums_val_labels(cps_data4$RACE)
ipums_var_desc(cps_data4, "RACE")
ipums_val_labels(cps_data4$HISPAN)
ipums_var_desc(cps_data4, "HISPAN")
ipums_val_labels(cps_data4$EDUC)
ipums_var_desc(cps_data4, "EDUC")
ipums_val_labels(cps_data4$EMPSTAT)
ipums_var_desc(cps_data4, "EMPSTAT")
ipums_val_labels(cps_data4$LABFORCE)
ipums_var_desc(cps_data4, "LABFORCE")


