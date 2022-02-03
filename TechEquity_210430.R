
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

colnames(cps_data5)

cps_data5$STATEFIP = stri_pad_left(cps_data5$STATEFIP, 2, "0")
cps_data5$COUNTYFIP = stri_pad_left(cps_data5$COUNTYFIP, 3, "0")
cps_data5$PUMA = stri_pad_left(cps_data5$PUMA, 5, "0")

cps_data5 <- cps_data5 %>% 
  mutate(
    CountyFIPS = paste(STATEFIP, COUNTYFIP, sep="")
  ) %>%
  dplyr::rename (PUMACE10 = PUMA
                 )
View(cps_data5)

# 1) Personal-level
# A.AGE
#  1. MEIDAN
#   a. Make a aggregated subset data for new variables with Personal-level: AGE
data5_age_Counties <- aggregate(cps_data5$AGE,
                                by = list(cps_data5$CountyFIPS),
                                FUN = median, na.rm = TRUE) %>% 
  dplyr::rename (CountyFIPS = Group.1,
                 AGE_MED = x)

data5_age_PUMAs <- aggregate(cps_data5$AGE,
                             by = list(cps_data5$PUMACE10),
                             FUN = median, na.rm = TRUE) %>%
  dplyr::rename (PUMACE10 = Group.1,
                 AGE_MED = x)
View(data5_age_Counties)
View(data5_age_PUMAs)

#     2. CATEGORIES
colnames(cps_data5)
data5_age_categories <- cps_data5 %>%
  select(YEAR, SAMPLE:CountyFIPS) %>%
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

colnames(data5_age_categories)
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

data5_age_cinethh_pct = merge(data5_age_cinethh_yes, data5_age_cinethh_no, by = c("CountyFIPS", "AGE_CATE"), all.x = TRUE) %>%
  mutate(CINETHH_ALL = CINETHH_Y + CINETHH_N,
         CINETHH_Y_PER = (CINETHH_Y / CINETHH_ALL)*100) %>%
  mutate_at(vars(CINETHH_Y_PER), funs(round(., 1))) %>%
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

colnames(data5_age_categories)
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

data5_age_cinethh_pct2 = merge(data5_age_cinethh_yes2, data5_age_cinethh_no2, by = c("PUMACE10", "AGE_CATE"), all.x = TRUE) %>%
  mutate(CINETHH_ALL = CINETHH_Y + CINETHH_N,
         CINETHH_Y_PER = (CINETHH_Y / CINETHH_ALL)*100) %>%
  mutate_at(vars(CINETHH_Y_PER), funs(round(., 1))) %>%
  select(PUMACE10, AGE_CATE, CINETHH_Y_PER) %>%
  cast(PUMACE10~AGE_CATE) %>%
  dplyr::rename(CINETHH_PCT_AGEG1 = "1",
                CINETHH_PCT_AGEG2 = "2",
                CINETHH_PCT_AGEG3 = "3",
                CINETHH_PCT_AGEG4 = "4"
  )
View(data5_age_cinethh_pct2)

#  2) CISMRTPHN
# Counties
ipums_val_labels(cps_data5$CISMRTPHN)
ipums_var_desc(cps_data5, "CISMRTPHN")
colnames(data5_age_categories)

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

data5_age_cismrtphn_pct = merge(data5_age_cismrtphn_yes, data5_age_cismrtphn_no, by = c("CountyFIPS", "AGE_CATE"), all.x = TRUE) %>%
  mutate(CISMRTPHN_ALL = CISMRTPHN_Y + CISMRTPHN_N,
         CISMRTPHN_Y_PCT = (CISMRTPHN_Y / CISMRTPHN_ALL)*100) %>%
  mutate_at(vars(CISMRTPHN_Y_PCT), funs(round(., 1))) %>%
  select(CountyFIPS, AGE_CATE, CISMRTPHN_Y_PCT) %>%
  cast(CountyFIPS~AGE_CATE) %>%
  dplyr::rename(CISMRTPHN_PCT_AGEG1 = "1",
                CISMRTPHN_PCT_AGEG2 = "2",
                CISMRTPHN_PCT_AGEG3 = "3",
                CISMRTPHN_PCT_AGEG4 = "4"
  )
View(data5_age_cismrtphn_pct)

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

data5_age_cismrtphn_pct2 = merge(data5_age_cismrtphn_yes2, data5_age_cismrtphn_no2, by = c("PUMACE10", "AGE_CATE"), all.x = TRUE) %>%
  mutate(CISMRTPHN_ALL = CISMRTPHN_Y + CISMRTPHN_N,
         CISMRTPHN_Y_PCT = (CISMRTPHN_Y / CISMRTPHN_ALL)*100) %>%
  mutate_at(vars(CISMRTPHN_Y_PCT), funs(round(., 1))) %>%
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

data5_age_cidatapln_pct = merge(data5_age_cidatapln_yes, data5_age_cidatapln_no, by = c("CountyFIPS", "AGE_CATE"), all.x = TRUE) %>%
  mutate(CIDATAPLN_ALL = CIDATAPLN_Y + CIDATAPLN_N,
         CIDATAPLN_Y_PCT = (CIDATAPLN_Y / CIDATAPLN_ALL)*100) %>%
  mutate_at(vars(CIDATAPLN_Y_PCT), funs(round(., 1))) %>%
  select(CountyFIPS, AGE_CATE, CIDATAPLN_Y_PCT) %>%
  cast(CountyFIPS~AGE_CATE) %>%
  dplyr::rename(CIDATAPLN_PCT_AGEG1 = "1",
                CIDATAPLN_PCT_AGEG2 = "2",
                CIDATAPLN_PCT_AGEG3 = "3",
                CIDATAPLN_PCT_AGEG4 = "4"
  )
View(data5_age_cidatapln_pct)

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

data5_age_cihispeed_pct = merge(data5_age_cihispeed_yes, data5_age_cihispeed_no, by = c("CountyFIPS", "AGE_CATE"), all.x = TRUE) %>%
  mutate(CIHISPEED_ALL = CIHISPEED_Y + CIHISPEED_N,
         CIHISPEED_Y_PCT = (CIHISPEED_Y / CIHISPEED_ALL)*100) %>%
  mutate_at(vars(CIHISPEED_Y_PCT), funs(round(., 1))) %>%
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

data5_age_cihispeed_pct2 = merge(data5_age_cihispeed_yes2, data5_age_cihispeed_no2, by = c("PUMACE10", "AGE_CATE"), all.x = TRUE) %>%
  mutate(CIHISPEED_ALL = CIHISPEED_Y + CIHISPEED_N,
         CIHISPEED_Y_PCT = (CIHISPEED_Y / CIHISPEED_ALL)*100) %>%
  mutate_at(vars(CIHISPEED_Y_PCT), funs(round(., 1))) %>%
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


cps_data4$STATEFIP = stri_pad_left(cps_data4$STATEFIP, 2, "0")
cps_data4$COUNTYFIP = stri_pad_left(cps_data4$COUNTYFIP, 3, "0")
cps_data4$PUMA = stri_pad_left(cps_data4$PUMA, 5, "0")

cps_data4 <- cps_data4 %>% 
  mutate(
    CountyFIPS = paste(STATEFIP, COUNTYFIP, sep="")
  ) %>%
  dplyr::rename (PUMACE10 = PUMA
  )
View(cps_data4)

# First, check the relationship between Race and Hispan
ipums_val_labels(cps_data4$RACE)
ipums_val_labels(cps_data4$HISPAN)

cps_data4 %>%
  group_by(RACE, HISPAN) %>%
  summarize(n()) %>%
  cast(HISPAN~RACE)
colnames(cps_data4)

# Counties
data4_race_hisp_pct <- cps_data4 %>%
  select(YEAR, SAMPLE:GQ, PERNUM:PERWT, RACE, HISPAN, CountyFIPS) %>%
    mutate(RACE_SMPL = case_when(
      RACE == 1 ~ 1, # 1. WHITE
      RACE == 2 ~ 2, # 2. AFRICAN-AMERICAN
      RACE == 3 ~ 3, # 3. AMERICAN INDIAN OR ALASKA NATIVE
      RACE == 4 ~ 4, # 4. ASIAN
      RACE == 5 ~ 4,
      RACE == 6 ~ 4, 
      RACE == 7 ~ 5, # 5. OTHER RACE
      RACE == 8 ~ 5,
      RACE == 9 ~ 5), 
      RACE_HISP_SMPL = case_when(
        HISPAN == 0 & RACE_SMPL == 1 ~ 1,
        HISPAN == 0 & RACE_SMPL == 2 ~ 2,
        HISPAN == 0 & RACE_SMPL == 3 ~ 3,
        HISPAN == 0 & RACE_SMPL == 4 ~ 4,
        HISPAN == 0 & RACE_SMPL == 5 ~ 5,
        HISPAN == 1 | HISPAN == 2 | HISPAN == 3 | HISPAN == 4 ~ 6), # 6. HISPANIC
      ) %>%
  group_by(CountyFIPS, RACE_HISP_SMPL) %>%
  summarize(n()) %>%
  cast(CountyFIPS~RACE_HISP_SMPL) %>%
  dplyr::rename(
    RACE_WHT_NUM = "1",
    RACE_AFA_NUM = "2",
    RACE_AIN_NUM = "3",
    RACE_ASN_NUM = "4",
    RACE_OTH_NUM = "5",
    RACE_HSP_NUM = "6"
    ) %>%
  replace_na(list(
    RACE_WHT_NUM = 0,
    RACE_AFA_NUM = 0,
    RACE_AIN_NUM = 0,
    RACE_ASN_NUM = 0,
    RACE_OTH_NUM = 0,
    RACE_HSP_NUM = 0
    )) %>%
  mutate(
      RACE_TTL_NUM = RACE_WHT_NUM + RACE_AFA_NUM + RACE_AIN_NUM + RACE_ASN_NUM + RACE_OTH_NUM + RACE_HSP_NUM, 
      RACE_WHT_PCT = (RACE_WHT_NUM / RACE_TTL_NUM)*100, 
      RACE_AFA_PCT = (RACE_AFA_NUM / RACE_TTL_NUM)*100,
      RACE_AIN_PCT = (RACE_AIN_NUM / RACE_TTL_NUM)*100,
      RACE_ASN_PCT = (RACE_ASN_NUM / RACE_TTL_NUM)*100,
      RACE_OTH_PCT = (RACE_OTH_NUM / RACE_TTL_NUM)*100,
      RACE_HSP_PCT = (RACE_HSP_NUM / RACE_TTL_NUM)*100
  ) %>%
  mutate(across(9:14, round, 1))

# PUMAs
data4_race_hisp_pct2 <- cps_data4 %>%
  select(YEAR, SAMPLE:GQ, PERNUM:PERWT, RACE, HISPAN, CountyFIPS) %>%
  mutate(RACE_SMPL = case_when(
    RACE == 1 ~ 1, # 1. WHITE
    RACE == 2 ~ 2, # 2. AFRICAN-AMERICAN
    RACE == 3 ~ 3, # 3. AMERICAN INDIAN OR ALASKA NATIVE
    RACE == 4 ~ 4, # 4. ASIAN
    RACE == 5 ~ 4,
    RACE == 6 ~ 4, 
    RACE == 7 ~ 5, # 5. OTHER RACE
    RACE == 8 ~ 5,
    RACE == 9 ~ 5), 
    RACE_HISP_SMPL = case_when(
      HISPAN == 0 & RACE_SMPL == 1 ~ 1,
      HISPAN == 0 & RACE_SMPL == 2 ~ 2,
      HISPAN == 0 & RACE_SMPL == 3 ~ 3,
      HISPAN == 0 & RACE_SMPL == 4 ~ 4,
      HISPAN == 0 & RACE_SMPL == 5 ~ 5,
      HISPAN == 1 | HISPAN == 2 | HISPAN == 3 | HISPAN == 4 ~ 6), # 6. HISPANIC
  ) %>%
  group_by(PUMACE10, RACE_HISP_SMPL) %>%
  summarize(n()) %>%
  cast(PUMACE10~RACE_HISP_SMPL) %>%
  dplyr::rename(
    RACE_WHT_NUM = "1",
    RACE_AFA_NUM = "2",
    RACE_AIN_NUM = "3",
    RACE_ASN_NUM = "4",
    RACE_OTH_NUM = "5",
    RACE_HSP_NUM = "6"
  ) %>%
  replace_na(list(
    RACE_WHT_NUM = 0,
    RACE_AFA_NUM = 0,
    RACE_AIN_NUM = 0,
    RACE_ASN_NUM = 0,
    RACE_OTH_NUM = 0,
    RACE_HSP_NUM = 0
  )) %>%
  mutate(
    RACE_TTL_NUM = RACE_WHT_NUM + RACE_AFA_NUM + RACE_AIN_NUM + RACE_ASN_NUM + RACE_OTH_NUM + RACE_HSP_NUM, 
    RACE_WHT_PCT = (RACE_WHT_NUM / RACE_TTL_NUM)*100, 
    RACE_AFA_PCT = (RACE_AFA_NUM / RACE_TTL_NUM)*100,
    RACE_AIN_PCT = (RACE_AIN_NUM / RACE_TTL_NUM)*100,
    RACE_ASN_PCT = (RACE_ASN_NUM / RACE_TTL_NUM)*100,
    RACE_OTH_PCT = (RACE_OTH_NUM / RACE_TTL_NUM)*100,
    RACE_HSP_PCT = (RACE_HSP_NUM / RACE_TTL_NUM)*100
  ) %>%
  mutate(across(9:14, round, 1))

View(data4_race_hisp_pct)
View(data4_race_hisp_pct2)

# C.EDUC
ipums_val_labels(cps_data4$EDUC)
colnames(cps_data4)

# Counties
data4_educ_pct <- cps_data4 %>%
  select(YEAR, SAMPLE:GQ, PERNUM:PERWT, EDUC, CountyFIPS) %>%
  filter(EDUC != 0) %>%
  mutate(EDUC_SMPL = case_when(
    EDUC == 1 ~ 1, # No High School Diploma
    EDUC == 2 ~ 1, 
    EDUC == 3 ~ 1, 
    EDUC == 4 ~ 1, 
    EDUC == 5 ~ 1,
    EDUC == 6 ~ 2, # High School Diploma
    EDUC == 7 ~ 3, # Some College
    EDUC == 8 ~ 3,
    EDUC == 9 ~ 3, 
    EDUC == 10 ~ 4, # College Degree
    EDUC == 11 ~ 4)
    ) %>%
  group_by(CountyFIPS, EDUC_SMPL) %>%
  summarize(n()) %>%
  cast(CountyFIPS~EDUC_SMPL) %>%
  dplyr::rename(
    EDUC_NHS_NUM = "1",
    EDUC_YHS_NUM = "2",
    EDUC_SCD_NUM = "3",
    EDUC_CLD_NUM = "4"
  ) %>%
  mutate(
    EDUC_TTL_NUM = EDUC_NHS_NUM + EDUC_YHS_NUM + EDUC_SCD_NUM + EDUC_CLD_NUM,
    EDUC_NHS_PCT = (EDUC_NHS_NUM / EDUC_TTL_NUM)*100,
    EDUC_YHS_PCT = (EDUC_YHS_NUM / EDUC_TTL_NUM)*100,
    EDUC_SCD_PCT = (EDUC_SCD_NUM / EDUC_TTL_NUM)*100,
    EDUC_CLD_PCT = (EDUC_CLD_NUM / EDUC_TTL_NUM)*100
  ) %>%
  mutate(across(7:10, round, 1))
View(data4_educ_pct)

# PUMAs
data4_educ_pct2 <- cps_data4 %>%
  select(YEAR, SAMPLE:GQ, PERNUM:PERWT, EDUC, CountyFIPS) %>%
  filter(EDUC != 0) %>%
  mutate(EDUC_SMPL = case_when(
    EDUC == 1 ~ 1, # No High School Diploma
    EDUC == 2 ~ 1, 
    EDUC == 3 ~ 1, 
    EDUC == 4 ~ 1, 
    EDUC == 5 ~ 1,
    EDUC == 6 ~ 2, # High School Diploma
    EDUC == 7 ~ 3, # Some College
    EDUC == 8 ~ 3,
    EDUC == 9 ~ 3, 
    EDUC == 10 ~ 4, # College Degree
    EDUC == 11 ~ 4)
  ) %>%
  group_by(PUMACE10, EDUC_SMPL) %>%
  summarize(n()) %>%
  cast(PUMACE10~EDUC_SMPL) %>%
  dplyr::rename(
    EDUC_NHS_NUM = "1",
    EDUC_YHS_NUM = "2",
    EDUC_SCD_NUM = "3",
    EDUC_CLD_NUM = "4"
  ) %>%
  mutate(
    EDUC_TTL_NUM = EDUC_NHS_NUM + EDUC_YHS_NUM + EDUC_SCD_NUM + EDUC_CLD_NUM,
    EDUC_NHS_PCT = (EDUC_NHS_NUM / EDUC_TTL_NUM)*100,
    EDUC_YHS_PCT = (EDUC_YHS_NUM / EDUC_TTL_NUM)*100,
    EDUC_SCD_PCT = (EDUC_SCD_NUM / EDUC_TTL_NUM)*100,
    EDUC_CLD_PCT = (EDUC_CLD_NUM / EDUC_TTL_NUM)*100
  ) %>%
  mutate(across(7:10, round, 1))

# D.EMPSTAT
ipums_val_labels(cps_data4$EMPSTAT)
colnames(cps_data4)

# Counties
data4_emps_pct <- cps_data4 %>%
  select(YEAR, SAMPLE:GQ, PERNUM:PERWT, EMPSTAT, CountyFIPS) %>%
  filter(EMPSTAT != 0) %>%
  filter(EMPSTAT != 3) %>%
  group_by(CountyFIPS, EMPSTAT) %>%
  summarize(n()) %>%
  cast(CountyFIPS ~ EMPSTAT) %>%
  dplyr::rename(
    EMPS_EMP_NUM = "1", # EMPLOYED
    EMPS_UNE_NUM = "2", # UNEMPLOYED
  ) %>%
  mutate(
    EMPS_TTL_NUM = EMPS_EMP_NUM + EMPS_UNE_NUM,
    EMPS_EMP_PCT = (EMPS_EMP_NUM / EMPS_TTL_NUM) * 100,
    EMPS_UNE_PCT = (EMPS_UNE_NUM / EMPS_TTL_NUM) * 100
  ) %>%
  mutate(across(5:6, round, 1))
View(data4_emps_pct)

# PUMAs
data4_emps_pct2 <-cps_data4 %>%
  select(YEAR, SAMPLE:GQ, PERNUM:PERWT, EMPSTAT, CountyFIPS) %>%
  filter(EMPSTAT != 0) %>%
  filter(EMPSTAT != 3) %>%
  group_by(PUMACE10, EMPSTAT) %>%
  summarize(n()) %>%
  cast(PUMACE10 ~ EMPSTAT) %>%
  dplyr::rename(
    EMPS_EMP_NUM = "1", # EMPLOYED
    EMPS_UNE_NUM = "2" # UNEMPLOYED
  ) %>%
  mutate(
    EMPS_TTL_NUM = EMPS_EMP_NUM + EMPS_UNE_NUM,
    EMPS_EMP_PCT = (EMPS_EMP_NUM / EMPS_TTL_NUM) * 100,
    EMPS_UNE_PCT = (EMPS_UNE_NUM / EMPS_TTL_NUM) * 100
  ) %>%
  mutate(across(5:6, round, 1))
View(data4_emps_pct2)

# E.LABFORCE
ipums_val_labels(cps_data4$LABFORCE)

# Counties
data4_labf_pct <- cps_data4 %>%
  select(YEAR, SAMPLE:GQ, PERNUM:PERWT, LABFORCE, CountyFIPS) %>%
  filter(LABFORCE != 0) %>%
  group_by(CountyFIPS, LABFORCE) %>%
  summarize(n()) %>%
  cast(CountyFIPS ~ LABFORCE) %>%
  dplyr::rename(
    LABF_YLF_NUM = "2", # IN THE LABOR FORCE
    LABF_NLF_NUM = "1" # NOT IN THE LABOR FORCE
  ) %>%
  mutate(
    LABF_TTL_NUM = LABF_YLF_NUM + LABF_NLF_NUM,
    LABF_YLF_PCT = (LABF_YLF_NUM/LABF_TTL_NUM)*100,
    LABF_NLF_PCT = (LABF_NLF_NUM/LABF_TTL_NUM)*100
  ) %>%
  mutate(across(5:6, round, 1))
View(data4_labf_pct)

# PUMAs
data4_labf_pct2 <- cps_data4 %>%
  select(YEAR, SAMPLE:GQ, PERNUM:PERWT, LABFORCE, CountyFIPS) %>%
  filter(LABFORCE != 0) %>%
  group_by(PUMACE10, LABFORCE) %>%
  summarize(n()) %>%
  cast(PUMACE10 ~ LABFORCE) %>%
  dplyr::rename(
    LABF_YLF_NUM = "2", # IN THE LABOR FORCE
    LABF_NLF_NUM = "1" # NOT IN THE LABOR FORCE
  ) %>%
  mutate(
    LABF_TTL_NUM = LABF_YLF_NUM + LABF_NLF_NUM,
    LABF_YLF_PCT = (LABF_YLF_NUM/LABF_TTL_NUM)*100,
    LABF_NLF_PCT = (LABF_NLF_NUM/LABF_TTL_NUM)*100
  ) %>%
  mutate(across(5:6, round, 1))
View(data4_labf_pct2)


# 2) Household-level
# A.Household Income - MEDIAN
#   a. Make a aggregated subset data for new variables with Personal-level: AGE
colnames(cps_data4)
ipums_val_labels(cps_data4$HHINCOME)
ipums_var_desc(cps_data4, "PERNUM")
headTail(cps_data4)

# Counties
data4_hhic_med <- cps_data4 %>% 
  select(YEAR, SAMPLE:GQ, PERNUM:PERWT, HHINCOME, CountyFIPS) %>%
  filter(PERNUM == 1) %>%
  filter(HHINCOME != 9999999) %>%
  group_by(CountyFIPS) %>%
  summarize(HHIC_MED = median(HHINCOME))

# PUMAs
data4_hhic_med2 <- cps_data4 %>% 
  select(YEAR, SAMPLE:GQ, PERNUM:PERWT, HHINCOME) %>%
  filter(PERNUM == 1) %>%
  filter(HHINCOME != 9999999) %>%
  group_by(PUMACE10) %>%
  summarize(HHIC_MED = median(HHINCOME))



# B.Owner Cost & Rent - Median
ipums_val_labels(cps_data4$OWNCOST)
ipums_val_labels(cps_data4$RENT)
colnames(cps_data4)

# Counties
data4_ownc_rent_med_Counties <- cps_data4 %>% 
  select(YEAR, SAMPLE:GQ, PERNUM:PERWT, OWNCOST, RENT, CountyFIPS) %>%
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
                                  data4_hhic_med, data4_ownc_rent_med_Counties) %>%
  reduce(left_join, by = "CountyFIPS")
# PUMAs
geodata_age_tech_PUMAs = list (geo_PUMAs, data5_age_PUMAs, data5_age_cinethh_pct2, 
                               data5_age_cismrtphn_pct2, data5_age_cidatapln_pct2, data5_age_cihispeed_pct2,
                               data4_race_hisp_pct2, data4_educ_pct2, data4_emps_pct2, data4_labf_pct2,
                               data4_hhic_med2, data4_ownc_rent_med_PUMAs) %>%
  reduce(left_join, by = "PUMACE10")

View(geodata_age_tech_Counties)
View(geodata_age_tech_PUMAs)

# Write CSV
write.csv(geodata_age_tech_Counties, file = 'IPUMS_Counties_csv.csv')
write.csv(geodata_age_tech_PUMAs, file = 'IPUMS_PUMAs_csv.csv')

geodata_age_tech_Counties2 <- st_collection_extract(geodata_age_tech_Counties, "POLYGON")
st_write(geodata_age_tech_Counties2, "IPUMS_Counties_shp.shp")






# Create Charts with Highchart
data_Counties = list (data5_age_Counties, data5_age_cinethh_pct, data5_age_cismrtphn_pct, 
                      data5_age_cidatapln_pct, data5_age_cihispeed_pct, data4_race_hisp_pct, 
                      data4_educ_pct, data4_emps_pct, data4_labf_pct, data4_hhic_med, 
                      data4_ownc_rent_med_Counties) %>%
  reduce(left_join, by = "CountyFIPS")
data_PUMAs = list (data5_age_PUMAs, data5_age_cinethh_pct2, data5_age_cismrtphn_pct2, 
                      data5_age_cidatapln_pct2, data5_age_cihispeed_pct2, data4_race_hisp_pct2, 
                      data4_educ_pct2, data4_emps_pct2, data4_labf_pct2, data4_hhic_med2, 
                   data4_ownc_rent_med_PUMAs) %>%
  reduce(left_join, by = "PUMACE10")
View(data_Counties)
View(data_PUMAs)

write.csv(data_Counties, file = 'IPUMS_Counties_csv.csv')
write.csv(data_PUMAs, file = 'IPUMS_PUMAs_csv.csv')


# Chart
# 1) Counties
# Example 1 - Access to Internet (CINETHH_PCT)
data_Counties_Example1 <- data_Counties %>%
  select(HHIC_MED, CINETHH_PCT_AGEG1, CINETHH_PCT_AGEG2, CINETHH_PCT_AGEG3, CINETHH_PCT_AGEG4) %>%
  dplyr::rename(Age_Under18 = "CINETHH_PCT_AGEG1",
                Age_18to39 = "CINETHH_PCT_AGEG2",
                Age_40to64 = "CINETHH_PCT_AGEG3",
                Age_65over = "CINETHH_PCT_AGEG4")

data_Counties_Example1_long <- gather(data_Counties_Example1, Condition, Percentage, 
                                      Age_Under18:Age_65over, factor_key=TRUE)

data_Counties_Example1_long %>%
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
  hc_subtitle(text = "Relation with Median Household Income by Counties in Tri-State Region")

# Example 2 - AcceSs to Smartphone (CISMRTPHN) 
data_Counties_Example2 <- data_Counties %>%
  select(HHIC_MED, CISMRTPHN_PCT_AGEG1, CISMRTPHN_PCT_AGEG2, CISMRTPHN_PCT_AGEG3, CISMRTPHN_PCT_AGEG4) %>%
  dplyr::rename(Age_Under18 = "CISMRTPHN_PCT_AGEG1",
                Age_18to39 = "CISMRTPHN_PCT_AGEG2",
                Age_40to64 = "CISMRTPHN_PCT_AGEG3",
                Age_65over = "CISMRTPHN_PCT_AGEG4")

data_Counties_Example2_long <- gather(data_Counties_Example2, Condition, Percentage, 
                                      Age_Under18:Age_65over, factor_key=TRUE)
head(data_Counties_Example2_long)

data_Counties_Example2_long %>%
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
  hc_subtitle(text = "Relationship with Median Household Income by Counties in Tri-State Region")

# Example 3 - AcceSs to Phone with Data Plan (CIDATAPLN) 
data_Counties_Example3 <- data_Counties %>%
  select(HHIC_MED, CIDATAPLN_PCT_AGEG1, CIDATAPLN_PCT_AGEG2, CIDATAPLN_PCT_AGEG3, CIDATAPLN_PCT_AGEG4) %>%
  dplyr::rename(Age_Under18 = "CIDATAPLN_PCT_AGEG1",
                Age_18to39 = "CIDATAPLN_PCT_AGEG2",
                Age_40to64 = "CIDATAPLN_PCT_AGEG3",
                Age_65over = "CIDATAPLN_PCT_AGEG4")

data_Counties_Example3_long <- gather(data_Counties_Example3, Condition, Percentage, 
                                      Age_Under18:Age_65over, factor_key=TRUE)

data_Counties_Example3_long %>%
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
  hc_subtitle(text = "Relationship with Median Household Income by Counties in Tri-State Region")

# Example 4 - AcceSs to Broadband (CIHISPEED) 
data_Counties_Example4 <- data_Counties %>%
  select(HHIC_MED, CIHISPEED_PCT_AGEG1, CIHISPEED_PCT_AGEG2, CIHISPEED_PCT_AGEG3, CIHISPEED_PCT_AGEG4) %>%
  dplyr::rename(Age_Under18 = "CIHISPEED_PCT_AGEG1",
                Age_18to39 = "CIHISPEED_PCT_AGEG2",
                Age_40to64 = "CIHISPEED_PCT_AGEG3",
                Age_65over = "CIHISPEED_PCT_AGEG4")

data_Counties_Example4_long <- gather(data_Counties_Example4, Condition, Percentage, 
                                      Age_Under18:Age_65over, factor_key=TRUE)

data_Counties_Example4_long %>%
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
  hc_subtitle(text = "Relationship with Median Household Income by Counties in Tri-State Region")

# 2) PUMAs
# Example 1 - Access to Internet (CINETHH_PCT)
data_PUMAs_Example1 <- data_PUMAs %>%
  select(HHIC_MED, CINETHH_PCT_AGEG1, CINETHH_PCT_AGEG2, CINETHH_PCT_AGEG3, CINETHH_PCT_AGEG4) %>%
  dplyr::rename(Age_Under18 = "CINETHH_PCT_AGEG1",
                Age_18to39 = "CINETHH_PCT_AGEG2",
                Age_40to64 = "CINETHH_PCT_AGEG3",
                Age_65over = "CINETHH_PCT_AGEG4")

data_PUMAs_Example1_long <- gather(data_PUMAs_Example1, Condition, Percentage, 
                                      Age_Under18:Age_65over, factor_key=TRUE)

data_PUMAs_Example1_long %>%
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
  hc_subtitle(text = "Relation with Median Household Income by PUMAs in Tri-State Region")

# Example 2 - AcceSs to Smartphone (CISMRTPHN) 
data_PUMAs_Example2 <- data_PUMAs %>%
  select(HHIC_MED, CISMRTPHN_PCT_AGEG1, CISMRTPHN_PCT_AGEG2, CISMRTPHN_PCT_AGEG3, CISMRTPHN_PCT_AGEG4) %>%
  dplyr::rename(Age_Under18 = "CISMRTPHN_PCT_AGEG1",
                Age_18to39 = "CISMRTPHN_PCT_AGEG2",
                Age_40to64 = "CISMRTPHN_PCT_AGEG3",
                Age_65over = "CISMRTPHN_PCT_AGEG4")

data_PUMAs_Example2_long <- gather(data_PUMAs_Example2, Condition, Percentage, 
                                      Age_Under18:Age_65over, factor_key=TRUE)

data_PUMAs_Example2_long %>%
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

# Example 3 - AcceSs to Phone with Data Plan (CIDATAPLN) 
data_PUMAs_Example3 <- data_PUMAs %>%
  select(HHIC_MED, CIDATAPLN_PCT_AGEG1, CIDATAPLN_PCT_AGEG2, CIDATAPLN_PCT_AGEG3, CIDATAPLN_PCT_AGEG4) %>%
  dplyr::rename(Age_Under18 = "CIDATAPLN_PCT_AGEG1",
                Age_18to39 = "CIDATAPLN_PCT_AGEG2",
                Age_40to64 = "CIDATAPLN_PCT_AGEG3",
                Age_65over = "CIDATAPLN_PCT_AGEG4")

data_PUMAs_Example3_long <- gather(data_PUMAs_Example3, Condition, Percentage, 
                                      Age_Under18:Age_65over, factor_key=TRUE)

data_PUMAs_Example3_long %>%
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

# Example 4 - AcceSs to Broadband (CIHISPEED) 
data_PUMAs_Example4 <- data_PUMAs %>%
  select(HHIC_MED, CIHISPEED_PCT_AGEG1, CIHISPEED_PCT_AGEG2, CIHISPEED_PCT_AGEG3, CIHISPEED_PCT_AGEG4) %>%
  dplyr::rename(Age_Under18 = "CIHISPEED_PCT_AGEG1",
                Age_18to39 = "CIHISPEED_PCT_AGEG2",
                Age_40to64 = "CIHISPEED_PCT_AGEG3",
                Age_65over = "CIHISPEED_PCT_AGEG4")

data_PUMAs_Example4_long <- gather(data_PUMAs_Example4, Condition, Percentage, 
                                      Age_Under18:Age_65over, factor_key=TRUE)

data_PUMAs_Example4_long %>%
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

# Extra Search - 2-4) Pumas & Broadband
colnames(data_PUMAs)
head(data_PUMAs)

data_PUMAs_interests <-data_PUMAs %>%
  select(PUMACE10, CIHISPEED_PCT_AGEG1, CIHISPEED_PCT_AGEG2, 
         CIHISPEED_PCT_AGEG3, CIHISPEED_PCT_AGEG4, HHIC_MED) %>%
  filter(CIHISPEED_PCT_AGEG1 < 75 | CIHISPEED_PCT_AGEG2 < 75 | 
           CIHISPEED_PCT_AGEG3 < 75 | CIHISPEED_PCT_AGEG4 < 75) %>%
  filter(HHIC_MED < 67000)

data_PUMAs_interests2 <-data_PUMAs %>%
  select(PUMACE10, CIHISPEED_PCT_AGEG1, CIHISPEED_PCT_AGEG2, 
         CIHISPEED_PCT_AGEG3, CIHISPEED_PCT_AGEG4, HHIC_MED) %>%
  filter(CIHISPEED_PCT_AGEG1 < 75) %>%
  filter(CIHISPEED_PCT_AGEG2 < 75) %>%
  filter(CIHISPEED_PCT_AGEG3 < 75) %>%
  filter(CIHISPEED_PCT_AGEG4 < 75) %>%
  filter(HHIC_MED < 67000)

View(data_PUMAs_interests)
View(data_PUMAs_interests2)

colnames(geo_PUMAs)
colnames(data_PUMAs_interests)
head(data_PUMAs_interests$PUMACE10)
View(data_PUMAs_interests)
View(geo_PUMAs)
View(data_PUMAs_interests2)

data_PUMAs_interests <- geo_PUMAs %>%
  filter(PUMACE10 == "00403" | PUMACE10 == "00704" | PUMACE10 == "02402" | PUMACE10 == "02500" | PUMACE10 == "04002")
# Only one PUMA Selected? WHY?????
# -> In the analysis data, there are some PUMAs that are not included in the RPA regions
# Total Number of PUMAs in All Tri-States = 199
# Total Number of PUMAs in RPA Tri-State Region = 170
# But, there is one PUMA in RPA region: 04002 in Brooklyn
View(data_PUMAs_interests)

plot(cps_data_Counties[, "CINETHH"], nbreaks = 5, breaks = "pretty")



# 05/03/2021    Create Maps with Highchart 
# Import RPA Json File
rpa_county_json <- jsonlite::fromJSON(txt = "RPA_counties_shp.json",simplifyVector = FALSE) %>%
  geojsonio::as.json()

highchart(type = "map") %>%
  hc_add_series(mapData = rpa_county_json, showInLegend = FALSE)






# Create shapefile
str(geodata_age_tech_Counties)
new_geodata_age_tech_Counties <- st_zm(geodata_age_tech_Counties, drop = T, what = 'ZM')

st_write(new_geodata_age_tech_Counties, 'IPUMS_Age_Tech_Counties_shp.shp')



#################################################################################

# 04-30-2021
# Highchart Practice

# 1. Create Data
symbols <- 
  c("SPY","EFA", "IJS", "EEM","AGG")

prices <- 
  getSymbols(symbols, 
             src = 'yahoo', 
             from = "2013-01-01",
             to = "2017-12-31",
             auto.assign = TRUE, 
             warnings = FALSE) %>% 
  map(~Ad(get(.))) %>%
  reduce(merge) %>% 
  `colnames<-`(symbols)

prices_monthly <- 
  to.monthly(prices, 
             indexAt = "last", 
             OHLC = FALSE)

asset_returns_xts <- 
  na.omit(Return.calculate(prices_monthly, 
                           method = "log"))

asset_returns_xts <- asset_returns_xts * 100


asset_returns_long <-  
  prices %>% 
  to.monthly(indexAt = "last", 
             OHLC = FALSE) %>% 
  tk_tbl(preserve_index = TRUE, 
         rename_index = "date") %>%
  gather(asset, returns, -date) %>% 
  group_by(asset) %>%  
  mutate(returns = 
           (log(returns) - log(lag(returns))) *100
  ) %>% 
  na.omit()

# 2. Creating a chart
#   1) asset_returns_xts (Wide data)
headTail(asset_returns_xts)

# Line Chart
highchart(type = "stock") %>% # Create navigator or date selector
  hc_add_series(asset_returns_xts$SPY,
                type = "line",
                color = "green")

# Column Chart
highchart(type = "stock") %>%
  hc_add_series(asset_returns_xts$SPY,
                type = "column",
                color = "red")

# Scatter Plot
highchart(type = "stock") %>%
  hc_add_series(asset_returns_xts$SPY, type = "scatter", 
                name = "SPY", color = "lightblue") %>%
  hc_add_series(asset_returns_xts$EFA, type = "scatter", 
                name = "EFA", color = "pink") %>%
  hc_add_series(asset_returns_xts$IJS, type = "scatter", 
                name = "IJS", color =  "lightgreen") %>%
  hc_tooltip(pointFormat = '{point.x: %Y-%m-%d},
                            {point.y:.4f}%')

#   2) asset_returns_xts (Long data)
headTail(asset_returns_long)

# Line Chart
asset_returns_long %>%
  filter(asset == "SPY") %>%
  hchart(.,
         type = "line",
         hcaes(x = date,
               y = returns)) %>%
  hc_yAxis(opposite = TRUE,
           labels = list(format = "{value}%"))

# Column Chart
asset_returns_long %>%
  filter(asset == "SPY") %>%
  hchart(.,
         type = "column",
         hcaes(x = date,
               y = returns), color = "green") %>%
  hc_yAxis(opposite = TRUE,
           labels = list(format = "{value}%"))

# Scatter Plot
asset_returns_long %>%
  filter(asset == "SPY") %>%
  hchart(.,
         type = "scatter",
         hcaes(x = date,
               y = returns), color = "pink") %>%
  hc_yAxis(opposite = TRUE,
           labels = list(format = "{value}%")) %>%
  hc_tooltip(pointFormat = '{point.x: %Y-%m-%d},
                            {point.y:.4f}%')

# Advanced 1
asset_returns_long %>%
  hchart(.,
         type = "scatter",
         hcaes(x = date,
               y = returns,
               group = asset)) %>%
  hc_yAxis(opposite = TRUE,
           labels = list(format = "{value}%")) %>%
  hc_tooltip(pointFormat = '{pointx:%Y-%m-%d}
                            {point.y:.4f}%')

# Advanced 2
asset_returns_long %>%
  filter(date >= "2017-01-01" & date < "2018-01-01") %>%
  hchart(., type = "column",
         hcaes(x = date,
               y = returns,
               group = asset)) %>%
  hc_yAxis(opposite = FALSE,
           labels = list(format = "{value}%")) %>%
  hc_tooltip(pointFormat = '{point.x:%Y-%m-%d},
                            {point.y:.4f}%')


##############################################################################



# (Optional) If it needs to be created 
cps_data3 <- cps_data3 %>%
  mutate(CINETHH_factor =  as_factor(lbl_clean(CINETHH)),
         CISMRTPHN_factor = as_factor(lbl_clean(CISMRTPHN)),
         CIDATAPLN_factor = as_factor(lbl_clean(CIDATAPLN)),
         CIHISPEED_factor = as_factor(lbl_clean(CIHISPEED))
         )
table(cps_data$STATE_factor, useNA = "always")


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
