

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
setwd("G:/Shared drives/Projects/5035_Tech Equity/Data/A_IPUMS_Data")

#################################################################################


# Reading IPUMS DATA
cps_ddi8 <- read_ipums_ddi("usa_00008.xml")
cps_data8 <- read_ipums_micro(cps_ddi8, verbose = FALSE)

cps_data8$STATEFIP = stri_pad_left(cps_data8$STATEFIP, 2, "0")
cps_data8$COUNTYFIP = stri_pad_left(cps_data8$COUNTYFIP, 3, "0")
cps_data8$PUMA = stri_pad_left(cps_data8$PUMA, 5, "0")
cps_data8$GEOID <- str_c(cps_data8$STATEFIP, '', cps_data8$COUNTYFIP)

View(cps_data8)
colnames(cps_data8)
ipums_val_labels(cps_data8$CIDATAPLN)
ipums_var_desc(cps_data8, "CIDATAPLN")
ipums_val_labels(cps_data8$CIHISPEED)
ipums_var_desc(cps_data8, "CIHISPEED")


#################################################################################


# 1. Create Bradband Access dataset by 3 different geographies (PRA Region, New York City, Manhattan)

cps_data8_RPA <-
cps_data8 %>%
  filter(GEOID == "36005" | GEOID == "36027" | GEOID == "36047" | GEOID == "36059" | GEOID == "36061" | 
         GEOID == "36071" | GEOID == "36079" | GEOID == "36081" | GEOID == "36085" | GEOID == "36087" |
         GEOID == "36103" | GEOID == "36105" | GEOID == "36111" | GEOID == "36119" | GEOID == "34003" | 
         GEOID == "34013" | GEOID == "34017" | GEOID == "34019" | GEOID == "34021" | GEOID == "34023" |
         GEOID == "34025" | GEOID == "34027" | GEOID == "34029" | GEOID == "34031" | GEOID == "34035" |
         GEOID == "34037" | GEOID == "34039" | GEOID == "34041" | GEOID == "09001" | GEOID == "09005" | 
         GEOID == "09009"
  ) %>%
  select(
    YEAR, HHWT, STATEFIP, COUNTYFIP, GEOID, 
    CIDATAPLN, CIHISPEED, AGE, HHINCOME, RACE, HISPAN, EDUC,
    PERNUM, PERWT
  ) %>%
  mutate(
    AGE_RE = case_when(
      AGE < 18 ~ "Ag1_Und18",
      AGE < 40 ~ "Ag2_19_30",
      AGE < 65 ~ "Ag3_40_64",
      AGE > 64 ~ "Ag4_65Over"
    ),
    RACE_RE = case_when(
      RACE == 1 & HISPAN == 0 ~ "1_WHITE", # 1. WHITE
      RACE == 2 & HISPAN == 0 ~ "2_BLACK", # 2. BLACK
      RACE == 3 & HISPAN == 0 ~ "5_OTHRS", # 5. OTHERS
      RACE == 4 & HISPAN == 0 ~ "4_ASIAN", # 4. ASIAN
      RACE == 5 & HISPAN == 0 ~ "4_ASIAN",
      RACE == 6 & HISPAN == 0 ~ "4_ASIAN",
      RACE == 7 & HISPAN == 0 ~ "5_OTHRS", 
      RACE == 8 & HISPAN == 0 ~ "5_OTHRS",
      RACE == 9 & HISPAN == 0 ~ "5_OTHRS",
      HISPAN == 1 | HISPAN == 2 | HISPAN == 3 | HISPAN == 4 ~ "3_HSPAN" # 3. HISPANIC
    ),
    EDUC_RE = case_when(
      EDUC == 0 ~ "1_less than HSD", # 1. LESS THAN HIGH SCHOOL DIPLOMA
      EDUC == 1 ~ "1_less than HSD", 
      EDUC == 2 ~ "1_less than HSD", 
      EDUC == 3 ~ "1_less than HSD", 
      EDUC == 4 ~ "1_less than HSD", 
      EDUC == 5 ~ "1_less than HSD",
      EDUC == 6 ~ "2_High School Diploma", # 2. HIGH SCHOOL DIPLOMA
      EDUC == 7 ~ "3_Some College Degree", # 3. SOME COLLEGE
      EDUC == 8 ~ "3_Some College Degree",
      EDUC == 9 ~ "3_Some College Degree", 
      EDUC == 10 ~ "4_Bachelor's and over", # 4. OVER COLLEGE DEGREE
      EDUC == 11 ~ "4_Bachelor's and over"
    ),
    SUBREG = case_when(
      GEOID == "09001" ~ "5_SW Connecticut", # 5. Connecticut
      GEOID == "09005" ~ "5_SW Connecticut", 
      GEOID == "09009" ~ "5_SW Connecticut", 
      GEOID == 34003 ~ "4_Nothern New Jersey", # 4. New Jersey 
      GEOID == 34013 ~ "4_Nothern New Jersey",
      GEOID == 34017 ~ "4_Nothern New Jersey", 
      GEOID == 34019 ~ "4_Nothern New Jersey", 
      GEOID == 34021 ~ "4_Nothern New Jersey",
      GEOID == 34023 ~ "4_Nothern New Jersey",
      GEOID == 34025 ~ "4_Nothern New Jersey",
      GEOID == 34027 ~ "4_Nothern New Jersey", 
      GEOID == 34029 ~ "4_Nothern New Jersey",
      GEOID == 34031 ~ "4_Nothern New Jersey",
      GEOID == 34035 ~ "4_Nothern New Jersey",
      GEOID == 34037 ~ "4_Nothern New Jersey",
      GEOID == 34039 ~ "4_Nothern New Jersey",
      GEOID == 34041 ~ "4_Nothern New Jersey",
      GEOID == 36005 ~ "1_New York City", # 1. New York City
      GEOID == 36027 ~ "3_Hudson Valley", # 3. Other New York
      GEOID == 36047 ~ "1_New York City", 
      GEOID == 36059 ~ "2_Long Island", # 2. Long Island
      GEOID == 36061 ~ "1_New York City", 
      GEOID == 36071 ~ "3_Hudson Valley",
      GEOID == 36079 ~ "3_Hudson Valley",
      GEOID == 36081 ~ "1_New York City",
      GEOID == 36085 ~ "1_New York City", 
      GEOID == 36087 ~ "3_Hudson Valley", 
      GEOID == 36103 ~ "2_Long Island", 
      GEOID == 36105 ~ "3_Hudson Valley",
      GEOID == 36111 ~ "3_Hudson Valley", 
      GEOID == 36119 ~ "3_Hudson Valley"
    )
  ) %>%
  filter(
    CIHISPEED != 0
  )

cps_data8_NYC <-
  cps_data8 %>%
  filter(
    GEOID == "36061" | GEOID == "36047" | GEOID == "36081" | GEOID == "36005" | GEOID == "36085"
  ) %>%
  select(
    YEAR, HHWT, STATEFIP, COUNTYFIP, GEOID, 
    CIDATAPLN, CIHISPEED, AGE, HHINCOME, RACE, HISPAN, EDUC,
    PERNUM, PERWT
  ) %>%
  mutate(
    AGE_RE = case_when(
      AGE < 18 ~ "Ag1_Und18",
      AGE < 40 ~ "Ag2_19_30",
      AGE < 65 ~ "Ag3_40_64",
      AGE > 64 ~ "Ag4_65Over"
    ),
    RACE_RE = case_when(
      RACE == 1 & HISPAN == 0 ~ "1_WHITE", # 1. WHITE
      RACE == 2 & HISPAN == 0 ~ "2_BLACK", # 2. BLACK
      RACE == 3 & HISPAN == 0 ~ "5_OTHRS", # 5. OTHERS
      RACE == 4 & HISPAN == 0 ~ "4_ASIAN", # 4. ASIAN
      RACE == 5 & HISPAN == 0 ~ "4_ASIAN",
      RACE == 6 & HISPAN == 0 ~ "4_ASIAN",
      RACE == 7 & HISPAN == 0 ~ "5_OTHRS", 
      RACE == 8 & HISPAN == 0 ~ "5_OTHRS",
      RACE == 9 & HISPAN == 0 ~ "5_OTHRS",
      HISPAN == 1 | HISPAN == 2 | HISPAN == 3 | HISPAN == 4 ~ "3_HSPAN" # 3. HISPANIC
    ),
    EDUC_RE = case_when(
      EDUC == 0 ~ "1_less than HSD", # 1. LESS THAN HIGH SCHOOL DIPLOMA
      EDUC == 1 ~ "1_less than HSD", 
      EDUC == 2 ~ "1_less than HSD", 
      EDUC == 3 ~ "1_less than HSD", 
      EDUC == 4 ~ "1_less than HSD", 
      EDUC == 5 ~ "1_less than HSD",
      EDUC == 6 ~ "2_High School Diploma", # 2. HIGH SCHOOL DIPLOMA
      EDUC == 7 ~ "3_Some College Degree", # 3. SOME COLLEGE
      EDUC == 8 ~ "3_Some College Degree",
      EDUC == 9 ~ "3_Some College Degree", 
      EDUC == 10 ~ "4_Bachelor's and over", # 4. OVER COLLEGE DEGREE
      EDUC == 11 ~ "4_Bachelor's and over"
    ),
    SUBREG = case_when(
      GEOID == "09001" ~ "5_SW Connecticut", # 5. Connecticut
      GEOID == "09005" ~ "5_SW Connecticut", 
      GEOID == "09009" ~ "5_SW Connecticut", 
      GEOID == 34003 ~ "4_Nothern New Jersey", # 4. New Jersey 
      GEOID == 34013 ~ "4_Nothern New Jersey",
      GEOID == 34017 ~ "4_Nothern New Jersey", 
      GEOID == 34019 ~ "4_Nothern New Jersey", 
      GEOID == 34021 ~ "4_Nothern New Jersey",
      GEOID == 34023 ~ "4_Nothern New Jersey",
      GEOID == 34025 ~ "4_Nothern New Jersey",
      GEOID == 34027 ~ "4_Nothern New Jersey", 
      GEOID == 34029 ~ "4_Nothern New Jersey",
      GEOID == 34031 ~ "4_Nothern New Jersey",
      GEOID == 34035 ~ "4_Nothern New Jersey",
      GEOID == 34037 ~ "4_Nothern New Jersey",
      GEOID == 34039 ~ "4_Nothern New Jersey",
      GEOID == 34041 ~ "4_Nothern New Jersey",
      GEOID == 36005 ~ "1_New York City", # 1. New York City
      GEOID == 36027 ~ "3_Hudson Valley", # 3. Other New York
      GEOID == 36047 ~ "1_New York City", 
      GEOID == 36059 ~ "2_Long Island", # 2. Long Island
      GEOID == 36061 ~ "1_New York City", 
      GEOID == 36071 ~ "3_Hudson Valley",
      GEOID == 36079 ~ "3_Hudson Valley",
      GEOID == 36081 ~ "1_New York City",
      GEOID == 36085 ~ "1_New York City", 
      GEOID == 36087 ~ "3_Hudson Valley", 
      GEOID == 36103 ~ "2_Long Island", 
      GEOID == 36105 ~ "3_Hudson Valley",
      GEOID == 36111 ~ "3_Hudson Valley", 
      GEOID == 36119 ~ "3_Hudson Valley"
    )
  ) %>%
  filter(
    CIHISPEED != 0
  )

cps_data8_MAN <-
  cps_data8 %>%
  filter(
    GEOID == "36061"
  ) %>%
  select(
    YEAR, HHWT, STATEFIP, COUNTYFIP, GEOID, 
    CIDATAPLN, CIHISPEED, AGE, HHINCOME, RACE, HISPAN, EDUC,
    PERNUM, PERWT
  ) %>%
  mutate(
    AGE_RE = case_when(
      AGE < 18 ~ "Ag1_Und18",
      AGE < 40 ~ "Ag2_19_30",
      AGE < 65 ~ "Ag3_40_64",
      AGE > 64 ~ "Ag4_65Over"
    ),
    RACE_RE = case_when(
      RACE == 1 & HISPAN == 0 ~ "1_WHITE", # 1. WHITE
      RACE == 2 & HISPAN == 0 ~ "2_BLACK", # 2. BLACK
      RACE == 3 & HISPAN == 0 ~ "5_OTHRS", # 5. OTHERS
      RACE == 4 & HISPAN == 0 ~ "4_ASIAN", # 4. ASIAN
      RACE == 5 & HISPAN == 0 ~ "4_ASIAN",
      RACE == 6 & HISPAN == 0 ~ "4_ASIAN",
      RACE == 7 & HISPAN == 0 ~ "5_OTHRS", 
      RACE == 8 & HISPAN == 0 ~ "5_OTHRS",
      RACE == 9 & HISPAN == 0 ~ "5_OTHRS",
      HISPAN == 1 | HISPAN == 2 | HISPAN == 3 | HISPAN == 4 ~ "3_HSPAN" # 3. HISPANIC
    ),
    EDUC_RE = case_when(
      EDUC == 0 ~ "1_less than HSD", # 1. LESS THAN HIGH SCHOOL DIPLOMA
      EDUC == 1 ~ "1_less than HSD", 
      EDUC == 2 ~ "1_less than HSD", 
      EDUC == 3 ~ "1_less than HSD", 
      EDUC == 4 ~ "1_less than HSD", 
      EDUC == 5 ~ "1_less than HSD",
      EDUC == 6 ~ "2_High School Diploma", # 2. HIGH SCHOOL DIPLOMA
      EDUC == 7 ~ "3_Some College Degree", # 3. SOME COLLEGE
      EDUC == 8 ~ "3_Some College Degree",
      EDUC == 9 ~ "3_Some College Degree", 
      EDUC == 10 ~ "4_Bachelor's and over", # 4. OVER COLLEGE DEGREE
      EDUC == 11 ~ "4_Bachelor's and over"
    ),
    SUBREG = case_when(
      GEOID == "09001" ~ "5_SW Connecticut", # 5. Connecticut
      GEOID == "09005" ~ "5_SW Connecticut", 
      GEOID == "09009" ~ "5_SW Connecticut", 
      GEOID == 34003 ~ "4_Nothern New Jersey", # 4. New Jersey 
      GEOID == 34013 ~ "4_Nothern New Jersey",
      GEOID == 34017 ~ "4_Nothern New Jersey", 
      GEOID == 34019 ~ "4_Nothern New Jersey", 
      GEOID == 34021 ~ "4_Nothern New Jersey",
      GEOID == 34023 ~ "4_Nothern New Jersey",
      GEOID == 34025 ~ "4_Nothern New Jersey",
      GEOID == 34027 ~ "4_Nothern New Jersey", 
      GEOID == 34029 ~ "4_Nothern New Jersey",
      GEOID == 34031 ~ "4_Nothern New Jersey",
      GEOID == 34035 ~ "4_Nothern New Jersey",
      GEOID == 34037 ~ "4_Nothern New Jersey",
      GEOID == 34039 ~ "4_Nothern New Jersey",
      GEOID == 34041 ~ "4_Nothern New Jersey",
      GEOID == 36005 ~ "1_New York City", # 1. New York City
      GEOID == 36027 ~ "3_Hudson Valley", # 3. Other New York
      GEOID == 36047 ~ "1_New York City", 
      GEOID == 36059 ~ "2_Long Island", # 2. Long Island
      GEOID == 36061 ~ "1_New York City", 
      GEOID == 36071 ~ "3_Hudson Valley",
      GEOID == 36079 ~ "3_Hudson Valley",
      GEOID == 36081 ~ "1_New York City",
      GEOID == 36085 ~ "1_New York City", 
      GEOID == 36087 ~ "3_Hudson Valley", 
      GEOID == 36103 ~ "2_Long Island", 
      GEOID == 36105 ~ "3_Hudson Valley",
      GEOID == 36111 ~ "3_Hudson Valley", 
      GEOID == 36119 ~ "3_Hudson Valley"
    )
  ) %>%
  filter(
    CIHISPEED != 0
  )

View(cps_data8)
colnames(cps_data8)
ipums_val_labels(cps_data8$AGE)
ipums_val_labels(cps_data8$HISPAN)
ipums_val_labels(cps_data8$CIHISPEED)
ipums_val_labels(cps_data8$EDUC)
ipums_var_desc(cps_data8, "RACE")
ipums_var_desc(cps_data8, "AGE")

summary(cps_data8_RPA)

#################################################################################



# 2. Summarize datasets
# 2-1. Broadband Access
#   1) Broadband access by income quintile, grouped by age
ipums_val_labels(cps_data8$HHINCOME)
ipums_val_labels(cps_data8$AGE)

# Remove NA values & Create quintile column
cps_data8_RPA_INCOME <-
  cps_data8_RPA %>% 
  filter(HHINCOME != 9999999
  ) %>%
  mutate(
    HHINCOME_RE = ntile(HHINCOME,5)
  )

cps_data8_NYC_INCOME <-
  cps_data8_NYC %>% 
  filter(HHINCOME != 9999999
  ) %>%
  mutate(
    HHINCOME_RE = ntile(HHINCOME,5)
  )

cps_data8_MAN_INCOME <-
  cps_data8_MAN %>% 
  filter(HHINCOME != 9999999
  ) %>%
  mutate(
    HHINCOME_RE = ntile(HHINCOME,5)
  )

# RPA
BRBD_RPA_INC_SUM1 <-
  cps_data8_RPA_INCOME %>%
  group_by(HHINCOME_RE, AGE_RE) %>%
  summarize(n(),
            PERWT = sum(PERWT)) %>%
  dplyr::rename(
    COUNT ="n()"
  )

BRBD_RPA_INC_SUM2 <-
  cps_data8_RPA_INCOME %>%
  filter(CIHISPEED == 10) %>%
  group_by(HHINCOME_RE, AGE_RE) %>%
  summarize(n(),
            PERWT = sum(PERWT)) %>%
  dplyr::rename(
    COUNT ="n()"
  )

BRBD_RPA_INC_SUM3_MERGED <- merge(BRBD_RPA_INC_SUM1, BRBD_RPA_INC_SUM2, by = c("HHINCOME_RE", "AGE_RE"), all.x = TRUE)

BRBD_RPA_INC_SUM4_FIN <-
  BRBD_RPA_INC_SUM3_MERGED %>%
  dplyr::rename(
    CNT_TOTAL = "PERWT.x",
    CNT_BRDBD = "PERWT.y"
  ) %>%
  mutate(
    PCT_BRDBD = (CNT_BRDBD/CNT_TOTAL) * 100
  ) %>%
  mutate_at(vars(PCT_BRDBD), 
            funs(round(., 1)))

# NYC
BRBD_NYC_INC_SUM1 <-
  cps_data8_NYC_INCOME %>%
  group_by(HHINCOME_RE, AGE_RE) %>%
  summarize(n(),
            PERWT = sum(PERWT)) %>%
  dplyr::rename(
    COUNT ="n()"
  )

BRBD_NYC_INC_SUM2 <-
  cps_data8_NYC_INCOME %>%
  filter(CIHISPEED == 10) %>%
  group_by(HHINCOME_RE, AGE_RE) %>%
  summarize(n(),
            PERWT = sum(PERWT)) %>%
  dplyr::rename(
    COUNT ="n()"
  )

BRBD_NYC_INC_SUM3_MERGED <- merge(BRBD_NYC_INC_SUM1, BRBD_NYC_INC_SUM2, by = c("HHINCOME_RE", "AGE_RE"), all.x = TRUE)

BRBD_NYC_INC_SUM4_FIN <-
  BRBD_NYC_INC_SUM3_MERGED %>%
  dplyr::rename(
    CNT_TOTAL = "PERWT.x",
    CNT_BRDBD = "PERWT.y"
  ) %>%
  mutate(
    PCT_BRDBD = (CNT_BRDBD/CNT_TOTAL) * 100
  ) %>%
  mutate_at(vars(PCT_BRDBD), 
            funs(round(., 1)))

# MAN
BRBD_MAN_INC_SUM1 <-
  cps_data8_MAN_INCOME %>%
  group_by(HHINCOME_RE, AGE_RE) %>%
  summarize(n(),
            PERWT = sum(PERWT)) %>%
  dplyr::rename(
    COUNT ="n()"
  )

BRBD_MAN_INC_SUM2 <-
  cps_data8_MAN_INCOME %>%
  filter(CIHISPEED == 10) %>%
  group_by(HHINCOME_RE, AGE_RE) %>%
  summarize(n(),
            PERWT = sum(PERWT)) %>%
  dplyr::rename(
    COUNT ="n()"
  )

BRBD_MAN_INC_SUM3_MERGED <- merge(BRBD_MAN_INC_SUM1, BRBD_MAN_INC_SUM2, by = c("HHINCOME_RE", "AGE_RE"), all.x = TRUE)

BRBD_MAN_INC_SUM4_FIN <-
  BRBD_MAN_INC_SUM3_MERGED %>%
  dplyr::rename(
    CNT_TOTAL = "PERWT.x",
    CNT_BRDBD = "PERWT.y"
  ) %>%
  mutate(
    PCT_BRDBD = (CNT_BRDBD/CNT_TOTAL) * 100
  ) %>%
  mutate_at(vars(PCT_BRDBD), 
            funs(round(., 1)))


#   2) Broadband access by race, grouped by age
ipums_val_labels(cps_data8$RACE)
ipums_val_labels(cps_data8$AGE)

# RPA Level
BRBD_RPA_RACE_SUM1 <-
  cps_data8_RPA %>%
  group_by(RACE_RE, AGE_RE) %>%
  summarize(n,
            PERWT = sum(PERWT)) %>%
  dplyr::rename(
    COUNT ="n()"
  )

BRBD_RPA_RACE_SUM2 <-
  cps_data8_RPA %>%
  filter(CIHISPEED == 10) %>%
  group_by(RACE_RE, AGE_RE) %>%
  summarize(n(),
            PERWT = sum(PERWT)) %>%
  dplyr::rename(
    COUNT ="n()"
  )

BRBD_RPA_RACE_SUM3_MERGED <- merge(BRBD_RPA_RACE_SUM1, BRBD_RPA_RACE_SUM2, by = c("RACE_RE", "AGE_RE"), all.x = TRUE)

BRBD_RPA_RACE_SUM4_FIN <-
  BRBD_RPA_RACE_SUM3_MERGED %>%
  dplyr::rename(
    CNT_TOTAL = "PERWT.x",
    CNT_BRDBD = "PERWT.y"
  ) %>%
  mutate(
    PCT_BRDBD = (CNT_BRDBD/CNT_TOTAL) * 100
  ) %>%
  mutate_at(vars(PCT_BRDBD), 
            funs(round(., 1)))

# NYC Level
BRBD_NYC_RACE_SUM1 <-
  cps_data8_NYC %>%
  group_by(RACE_RE, AGE_RE) %>%
  summarize(n(),
            PERWT = sum(PERWT)) %>%
  dplyr::rename(
    COUNT ="n()"
  )

BRBD_NYC_RACE_SUM2 <-
  cps_data8_NYC %>%
  filter(CIHISPEED == 10) %>%
  group_by(RACE_RE, AGE_RE) %>%
  summarize(n(),
            PERWT = sum(PERWT)) %>%
  dplyr::rename(
    COUNT ="n()"
  )

BRBD_NYC_RACE_SUM3_MERGED <- merge(BRBD_NYC_RACE_SUM1, BRBD_NYC_RACE_SUM2, by = c("RACE_RE", "AGE_RE"), all.x = TRUE)

BRBD_NYC_RACE_SUM4_FIN <-
  BRBD_NYC_RACE_SUM3_MERGED %>%
  dplyr::rename(
    CNT_TOTAL = "PERWT.x",
    CNT_BRDBD = "PERWT.y"
  ) %>%
  mutate(
    PCT_BRDBD = (CNT_BRDBD/CNT_TOTAL) * 100
  ) %>%
  mutate_at(vars(PCT_BRDBD), 
            funs(round(., 1)))

# Manhattan Level
BRBD_MAN_RACE_SUM1 <-
  cps_data8_MAN %>%
  group_by(RACE_RE, AGE_RE) %>%
  summarize(n(),
            PERWT = sum(PERWT)) %>%
  dplyr::rename(
    COUNT ="n()"
  )

BRBD_MAN_RACE_SUM2 <-
  cps_data8_MAN %>%
  filter(CIHISPEED == 10) %>%
  group_by(RACE_RE, AGE_RE) %>%
  summarize(n(),
            PERWT = sum(PERWT)) %>%
  dplyr::rename(
    COUNT ="n()"
  )

BRBD_MAN_RACE_SUM3_MERGED <- merge(BRBD_MAN_RACE_SUM1, BRBD_MAN_RACE_SUM2, by = c("RACE_RE", "AGE_RE"), all.x = TRUE)

BRBD_MAN_RACE_SUM4_FIN <-
  BRBD_MAN_RACE_SUM3_MERGED %>%
  dplyr::rename(
    CNT_TOTAL = "PERWT.x",
    CNT_BRDBD = "PERWT.y"
  ) %>%
  mutate(
    PCT_BRDBD = (CNT_BRDBD/CNT_TOTAL) * 100
  ) %>%
  mutate_at(vars(PCT_BRDBD), 
            funs(round(., 1)))



#   3) Broadband access by education level, grouped by age
ipums_val_labels(cps_data8$EDUC)
ipums_val_labels(cps_data8$AGE)

# RPA Level
BRBD_RPA_EDUC_SUM1 <-
  cps_data8_RPA %>%
  group_by(EDUC_RE, AGE_RE) %>%
  summarize(n(),
            PERWT = sum(PERWT)) %>%
  dplyr::rename(
    COUNT ="n()"
  )

BRBD_RPA_EDUC_SUM2 <-
  cps_data8_RPA %>%
  filter(CIHISPEED == 10) %>%
  group_by(EDUC_RE, AGE_RE) %>%
  summarize(n(),
            PERWT = sum(PERWT)) %>%
  dplyr::rename(
    COUNT ="n()"
  )

BRBD_RPA_EDUC_SUM3_MERGED <- merge(BRBD_RPA_EDUC_SUM1, BRBD_RPA_EDUC_SUM2, by = c("EDUC_RE", "AGE_RE"), all.x = TRUE)

BRBD_RPA_EDUC_SUM4_FIN <-
  BRBD_RPA_EDUC_SUM3_MERGED %>%
  dplyr::rename(
    CNT_TOTAL = "PERWT.x",
    CNT_BRDBD = "PERWT.y"
  ) %>%
  mutate(
    PCT_BRDBD = (CNT_BRDBD/CNT_TOTAL) * 100
  ) %>%
  mutate_at(vars(PCT_BRDBD), 
            funs(round(., 1)))

# # NYC Level
# BRBD_NYC_EDUC_SUM1 <-
#   cps_data8_NYC %>%
#   group_by(EDUC_RE, AGE_RE) %>%
#   summarize(n()) %>%
#   dplyr::rename(
#     COUNT ="n()"
#   )
# 
# BRBD_NYC_EDUC_SUM2 <-
#   cps_data8_NYC %>%
#   filter(CIHISPEED == 10) %>%
#   group_by(EDUC_RE, AGE_RE) %>%
#   summarize(n()) %>%
#   dplyr::rename(
#     COUNT ="n()"
#   )
# 
# BRBD_NYC_EDUC_SUM3_MERGED <- merge(BRBD_NYC_EDUC_SUM1, BRBD_NYC_EDUC_SUM2, by = c("EDUC_RE", "AGE_RE"), all.x = TRUE)
# 
# BRBD_NYC_EDUC_SUM4_FIN <-
#   BRBD_NYC_EDUC_SUM3_MERGED %>%
#   dplyr::rename(
#     CNT_TOTAL = "PERWT.x",
#     CNT_BRDBD = "PERWT.y"
#   ) %>%
#   mutate(
#     PCT_BRDBD = (CNT_BRDBD/CNT_TOTAL) * 100
#   ) %>%
#   mutate_at(vars(PCT_BRDBD), 
#             funs(round(., 1)))
# 
# # MANHATTAN Level
# BRBD_MAN_EDUC_SUM1 <-
#   cps_data8_MAN %>%
#   group_by(EDUC_RE, AGE_RE) %>%
#   summarize(n()) %>%
#   dplyr::rename(
#     COUNT ="n()"
#   )
# 
# BRBD_MAN_EDUC_SUM2 <-
#   cps_data8_MAN %>%
#   filter(CIHISPEED == 10) %>%
#   group_by(EDUC_RE, AGE_RE) %>%
#   summarize(n()) %>%
#   dplyr::rename(
#     COUNT ="n()"
#   )
# 
# BRBD_MAN_EDUC_SUM3_MERGED <- merge(BRBD_MAN_EDUC_SUM1, BRBD_MAN_EDUC_SUM2, by = c("EDUC_RE", "AGE_RE"), all.x = TRUE)
# 
# BRBD_MAN_EDUC_SUM4_FIN <-
#   BRBD_MAN_EDUC_SUM3_MERGED %>%
#   dplyr::rename(
#     CNT_TOTAL = "PERWT.x",
#     CNT_BRDBD = "PERWT.y"
#   ) %>%
#   mutate(
#     PCT_BRDBD = (CNT_BRDBD/CNT_TOTAL) * 100
#   ) %>%
#   mutate_at(vars(PCT_BRDBD), 
#             funs(round(., 1)))
# 


#   4) Broadband access by race, grouped by subregion
ipums_val_labels(cps_data8$RACE)

# RPA Level
BRBD_RPA_SUBREG_SUM1 <-
  cps_data8_RPA %>%
  group_by(RACE_RE, SUBREG) %>%
  summarize(n(),
            PERWT = sum(PERWT)) %>%
  dplyr::rename(
    COUNT ="n()"
  )

BRBD_RPA_SUBREG_SUM2 <-
  cps_data8_RPA %>%
  filter(CIHISPEED == 10) %>%
  group_by(RACE_RE, SUBREG) %>%
  summarize(n(),
            PERWT = sum(PERWT)) %>%
  dplyr::rename(
    COUNT ="n()"
  )

BRBD_RPA_SUBREG_SUM3_MERGED <- merge(BRBD_RPA_SUBREG_SUM1, BRBD_RPA_SUBREG_SUM2, by = c("RACE_RE", "SUBREG"), all.x = TRUE)

BRBD_RPA_SUBREG_SUM4_FIN <-
  BRBD_RPA_SUBREG_SUM3_MERGED %>%
  dplyr::rename(
    CNT_TOTAL = "PERWT.x",
    CNT_BRDBD = "PERWT.y"
  ) %>%
  mutate(
    PCT_BRDBD = (CNT_BRDBD/CNT_TOTAL) * 100
  ) %>%
  mutate_at(vars(PCT_BRDBD), 
            funs(round(., 1)))

# # NYC Level
# BRBD_NYC_SUBREG_SUM1 <-
#   cps_data8_NYC %>%
#   group_by(RACE_RE, SUBREG) %>%
#   summarize(n()) %>%
#   dplyr::rename(
#     COUNT ="n()"
#   )
# 
# BRBD_NYC_SUBREG_SUM2 <-
#   cps_data8_NYC %>%
#   filter(CIHISPEED == 10) %>%
#   group_by(RACE_RE, SUBREG) %>%
#   summarize(n()) %>%
#   dplyr::rename(
#     COUNT ="n()"
#   )
# 
# BRBD_NYC_SUBREG_SUM3_MERGED <- merge(BRBD_NYC_SUBREG_SUM1, BRBD_NYC_SUBREG_SUM2, by = c("RACE_RE", "SUBREG"), all.x = TRUE)
# 
# BRBD_NYC_SUBREG_SUM4_FIN <-
#   BRBD_NYC_SUBREG_SUM3_MERGED %>%
#   dplyr::rename(
#     CNT_TOTAL = "PERWT.x",
#     CNT_BRDBD = "PERWT.y"
#   ) %>%
#   mutate(
#     PCT_BRDBD = (CNT_BRDBD/CNT_TOTAL) * 100
#   ) %>%
#   mutate_at(vars(PCT_BRDBD), 
#             funs(round(., 1)))
# 
# # MANHATTAN Level
# 
# BRBD_MAN_SUBREG_SUM1 <-
#   cps_data8_MAN %>%
#   group_by(RACE_RE, SUBREG) %>%
#   summarize(n()) %>%
#   dplyr::rename(
#     COUNT ="n()"
#   )
# 
# BRBD_MAN_SUBREG_SUM2 <-
#   cps_data8_MAN %>%
#   filter(CIHISPEED == 10) %>%
#   group_by(RACE_RE, SUBREG) %>%
#   summarize(n()) %>%
#   dplyr::rename(
#     COUNT ="n()"
#   )
# 
# BRBD_MAN_SUBREG_SUM3_MERGED <- merge(BRBD_MAN_SUBREG_SUM1, BRBD_MAN_SUBREG_SUM2, by = c("RACE_RE", "SUBREG"), all.x = TRUE)
# 
# BRBD_MAN_SUBREG_SUM4_FIN <-
#   BRBD_MAN_SUBREG_SUM3_MERGED %>%
#   dplyr::rename(
#     CNT_TOTAL = "PERWT.x",
#     CNT_BRDBD = "PERWT.y"
#   ) %>%
#   mutate(
#     PCT_BRDBD = (CNT_BRDBD/CNT_TOTAL) * 100
#   ) %>%
#   mutate_at(vars(PCT_BRDBD), 
#             funs(round(., 1)))
# 



# 2-2. Smartphone with Data Plan without Broadband Access
#   1) Smartphone with Data Plan without Broadband Access by income quintile, grouped by age
colnames(cps_data8)
ipums_val_labels(cps_data8$HHINCOME)
ipums_val_labels(cps_data8$CIDATAPLN)
ipums_val_labels(cps_data8$CIHISPEED)

cps_data8_RPA_INCOME %>%
  group_by(CIDATAPLN) %>%
  summarize(n(),
            PERWT = sum(PERWT))

cps_data8_RPA_INCOME %>%
  group_by(CIHISPEED) %>%
  summarize(n(),
            PERWT = sum(PERWT))

#   RPA
SMTP_RPA_INC_SUM1 <-
  cps_data8_RPA_INCOME %>%
  filter(CIDATAPLN == 1) %>%
  group_by(HHINCOME_RE, AGE_RE) %>%
  summarize(n()) %>%
  dplyr::rename(
    COUNT ="n()"
  )

SMTP_RPA_INC_SUM2 <-
  cps_data8_RPA_INCOME %>%
  filter(CIDATAPLN == 1) %>%
  filter(CIHISPEED == 20) %>%
  group_by(HHINCOME_RE, AGE_RE) %>%
  summarize(n()) %>%
  dplyr::rename(
    COUNT ="n()"
  )

SMTP_RPA_INC_SUM3_MERGED <- merge(SMTP_RPA_INC_SUM1, SMTP_RPA_INC_SUM2, by = c("HHINCOME_RE", "AGE_RE"), all.x = TRUE)

SMTP_RPA_INC_SUM4_FIN <-
  SMTP_RPA_INC_SUM3_MERGED %>%
  dplyr::rename(
    TOTAL = "PERWT.x",
    SMTP_XHISPD = "PERWT.y"
  ) %>%
  mutate(
    PCT_SMTP_XHISPD = (SMTP_XHISPD/TOTAL) * 100
  ) %>%
  mutate_at(vars(PCT_SMTP_XHISPD), 
            funs(round(., 1)))

# #   NYC
# SMTP_NYC_INC_SUM1 <-
#   cps_data8_NYC_INCOME %>%
#   filter(CIDATAPLN == 1) %>%
#   group_by(HHINCOME_RE, AGE_RE) %>%
#   summarize(n()) %>%
#   dplyr::rename(
#     COUNT ="n()"
#   )
# 
# SMTP_NYC_INC_SUM2 <-
#   cps_data8_NYC_INCOME %>%
#   filter(CIDATAPLN == 1) %>%
#   filter(CIHISPEED == 20) %>%
#   group_by(HHINCOME_RE, AGE_RE) %>%
#   summarize(n()) %>%
#   dplyr::rename(
#     COUNT ="n()"
#   )
# 
# SMTP_NYC_INC_SUM3_MERGED <- merge(SMTP_NYC_INC_SUM1, SMTP_NYC_INC_SUM2, by = c("HHINCOME_RE", "AGE_RE"), all.x = TRUE)
# 
# SMTP_NYC_INC_SUM4_FIN <-
#   SMTP_NYC_INC_SUM3_MERGED %>%
#   dplyr::rename(
#     TOTAL = "PERWT.x",
#     SMTP_XHISPD = "PERWT.y"
#   ) %>%
#   mutate(
#     PCT_SMTP_XHISPD = (SMTP_XHISPD/TOTAL) * 100
#   ) %>%
#   mutate_at(vars(PCT_SMTP_XHISPD), 
#             funs(round(., 1)))
# 
# #   MAN
# SMTP_MAN_INC_SUM1 <-
#   cps_data8_MAN_INCOME %>%
#   filter(CIDATAPLN == 1) %>%
#   group_by(HHINCOME_RE, AGE_RE) %>%
#   summarize(n()) %>%
#   dplyr::rename(
#     COUNT ="n()"
#   )
# 
# SMTP_MAN_INC_SUM2 <-
#   cps_data8_MAN_INCOME %>%
#   filter(CIDATAPLN == 1) %>%
#   filter(CIHISPEED == 20) %>%
#   group_by(HHINCOME_RE, AGE_RE) %>%
#   summarize(n()) %>%
#   dplyr::rename(
#     COUNT ="n()"
#   )
# 
# SMTP_MAN_INC_SUM3_MERGED <- merge(SMTP_MAN_INC_SUM1, SMTP_MAN_INC_SUM2, by = c("HHINCOME_RE", "AGE_RE"), all.x = TRUE)
# 
# SMTP_MAN_INC_SUM4_FIN <-
#   SMTP_MAN_INC_SUM3_MERGED %>%
#   dplyr::rename(
#     TOTAL = "PERWT.x",
#     SMTP_XHISPD = "PERWT.y"
#   ) %>%
#   mutate(
#     PCT_SMTP_XHISPD = (SMTP_XHISPD/TOTAL) * 100
#   ) %>%
#   mutate_at(vars(PCT_SMTP_XHISPD), 
#             funs(round(., 1)))
# 


#   2) Smartphone with Data Plan without Broadband Access by race, grouped by age
colnames(cps_data8_RPA)

cps_data8_RPA %>%
  filter(CIDATAPLN == 1) %>%
  group_by(CIHISPEED) %>%
  summarize(n(),
            PERWT = sum(PERWT))

#   RPA
SMTP_RPA_RAC_SUM1 <-
  cps_data8_RPA %>%
  filter(CIDATAPLN == 1) %>%
  group_by(RACE_RE, AGE_RE) %>%
  summarize(n(),
            PERWT = sum(PERWT)) %>%
  dplyr::rename(
    COUNT ="n()"
  )

SMTP_RPA_RAC_SUM2 <-
  cps_data8_RPA %>%
  filter(CIDATAPLN == 1) %>%
  filter(CIHISPEED == 20) %>%
  group_by(RACE_RE, AGE_RE) %>%
  summarize(n(),
            PERWT = sum(PERWT)) %>%
  dplyr::rename(
    COUNT ="n()"
  )

SMTP_RPA_RAC_SUM3_MERGED <- merge(SMTP_RPA_RAC_SUM1, SMTP_RPA_RAC_SUM2, by = c("RACE_RE", "AGE_RE"), all.x = TRUE)

SMTP_RPA_RAC_SUM4_FIN <-
  SMTP_RPA_RAC_SUM3_MERGED %>%
  dplyr::rename(
    TOTAL = "PERWT.x",
    SMTP_XHISPD = "PERWT.y"
  ) %>%
  mutate(
    PCT_SMTP_XHISPD = (SMTP_XHISPD/TOTAL) * 100
  ) %>%
  mutate_at(vars(PCT_SMTP_XHISPD), 
            funs(round(., 1)))

# #   NYC
# SMTP_NYC_RAC_SUM1 <-
#   cps_data8_NYC %>%
#   filter(CIDATAPLN == 1) %>%
#   group_by(RACE_RE, AGE_RE) %>%
#   summarize(n(),
#             PERWT = sum(PERWT)) %>%
#   dplyr::rename(
#     COUNT ="n()"
#   )
# 
# SMTP_NYC_RAC_SUM2 <-
#   cps_data8_NYC %>%
#   filter(CIDATAPLN == 1) %>%
#   filter(CIHISPEED == 20) %>%
#   group_by(RACE_RE, AGE_RE) %>%
#   summarize(n(),
#             PERWT = sum(PERWT)) %>%
#   dplyr::rename(
#     COUNT ="n()"
#   )
# 
# SMTP_NYC_RAC_SUM3_MERGED <- merge(SMTP_NYC_RAC_SUM1, SMTP_NYC_RAC_SUM2, by = c("RACE_RE", "AGE_RE"), all.x = TRUE)
# 
# SMTP_NYC_RAC_SUM4_FIN <-
#   SMTP_NYC_RAC_SUM3_MERGED %>%
#   dplyr::rename(
#     TOTAL = "PERWT.x",
#     SMTP_XHISPD = "PERWT.y"
#   ) %>%
#   mutate(
#     PCT_SMTP_XHISPD = (SMTP_XHISPD/TOTAL) * 100
#   ) %>%
#   mutate_at(vars(PCT_SMTP_XHISPD), 
#             funs(round(., 1)))
# 
# #   MAN
# SMTP_MAN_RAC_SUM1 <-
#   cps_data8_MAN %>%
#   filter(CIDATAPLN == 1) %>%
#   group_by(RACE_RE, AGE_RE) %>%
#   summarize(n()) %>%
#   dplyr::rename(
#     COUNT ="n()"
#   )
# 
# SMTP_MAN_RAC_SUM2 <-
#   cps_data8_MAN %>%
#   filter(CIDATAPLN == 1) %>%
#   filter(CIHISPEED == 20) %>%
#   group_by(RACE_RE, AGE_RE) %>%
#   summarize(n()) %>%
#   dplyr::rename(
#     COUNT ="n()"
#   )
# 
# SMTP_MAN_RAC_SUM3_MERGED <- merge(SMTP_MAN_RAC_SUM1, SMTP_MAN_RAC_SUM2, by = c("RACE_RE", "AGE_RE"), all.x = TRUE)
# 
# SMTP_MAN_RAC_SUM4_FIN <-
#   SMTP_MAN_RAC_SUM3_MERGED %>%
#   dplyr::rename(
#     TOTAL = "PERWT.x",
#     SMTP_XHISPD = "PERWT.y"
#   ) %>%
#   mutate(
#     PCT_SMTP_XHISPD = (SMTP_XHISPD/TOTAL) * 100
#   ) %>%
#   mutate_at(vars(PCT_SMTP_XHISPD), 
#             funs(round(., 1)))
# 


#   3) Smartphone with Data Plan without Broadband Access by education level, grouped by age
colnames(cps_data8)
ipums_val_labels(cps_data8$EDUC)

#   RPA
SMTP_RPA_EDU_SUM1 <-
  cps_data8_RPA %>%
  filter(CIDATAPLN == 1) %>%
  group_by(EDUC_RE, AGE_RE) %>%
  summarize(n(),
            PERWT = sum(PERWT)) %>%
  dplyr::rename(
    COUNT ="n()"
  )

SMTP_RPA_EDU_SUM2 <-
  cps_data8_RPA %>%
  filter(CIDATAPLN == 1) %>%
  filter(CIHISPEED == 20) %>%
  group_by(EDUC_RE, AGE_RE) %>%
  summarize(n(),
            PERWT = sum(PERWT)) %>%
  dplyr::rename(
    COUNT ="n()"
  )

SMTP_RPA_EDU_SUM3_MERGED <- merge(SMTP_RPA_EDU_SUM1, SMTP_RPA_EDU_SUM2, by = c("EDUC_RE", "AGE_RE"), all.x = TRUE)

SMTP_RPA_EDU_SUM4_FIN <-
  SMTP_RPA_EDU_SUM3_MERGED %>%
  dplyr::rename(
    TOTAL = "PERWT.x",
    SMTP_XHISPD = "PERWT.y"
  ) %>%
  mutate(
    PCT_SMTP_XHISPD = (SMTP_XHISPD/TOTAL) * 100
  ) %>%
  mutate_at(vars(PCT_SMTP_XHISPD), 
            funs(round(., 1)))

# #   NYC
# SMTP_NYC_EDU_SUM1 <-
#   cps_data8_NYC %>%
#   filter(CIDATAPLN == 1) %>%
#   group_by(EDUC_RE, AGE_RE) %>%
#   summarize(n(),
#             PERWT = sum(PERWT)) %>%
#   dplyr::rename(
#     COUNT ="n()"
#   )
# 
# SMTP_NYC_EDU_SUM2 <-
#   cps_data8_NYC %>%
#   filter(CIDATAPLN == 1) %>%
#   filter(CIHISPEED == 20) %>%
#   group_by(EDUC_RE, AGE_RE) %>%
#   summarize(n(),
#             PERWT = sum(PERWT)) %>%
#   dplyr::rename(
#     COUNT ="n()"
#   )
# 
# SMTP_NYC_EDU_SUM3_MERGED <- merge(SMTP_NYC_EDU_SUM1, SMTP_NYC_EDU_SUM2, by = c("EDUC_RE", "AGE_RE"), all.x = TRUE)
# 
# SMTP_NYC_EDU_SUM4_FIN <-
#   SMTP_NYC_EDU_SUM3_MERGED %>%
#   dplyr::rename(
#     TOTAL = "PERWT.x",
#     SMTP_XHISPD = "PERWT.y"
#   ) %>%
#   mutate(
#     PCT_SMTP_XHISPD = (SMTP_XHISPD/TOTAL) * 100
#   ) %>%
#   mutate_at(vars(PCT_SMTP_XHISPD), 
#             funs(round(., 1)))
# 
# #   MAN
# SMTP_MAN_EDU_SUM1 <-
#   cps_data8_MAN %>%
#   filter(CIDATAPLN == 1) %>%
#   group_by(EDUC_RE, AGE_RE) %>%
#   summarize(n()) %>%
#   dplyr::rename(
#     COUNT ="n()"
#   )
# 
# SMTP_MAN_EDU_SUM2 <-
#   cps_data8_MAN %>%
#   filter(CIDATAPLN == 1) %>%
#   filter(CIHISPEED == 20) %>%
#   group_by(EDUC_RE, AGE_RE) %>%
#   summarize(n()) %>%
#   dplyr::rename(
#     COUNT ="n()"
#   )
# 
# SMTP_MAN_EDU_SUM3_MERGED <- merge(SMTP_MAN_EDU_SUM1, SMTP_MAN_EDU_SUM2, by = c("EDUC_RE", "AGE_RE"), all.x = TRUE)
# 
# SMTP_MAN_EDU_SUM4_FIN <-
#   SMTP_MAN_EDU_SUM3_MERGED %>%
#   dplyr::rename(
#     TOTAL = "PERWT.x",
#     SMTP_XHISPD = "PERWT.y"
#   ) %>%
#   mutate(
#     PCT_SMTP_XHISPD = (SMTP_XHISPD/TOTAL) * 100
#   ) %>%
#   mutate_at(vars(PCT_SMTP_XHISPD), 
#             funs(round(., 1)))

#   4) Smartphone with Data Plan without Broadband Access by race, grouped by subregion

ipums_val_labels(cps_data8$CIDATAPLN)
ipums_val_labels(cps_data8$CIHISPEED)

#   RPA
SMTP_RPA_SUBREG_SUM1 <-
  cps_data8_RPA %>%
  #filter(CIDATAPLN == 1) %>%
  group_by(RACE_RE, SUBREG) %>%
  summarize(n(),
            PERWT = sum(PERWT)) %>%
  dplyr::rename(
    COUNT ="n()"
  )

SMTP_RPA_SUBREG_SUM2 <-
  cps_data8_RPA %>%
  filter(CIDATAPLN == 1) %>%
  filter(CIHISPEED == 20) %>%
  group_by(RACE_RE, SUBREG) %>%
  summarize(n(),
            PERWT = sum(PERWT)) %>%
  dplyr::rename(
    COUNT ="n()"
  )

SMTP_RPA_SUBREG_SUM3_MERGED <- merge(SMTP_RPA_SUBREG_SUM1, SMTP_RPA_SUBREG_SUM2, by = c("RACE_RE", "SUBREG"), all.x = TRUE)

SMTP_RPA_SUBREG_SUM4_FIN <-
  SMTP_RPA_SUBREG_SUM3_MERGED %>%
  dplyr::rename(
    TOTAL = "PERWT.x",
    SMTP_XHISPD = "PERWT.y"
  ) %>%
  mutate(
    PCT_SMTP_XHISPD = (SMTP_XHISPD/TOTAL) * 100
  ) %>%
  mutate_at(vars(PCT_SMTP_XHISPD), 
            funs(round(., 1)))

SMTP_RPA_SUBREG_SUM4_FIN %>%
  hchart(., type = "column", hcaes(x = SUBREG, y = PCT_SMTP_XHISPD, group = RACE_RE)) %>%
  hc_add_theme(hc_theme_rpa) %>%
  hc_yAxis(title = list(
    text = "% with cell phone data plan but no broadband"
  )) %>%
  hc_xAxis(title = list(
    text = "Subregion"
  )) %>%
  hc_title(text = "Percent of people with cell phone data plan but no broadband, by subregion and age") %>%
  hc_subtitle(text = "RPA Region") %>%
  hc_caption(text = "2019 ACS, IPUMS")

# #   NYC
# SMTP_NYC_SUBREG_SUM1 <-
#   cps_data8_NYC %>%
#   filter(CIDATAPLN == 1) %>%
#   group_by(RACE_RE, SUBREG) %>%
#   summarize(n()) %>%
#   dplyr::rename(
#     COUNT ="n()"
#   )
# 
# SMTP_NYC_SUBREG_SUM2 <-
#   cps_data8_NYC %>%
#   filter(CIDATAPLN == 1) %>%
#   filter(CIHISPEED == 20) %>%
#   group_by(RACE_RE, SUBREG) %>%
#   summarize(n()) %>%
#   dplyr::rename(
#     COUNT ="n()"
#   )
# 
# SMTP_NYC_SUBREG_SUM3_MERGED <- merge(SMTP_NYC_SUBREG_SUM1, SMTP_NYC_SUBREG_SUM2, by = c("RACE_RE", "SUBREG"), all.x = TRUE)
# 
# SMTP_NYC_SUBREG_SUM4_FIN <-
#   SMTP_NYC_SUBREG_SUM3_MERGED %>%
#   dplyr::rename(
#     TOTAL = "PERWT.x",
#     SMTP_XHISPD = "PERWT.y"
#   ) %>%
#   mutate(
#     PCT_SMTP_XHISPD = (SMTP_XHISPD/TOTAL) * 100
#   ) %>%
#   mutate_at(vars(PCT_SMTP_XHISPD), 
#             funs(round(., 1)))
# 
# 
# #   MAN
# SMTP_MAN_SUBREG_SUM1 <-
#   cps_data8_MAN %>%
#   filter(CIDATAPLN == 1) %>%
#   group_by(RACE_RE, SUBREG) %>%
#   summarize(n()) %>%
#   dplyr::rename(
#     COUNT ="n()"
#   )
# 
# SMTP_MAN_SUBREG_SUM2 <-
#   cps_data8_MAN %>%
#   filter(CIDATAPLN == 1) %>%
#   filter(CIHISPEED == 20) %>%
#   group_by(RACE_RE, SUBREG) %>%
#   summarize(n()) %>%
#   dplyr::rename(
#     COUNT ="n()"
#   )
# 
# SMTP_MAN_SUBREG_SUM3_MERGED <- merge(SMTP_MAN_SUBREG_SUM1, SMTP_MAN_SUBREG_SUM2, by = c("RACE_RE", "SUBREG"), all.x = TRUE)
# 
# SMTP_MAN_SUBREG_SUM4_FIN <-
#   SMTP_MAN_SUBREG_SUM3_MERGED %>%
#   dplyr::rename(
#     TOTAL = "PERWT.x",
#     SMTP_XHISPD = "PERWT.y"
#   ) %>%
#   mutate(
#     PCT_SMTP_XHISPD = (SMTP_XHISPD/TOTAL) * 100
#   ) %>%
#   mutate_at(vars(PCT_SMTP_XHISPD), 
#             funs(round(., 1)))
# 


#################################################################################


# 3. Create csv Files

# 1. Broadband Access

#   1) Broadband access by income quintile, grouped by age
write.csv(BRBD_RPA_INC_SUM4_FIN, file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/Tech_Equity_Report/Broadband_Access/1_Summary_Income_by_Age_RPA_csv.csv')
write.csv(BRBD_NYC_INC_SUM4_FIN, file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/Tech_Equity_Report/Broadband_Access/1_Summary_Income_by_Age_NYC_csv.csv')
write.csv(BRBD_MAN_INC_SUM4_FIN, file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/Tech_Equity_Report/Broadband_Access/1_Summary_Income_by_Age_MAN_csv.csv')

#   2) Broadband access by race, grouped by age
write.csv(BRBD_RPA_RACE_SUM4_FIN, file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/Tech_Equity_Report/Broadband_Access/2_Summary_Race_by_Age_RPA_csv.csv')
write.csv(BRBD_NYC_RACE_SUM4_FIN, file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/Tech_Equity_Report/Broadband_Access/2_Summary_Race_by_Age_NYC_csv.csv')
write.csv(BRBD_MAN_RACE_SUM4_FIN, file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/Tech_Equity_Report/Broadband_Access/2_Summary_Race_by_Age_MAN_csv.csv')

#   3) Broadband access by education level, grouped by age
write.csv(BRBD_RPA_EDUC_SUM4_FIN, file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/Tech_Equity_Report/Broadband_Access/3_Summary_Education_by_Age_RPA_csv.csv')
write.csv(BRBD_NYC_EDUC_SUM4_FIN, file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/Tech_Equity_Report/Broadband_Access/3_Summary_Education_by_Age_NYC_csv.csv')
write.csv(BRBD_MAN_EDUC_SUM4_FIN, file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/Tech_Equity_Report/Broadband_Access/3_Summary_Education_by_Age_MAN_csv.csv')

#   4) Broadband access by race, grouped by subregion
write.csv(BRBD_RPA_SUBREG_SUM4_FIN, file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/Tech_Equity_Report/Broadband_Access/4_Summary_Race_by_Subregion_RPA_csv.csv')
write.csv(BRBD_NYC_SUBREG_SUM4_FIN, file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/Tech_Equity_Report/Broadband_Access/4_Summary_Race_by_Subregion_NYC_csv.csv')
write.csv(BRBD_MAN_SUBREG_SUM4_FIN, file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/Tech_Equity_Report/Broadband_Access/4_Summary_Race_by_Subregion_MAN_csv.csv')



# 2. Smartphone with Data Plan without Broadband Access

#   1) Broadband access by income quintile, grouped by age
write.csv(SMTP_RPA_INC_SUM4_FIN, file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/Tech_Equity_Report/DataPlan_without_Braodband/1_Summary_Income_by_Age_RPA_csv.csv')
write.csv(SMTP_NYC_INC_SUM4_FIN, file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/Tech_Equity_Report/DataPlan_without_Braodband/1_Summary_Income_by_Age_NYC_csv.csv')
write.csv(SMTP_MAN_INC_SUM4_FIN, file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/Tech_Equity_Report/DataPlan_without_Braodband/1_Summary_Income_by_Age_MAN_csv.csv')

#   2) Broadband access by race, grouped by age
write.csv(SMTP_RPA_RAC_SUM4_FIN, file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/Tech_Equity_Report/DataPlan_without_Braodband/2_Summary_Race_by_Age_RPA_csv.csv')
write.csv(SMTP_NYC_RAC_SUM4_FIN, file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/Tech_Equity_Report/DataPlan_without_Braodband/2_Summary_Race_by_Age_NYC_csv.csv')
write.csv(SMTP_MAN_RAC_SUM4_FIN, file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/Tech_Equity_Report/DataPlan_without_Braodband/2_Summary_Race_by_Age_MAN_csv.csv')

#   3) Broadband access by education level, grouped by age
write.csv(SMTP_RPA_EDU_SUM4_FIN, file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/Tech_Equity_Report/DataPlan_without_Braodband/3_Summary_Education_by_Age_RPA_csv.csv')
write.csv(SMTP_NYC_EDU_SUM4_FIN, file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/Tech_Equity_Report/DataPlan_without_Braodband/3_Summary_Education_by_Age_NYC_csv.csv')
write.csv(SMTP_MAN_EDU_SUM4_FIN, file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/Tech_Equity_Report/DataPlan_without_Braodband/3_Summary_Education_by_Age_MAN_csv.csv')

#   4) Broadband access by race, grouped by subregion
write.csv(SMTP_RPA_SUBREG_SUM4_FIN, file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/Tech_Equity_Report/DataPlan_without_Braodband/4_Summary_Race_by_Subregion_RPA_csv.csv')
write.csv(SMTP_NYC_SUBREG_SUM4_FIN, file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/Tech_Equity_Report/DataPlan_without_Braodband/4_Summary_Race_by_Subregion_NYC_csv.csv')
write.csv(SMTP_MAN_SUBREG_SUM4_FIN, file = 'G:/Shared drives/Projects/5035_Tech Equity/Data/A_CSV/Tech_Equity_Report/DataPlan_without_Braodband/4_Summary_Race_by_Subregion_MAN_csv.csv')



#################################################################################

# 4. Visualization - Create Grouped Bar Charts

# 1. Broadband Access
#   1) Broadband access by income quintile, grouped by age
#   RPA
ggplot(BRBD_RPA_INC_SUM4_FIN, aes(fill = AGE_RE, y = PCT_BRDBD, x = HHINCOME_RE)) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_cartesian(ylim=c(70, 100)) +
  labs(y = "Percentage (%)", x = "Income Quintile")
#   NYC
ggplot(BRBD_NYC_INC_SUM4_FIN, aes(fill = AGE_RE, y = PCT_BRDBD, x = HHINCOME_RE)) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_cartesian(ylim=c(70, 100)) +
  labs(y = "Percentage (%)", x = "Income Quintile")
#   MAN
ggplot(BRBD_MAN_INC_SUM4_FIN, aes(fill = AGE_RE, y = PCT_BRDBD, x = HHINCOME_RE)) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_cartesian(ylim=c(70, 100)) +
  labs(y = "Percentage (%)", x = "Income Quintile") 


#   2) Broadband access by race, grouped by age
#   RPA
ggplot(BRBD_RPA_RACE_SUM4_FIN, aes(fill = AGE_RE, y = PCT_BRDBD, x = RACE_RE)) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_cartesian(ylim=c(70, 100)) +
  labs(y = "Percentage (%)", x = "Race")
#   NYC
ggplot(BRBD_NYC_RACE_SUM4_FIN, aes(fill = AGE_RE, y = PCT_BRDBD, x = RACE_RE)) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_cartesian(ylim=c(70, 100)) +
  labs(y = "Percentage (%)", x = "Race")
#   MAN
ggplot(BRBD_MAN_RACE_SUM4_FIN, aes(fill = AGE_RE, y = PCT_BRDBD, x = RACE_RE)) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_cartesian(ylim=c(70, 100)) +
  labs(y = "Percentage (%)", x = "Race")


#   3) Broadband access by education level, grouped by age
#   RPA
ggplot(BRBD_RPA_EDUC_SUM4_FIN, aes(fill = AGE_RE, y = PCT_BRDBD, x = EDUC_RE)) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_cartesian(ylim=c(70, 100)) +
  labs(y = "Percentage (%)", x = "Race") 
#   NYC
ggplot(BRBD_NYC_EDUC_SUM4_FIN, aes(fill = AGE_RE, y = PCT_BRDBD, x = EDUC_RE)) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_cartesian(ylim=c(70, 100)) +
  labs(y = "Percentage (%)", x = "Race")
#   MAN
ggplot(BRBD_MAN_EDUC_SUM4_FIN, aes(fill = AGE_RE, y = PCT_BRDBD, x = EDUC_RE)) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_cartesian(ylim=c(70, 100)) +
  labs(y = "Percentage (%)", x = "Race")


#   4) Broadband access by race, grouped by subregion
#   RPA
ggplot(BRBD_RPA_SUBREG_SUM4_FIN, aes(fill = SUBREG, y = PCT_BRDBD, x = RACE_RE)) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_cartesian(ylim=c(80, 95)) +
  labs(y = "Percentage (%)", x = "Race") 
#   NYC
ggplot(BRBD_NYC_SUBREG_SUM4_FIN, aes(fill = SUBREG, y = PCT_BRDBD, x = RACE_RE)) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_cartesian(ylim=c(70, 100)) +
  labs(y = "Percentage (%)", x = "Race") 
#   MAN
ggplot(BRBD_MAN_SUBREG_SUM4_FIN, aes(fill = SUBREG, y = PCT_BRDBD, x = RACE_RE)) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_cartesian(ylim=c(70, 100)) +
  labs(y = "Percentage (%)", x = "Race") 





# 2. Smartphone with Data Plan without Broadband Access

colnames(SMTP_RPA_INC_SUM4_FIN)
#   1) Smartphone with Data Plan without Broadband Access by income quintile, grouped by age
#   RPA
ggplot(SMTP_RPA_INC_SUM4_FIN, aes(fill = AGE_RE, y = PCT_SMTP_XHISPD, x = HHINCOME_RE)) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_cartesian(ylim=c(0, 30)) +
  labs(y = "Percentage (%)", x = "Income Quintile")
#   NYC
ggplot(SMTP_NYC_INC_SUM4_FIN, aes(fill = AGE_RE, y = PCT_SMTP_XHISPD, x = HHINCOME_RE)) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_cartesian(ylim=c(0, 30)) +
  labs(y = "Percentage (%)", x = "Income Quintile")
#   MAN
ggplot(SMTP_MAN_INC_SUM4_FIN, aes(fill = AGE_RE, y = PCT_SMTP_XHISPD, x = HHINCOME_RE)) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_cartesian(ylim=c(0, 30)) +
  labs(y = "Percentage (%)", x = "Income Quintile") 


#   2) Smartphone with Data Plan without Broadband Access by race, grouped by age
#   RPA
ggplot(SMTP_RPA_RAC_SUM4_FIN, aes(fill = AGE_RE, y = PCT_SMTP_XHISPD, x = RACE_RE)) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_cartesian(ylim=c(0, 30)) +
  labs(y = "Percentage (%)", x = "Race")
#   NYC
ggplot(SMTP_NYC_RAC_SUM4_FIN, aes(fill = AGE_RE, y = PCT_SMTP_XHISPD, x = RACE_RE)) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_cartesian(ylim=c(0, 30)) +
  labs(y = "Percentage (%)", x = "Race")
#   MAN
ggplot(SMTP_MAN_RAC_SUM4_FIN, aes(fill = AGE_RE, y = PCT_SMTP_XHISPD, x = RACE_RE)) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_cartesian(ylim=c(0, 30)) +
  labs(y = "Percentage (%)", x = "Race")


#   3) Smartphone with Data Plan without Broadband Access by education level, grouped by age
#   RPA
ggplot(SMTP_RPA_EDU_SUM4_FIN, aes(fill = AGE_RE, y = PCT_SMTP_XHISPD, x = EDUC_RE)) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_cartesian(ylim=c(0, 30)) +
  labs(y = "Percentage (%)", x = "Race") 
#   NYC
ggplot(SMTP_NYC_EDU_SUM4_FIN, aes(fill = AGE_RE, y = PCT_SMTP_XHISPD, x = EDUC_RE)) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_cartesian(ylim=c(0, 30)) +
  labs(y = "Percentage (%)", x = "Race")
#   MAN
ggplot(SMTP_MAN_EDU_SUM4_FIN, aes(fill = AGE_RE, y = PCT_SMTP_XHISPD, x = EDUC_RE)) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_cartesian(ylim=c(0, 30)) +
  labs(y = "Percentage (%)", x = "Race")


#   4) Smartphone with Data Plan without Broadband Access by race, grouped by subregion
#   RPA
ggplot(SMTP_RPA_SUBREG_SUM4_FIN, aes(fill = SUBREG, y = PCT_SMTP_XHISPD, x = RACE_RE)) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_cartesian(ylim=c(0, 25)) +
  labs(y = "Percentage (%)", x = "Race") 
#   NYC
ggplot(SMTP_NYC_SUBREG_SUM4_FIN, aes(fill = SUBREG, y = PCT_SMTP_XHISPD, x = RACE_RE)) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_cartesian(ylim=c(0, 30)) +
  labs(y = "Percentage (%)", x = "Race") 
#   MAN
ggplot(SMTP_MAN_SUBREG_SUM4_FIN, aes(fill = SUBREG, y = PCT_SMTP_XHISPD, x = RACE_RE)) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_cartesian(ylim=c(0, 30)) +
  labs(y = "Percentage (%)", x = "Race") 

colnames(SMTP_RPA_SUBREG_SUM4_FIN)


# + theme(legend.position = "none")

#################################################################################

