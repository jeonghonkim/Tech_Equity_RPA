

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


#################################################################################


# 1. Create datasets by 3 different geographies (PRA Region, New York City, Manhattan)

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
      AGE > 64 ~ "Ag4_Und65"
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
    )
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
      AGE > 64 ~ "Ag4_Und65"
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
    )
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
      AGE > 64 ~ "Ag4_Und65"
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
      GEOID == 09001 ~ "5_Connecticut", # 5. Connecticut
      GEOID == 09009 ~ "5_Connecticut", 
      GEOID == 34003 ~ "4_New Jersey", # 4. New Jersey 
      GEOID == 34013 ~ "4_New Jersey",
      GEOID == 34017 ~ "4_New Jersey", 
      GEOID == 34019 ~ "4_New Jersey", 
      GEOID == 34021 ~ "4_New Jersey",
      GEOID == 34023 ~ "4_New Jersey",
      GEOID == 34025 ~ "4_New Jersey",
      GEOID == 34027 ~ "4_New Jersey", 
      GEOID == 34029 ~ "4_New Jersey",
      GEOID == 34031 ~ "4_New Jersey",
      GEOID == 34035 ~ "4_New Jersey",
      GEOID == 34037 ~ "4_New Jersey",
      GEOID == 34039 ~ "4_New Jersey",
      GEOID == 34041 ~ "4_New Jersey",
      GEOID == 36005 ~ "1_New York City", # 1. New York City
      GEOID == 36027 ~ "3_Other New York", # 3. Other New York
      GEOID == 36047 ~ "1_New York City", 
      GEOID == 36059 ~ "2_Long Island", # 2. Long Island
      GEOID == 36061 ~ "1_New York City", 
      GEOID == 36071 ~ "3_Other New York",
      GEOID == 36079 ~ "3_Other New York",
      GEOID == 36081 ~ "1_New York City",
      GEOID == 36085 ~ "1_New York City", 
      GEOID == 36087 ~ "3_Other New York", 
      GEOID == 36103 ~ "2_Long Island", 
      GEOID == 36119 ~ "3_Other New York"
    )
  )




HHINCOME_RE = ntile(HHINCOME,5)

#################################################################################
View(cps_data8)
colnames(cps_data8)
ipums_val_labels(cps_data8$RACE)
ipums_val_labels(cps_data8$HISPAN)
ipums_val_labels(cps_data8$CIHISPEED)
ipums_val_labels(cps_data8$EDUC)
ipums_var_desc(cps_data8, "RACE")

# 2. Summarize datasets
#   1) Broadband access by income quintile, grouped by age




#   2) Broadband access by race, grouped by age

# RPA Level
data8_RPA_SUM1 <-
  cps_data8_RPA %>%
  group_by(RACE_RE, AGE_RE) %>%
  summarize(n()) %>%
  dplyr::rename(
    COUNT ="n()"
  )

data8_RPA_SUM2 <-
  cps_data8_RPA %>%
  filter(CIHISPEED == 10) %>%
  group_by(RACE_RE, AGE_RE) %>%
  summarize(n()) %>%
  dplyr::rename(
    COUNT ="n()"
  )

data8_RPA_SUM12 <- merge(data8_RPA_SUM, data8_RPA_SUM2, by = c("RACE_RE", "AGE_RE"), all.x = TRUE)

data8_RPA_SUMfin <-
data8_RPA_SUM12 %>%
  dplyr::rename(
    CNT_TOTAL = "COUNT.x",
    CNT_BRDBD = "COUNT.y"
  ) %>%
  mutate(
    PCT_BRDBD = (CNT_BRDBD/CNT_TOTAL) * 100
  ) %>%
  mutate_at(vars(PCT_BRDBD), 
            funs(round(., 1)))

# NYC Level
data8_NYC_SUM1 <-
  cps_data8_NYC %>%
  group_by(RACE_RE, AGE_RE) %>%
  summarize(n()) %>%
  dplyr::rename(
    COUNT ="n()"
  )

data8_NYC_SUM2 <-
  cps_data8_NYC %>%
  filter(CIHISPEED == 10) %>%
  group_by(RACE_RE, AGE_RE) %>%
  summarize(n()) %>%
  dplyr::rename(
    COUNT ="n()"
  )

data8_NYC_SUM12 <- merge(data8_NYC_SUM, data8_NYC_SUM2, by = c("RACE_RE", "AGE_RE"), all.x = TRUE)

data8_NYC_SUMfin <-
  data8_NYC_SUM12 %>%
  dplyr::rename(
    CNT_TOTAL = "COUNT.x",
    CNT_BRDBD = "COUNT.y"
  ) %>%
  mutate(
    PCT_BRDBD = (CNT_BRDBD/CNT_TOTAL) * 100
  ) %>%
  mutate_at(vars(PCT_BRDBD), 
            funs(round(., 1)))

# Manhattan Level
data8_MAN_SUM1 <-
  cps_data8_MAN %>%
  group_by(RACE_RE, AGE_RE) %>%
  summarize(n()) %>%
  dplyr::rename(
    COUNT ="n()"
  )

data8_MAN_SUM2 <-
  cps_data8_MAN %>%
  filter(CIHISPEED == 10) %>%
  group_by(RACE_RE, AGE_RE) %>%
  summarize(n()) %>%
  dplyr::rename(
    COUNT ="n()"
  )

data8_MAN_SUM12 <- merge(data8_MAN_SUM, data8_MAN_SUM2, by = c("RACE_RE", "AGE_RE"), all.x = TRUE)

data8_MAN_SUMfin <-
  data8_MAN_SUM12 %>%
  dplyr::rename(
    CNT_TOTAL = "COUNT.x",
    CNT_BRDBD = "COUNT.y"
  ) %>%
  mutate(
    PCT_BRDBD = (CNT_BRDBD/CNT_TOTAL) * 100
  ) %>%
  mutate_at(vars(PCT_BRDBD), 
            funs(round(., 1)))



#   3) Broadband access by education level, grouped by age

# RPA Level
data8_RPA_EDUC_SUM1 <-
  cps_data8_RPA %>%
  group_by(EDUC_RE, AGE_RE) %>%
  summarize(n()) %>%
  dplyr::rename(
    COUNT ="n()"
  )

data8_RPA_EDUC_SUM2 <-
  cps_data8_RPA %>%
  filter(CIHISPEED == 10) %>%
  group_by(EDUC_RE, AGE_RE) %>%
  summarize(n()) %>%
  dplyr::rename(
    COUNT ="n()"
  )

data8_RPA_EDUC_SUM12 <- merge(data8_RPA_EDUC_SUM1, data8_RPA_EDUC_SUM2, by = c("EDUC_RE", "AGE_RE"), all.x = TRUE)

data8_RPA_EDUC_SUMfin <-
  data8_RPA_EDUC_SUM12 %>%
  dplyr::rename(
    CNT_TOTAL = "COUNT.x",
    CNT_BRDBD = "COUNT.y"
  ) %>%
  mutate(
    PCT_BRDBD = (CNT_BRDBD/CNT_TOTAL) * 100
  ) %>%
  mutate_at(vars(PCT_BRDBD), 
            funs(round(., 1)))

# NYC Level
data8_NYC_EDUC_SUM1 <-
  cps_data8_NYC %>%
  group_by(EDUC_RE, AGE_RE) %>%
  summarize(n()) %>%
  dplyr::rename(
    COUNT ="n()"
  )

data8_NYC_EDUC_SUM2 <-
  cps_data8_NYC %>%
  filter(CIHISPEED == 10) %>%
  group_by(EDUC_RE, AGE_RE) %>%
  summarize(n()) %>%
  dplyr::rename(
    COUNT ="n()"
  )

data8_NYC_EDUC_SUM12 <- merge(data8_NYC_EDUC_SUM1, data8_NYC_EDUC_SUM2, by = c("EDUC_RE", "AGE_RE"), all.x = TRUE)

data8_NYC_EDUC_SUMfin <-
  data8_NYC_EDUC_SUM12 %>%
  dplyr::rename(
    CNT_TOTAL = "COUNT.x",
    CNT_BRDBD = "COUNT.y"
  ) %>%
  mutate(
    PCT_BRDBD = (CNT_BRDBD/CNT_TOTAL) * 100
  ) %>%
  mutate_at(vars(PCT_BRDBD), 
            funs(round(., 1)))

# MANHATTAN Level
data8_MAN_EDUC_SUM1 <-
  cps_data8_MAN %>%
  group_by(EDUC_RE, AGE_RE) %>%
  summarize(n()) %>%
  dplyr::rename(
    COUNT ="n()"
  )

data8_MAN_EDUC_SUM2 <-
  cps_data8_MAN %>%
  filter(CIHISPEED == 10) %>%
  group_by(EDUC_RE, AGE_RE) %>%
  summarize(n()) %>%
  dplyr::rename(
    COUNT ="n()"
  )

data8_MAN_EDUC_SUM12 <- merge(data8_MAN_EDUC_SUM1, data8_MAN_EDUC_SUM2, by = c("EDUC_RE", "AGE_RE"), all.x = TRUE)

data8_MAN_EDUC_SUMfin <-
  data8_MAN_EDUC_SUM12 %>%
  dplyr::rename(
    CNT_TOTAL = "COUNT.x",
    CNT_BRDBD = "COUNT.y"
  ) %>%
  mutate(
    PCT_BRDBD = (CNT_BRDBD/CNT_TOTAL) * 100
  ) %>%
  mutate_at(vars(PCT_BRDBD), 
            funs(round(., 1)))



#   4) Broadband access by race, grouped by subregion


# MANHATTAN Level
data8_MAN_SUBREG_SUM1 <-
  cps_data8_MAN %>%
  group_by(RACE_RE, SUBREG) %>%
  summarize(n()) %>%
  dplyr::rename(
    COUNT ="n()"
  )

data8_MAN_SUBREG_SUM2 <-
  cps_data8_MAN %>%
  filter(CIHISPEED == 10) %>%
  group_by(RACE_RE, SUBREG) %>%
  summarize(n()) %>%
  dplyr::rename(
    COUNT ="n()"
  )

data8_MAN_SUBREG_SUM12 <- merge(data8_MAN_SUBREG_SUM1, data8_MAN_SUBREG_SUM2, by = c("RACE_RE", "SUBREG"), all.x = TRUE)

data8_MAN_SUBREG_SUMfin <-
  data8_MAN_SUBREG_SUM12 %>%
  dplyr::rename(
    CNT_TOTAL = "COUNT.x",
    CNT_BRDBD = "COUNT.y"
  ) %>%
  mutate(
    PCT_BRDBD = (CNT_BRDBD/CNT_TOTAL) * 100
  ) %>%
  mutate_at(vars(PCT_BRDBD), 
            funs(round(., 1)))






#################################################################################

# Visualization - Createing Grouped Bar Charts


#   2) Broadband access by race, grouped by age
ggplot(data8_RPA_SUMfin, aes(fill = AGE_RE, y = PCT_BRDBD, x = RACE_RE)) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_cartesian(ylim=c(45, 95)) +
  labs(y = "Percentage (%)", x = "Race") +
  theme(legend.position = "none")

ggplot(data8_NYC_SUMfin, aes(fill = AGE_RE, y = PCT_BRDBD, x = RACE_RE)) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_cartesian(ylim=c(45, 95)) +
  labs(y = "Percentage (%)", x = "Race") +
  theme(legend.position = "none")

ggplot(data8_MAN_SUMfin, aes(fill = AGE_RE, y = PCT_BRDBD, x = RACE_RE)) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_cartesian(ylim=c(45, 95)) +
  labs(y = "Percentage (%)", x = "Race")



#   3) Broadband access by education level, grouped by age
# MANHATTAN
ggplot(data8_RPA_EDUC_SUMfin, aes(fill = AGE_RE, y = PCT_BRDBD, x = EDUC_RE)) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_cartesian(ylim=c(45, 95)) +
  labs(y = "Percentage (%)", x = "Race") +
  theme(legend.position = "none")
# NYC
ggplot(data8_NYC_EDUC_SUMfin, aes(fill = AGE_RE, y = PCT_BRDBD, x = EDUC_RE)) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_cartesian(ylim=c(45, 95)) +
  labs(y = "Percentage (%)", x = "Race") +
  theme(legend.position = "none")
# RPA
ggplot(data8_MAN_EDUC_SUMfin, aes(fill = AGE_RE, y = PCT_BRDBD, x = EDUC_RE)) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_cartesian(ylim=c(45, 95)) +
  labs(y = "Percentage (%)", x = "Race") +
  theme(legend.position = "none")



#   4) Broadband access by education level, grouped by age
# MANHATTAN
ggplot(data8_MAN_SUBREG_SUMfin, aes(fill = SUBREG, y = PCT_BRDBD, x = RACE_RE)) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_cartesian(ylim=c(45, 95)) +
  labs(y = "Percentage (%)", x = "Race") +
  theme(legend.position = "none")
# NYC

# RPA