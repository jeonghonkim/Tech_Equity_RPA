
# June 9. 2021
# 2. Scenarios Planning

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
setwd("G:/Shared drives/Projects/5035_Tech Equity/Data")

#################################################################################

# 1. Aggregate LINKED_Public.csv data by:
#     1) Origin Tract (OTRACT)
#     2) Destination Tract (DTRACT)
#     3) Time of Departure (TOD_R)
#     4) Primary Mode, short version (PMODE1)
#           - Use rts_variable_lookup to recode
#     6) Origin to Destination Primary Trip Purpose (ODTPURP)
#     7) Household Income Range (INCOM_R)

# Use the aggregated data to generate some summary tables:
#     1) Work and Non-work Trips to Manhattan (36061) by Origin County
#     2) Number of Auto, Transit, Other trips by Origin County
#     3) Number of Auto, Transit, Other trips by Destination County
#     4) Number of Auto, Transit, Other trips by Household Income by RPA Subregion


# Read LINKED_Public.csv
Linked_Public <- read.csv(file = 'G:/Shared drives/Projects/525_Workforce/Post-Covid Recovery/Data/NYMTC RTS/LINKED_Public.csv')
Relabel <- read.csv(file = 'G:/Shared drives/Projects/525_Workforce/Post-Covid Recovery/Data/NYMTC RTS/rts_variable_lookup.csv')

View(Linked_Public)
colnames(Linked_Public)
summary(Linked_Public)

# Aggregate Example
Linked_Public_Sub1 <-
Linked_Public %>%
  select(OTRACT, DTRACT, TOD_R, PMODE1, ODTPURP, INCOM_R) %>%
  mutate(PMODE1_REL1 = case_when(
    PMODE1 == 1 ~ 2, # 2. Bus
    PMODE1 == 2 ~ 1, # 1. Auto
    PMODE1 == 3 ~ 4, # 4. Commuter Rail
    PMODE1 == 4 ~ 2,
    PMODE1 == 5 ~ 5, # 5. Ferry
    PMODE1 == 6 ~ 3, # 3. Rail
    PMODE1 == 7 ~ 3,
    PMODE1 == 8 ~ 2,
    PMODE1 == 9 ~ 2,
    PMODE1 == 10 ~ 1,
    PMODE1 == 11 ~ 1,
    PMODE1 == 12 ~ 6, # 6. Bike
    PMODE1 == 13 ~ 7, # 7. Walk
    PMODE1 == 97 ~ 8  # 8. Other
    ),
    PMODE1_REL2 = case_when(
      PMODE1 == 1 ~ 2, # 2. Transit
      PMODE1 == 2 ~ 1, # 1. Auto
      PMODE1 == 3 ~ 2, 
      PMODE1 == 4 ~ 2,
      PMODE1 == 5 ~ 2, 
      PMODE1 == 6 ~ 2, 
      PMODE1 == 7 ~ 2,
      PMODE1 == 8 ~ 2,
      PMODE1 == 9 ~ 2,
      PMODE1 == 10 ~ 1,
      PMODE1 == 11 ~ 1,
      PMODE1 == 12 ~ 3, # 3. Other
      PMODE1 == 13 ~ 3, 
      PMODE1 == 97 ~ 3  
    ))






Linked_Public_Sub1 %>%
  select(PMODE1) %>%
  summarize()








#################################################################################

# Data Description
# 1.  PLSAM      Place Unique Identifier  11 digits wide = SAMPN + PERNO + PLANO
# 2.  SAMPN      Household Identifier     7 digits wide
# 3.  PERNO      Person Number            2 digits wide
# 4.  PLANO      Place Number             2 digits wide
# 5.  PERTYPE    Person Type Code         e.g. Full Time, Part Time, University Student...
# 6.  ORIG_HOME  If Home is Origin        0 - No / 1 - Yes
# 7.  GTYPE      GPS Record Type          1 - GPS / 2 - Non-GPS
# 8.  HHSIZE     Household Size
# 9.  DEST_HOME  If Home is Destination   0- No / 1 - Yes
# 10. LTRIPNO    Linked Trip Number
# 11. TOUR_ID    Tour ID  
# 12. SUBT_ID    Subtour ID               
# 13. DTYPE      Destination Type Code    e.g. Primary destination, Outbound stop, Inbound Stop or Home...
# 14. TOUR_PURP  Tour Purpose             e.g. Home, Work, University, School...
# 15. DOW        Day of Week of Travel    e.g. Monday, Tuesday, Wednesday...
# 16. HTAZ       Home TAZ (NYMTC)
# 17. HTRACT     Home Census Tract (2010)
# 18. OTAZ       Origin TAZ (NYMTC; WGS84)
# 19. DTAZ       Destination TAZ (NYMTC; WGS84)
# 20. NO_TAZ     No TAZ flag for origin and destination       e.g. 0 - TAZ in both origin and destination / 1 - TAZ in either origin and destination / 2 - No TAZ in both origin and destination
# 21. OTPURP     Origin Primary Trip Purpose
# 22. DTPURP     Destination Primary Trip Purpose    
# 23. OTPURP_AGG    Origin Primary Trip Purpose Aggregated
# 24. DTPURP_AGG    Destination Primary  Trip Purpose Aggregated
# 25. DTPURP2       Secondary Trip Purpose
# 26. LTMODE_AGG    Aggregate Linked Trip Mode
# 27. MODE_SAMP     Linked Trip Mode Code
# 28. TOTTR         Total Number of People in the Trip
# 29. HHMEM         Household Members
# 30. PER1          Person Number on Trip
# 31. PER2          Person Number on Trip
# 32. PER3          Person Number on Trip
# 33. PER4          Person Number on Trip
# 34. PER5          Person Number on Trip
# 35. NONHH         Non-Household Member
# 36. VEHNO         Vehicle Number
# 37. DYGOV         Exit Vehicle      
# 38. PLOC          Parking Location
# 39. PRKTY         Parking Description
# 40. PAYPK         Pay to Park
# 41. PKAMT         Pay to Park Amount
# 42. PKUNT         Pay Unit
# 43. TOLFT         
# 44. TLONB         
# 45. TLFC1         
# 46. TOPN1   
# 47. TOLE1         
# 48. TOLX1         
# 49. TLLC1        
# 50. TLFR1         
# 51. TLFC2         
# 52. TOPN2         
# 53. TOLE2         
# 54. TOLX2         
# 55. TLLC2         
# 56. TLFR2        
# 57. TLFC3         
# 58. TOPN3         
# 59. TOLE3         
# 60. TOLX3         
# 61. TLLC3         
# 62. TLFR3         
# 63. O_TLFR3      
# 64. ROUTE         
# 65. SERVC         
# 66. FARE          
# 67. FAREC         
# 68. BUSPS         
# 69. MTABP 
# 70. BPFAR        
# 71. FRBAS
# 72. TRP_DEP_HR
# 73. TRP_DEP_MIN
# 74. TRP_ARR_HR
# 75. TRP_ARR_MIN
# 76. TRPDUR
# 77. ACTDUR       
# 78. TRIPDIST      
# 79. TRPDIST_HN    
# 80. TRPDUR_HN     
        # 81. OTRACT        Origin Tract 2010 (WGS84)
        # 82. DTRACT        Destination Tract 2010 (WGS84)
        # 83. OMCD          Origin MCD
        # 84. DMCD          Destination MCD
        # 85. OZIP          Origin Zip
        # 86. DZIP          Destination Zip
# 87. UNIQUEID2
# 88. UNIQUEID3
# 89. UNIQUEID4
# 90. HH1
# 91. HH_WHT2      
# 92. TOURFAC
# 93. WHT_FAC3
# 94. WHT_FAC3_VOCC
        # 95. HCOUNTY       Home County FIPS
# 96. HCITY_MCD
# 97. HSTATE
# 98. HZIP         
# 99. HNYC
# 100. HMPO 
# 101. ONYC
# 102. OMPO
# 103. DNYC
# 104. DMPO
# 105. GEO_GROUP1   
# 106. GEO_GROUP2
# 107. GEO_GROUP3
# 108. GEO_GROUP1_O
# 109. GEO_GROUP2_O
# 110. GEO_GROUP3_O
# 111. GEO_GROUP1_D
# 112. GEO_GROUP2_D 
# 113. GEO_GROUP3_D
# 114. ADJ_COUNTY    
# 115. OCOUNTY  
# 116. DCOUNTY  
# 117. PMODE
# 118. PMODE1       Primary Mode        e.g. School Bus, Raxi or Group Ride, Commuter Rail....
# 119. PAMODE     
# 120. PMODE_R
# 121. PMODE_R2
# 122. PMODE_R3
# 123. WORK_PURP
# 124. DTPURP_R
# 125. ODTPURP      Origin to Destination Primary Trip Purpose (Aggregate)        e.g. Home, Work, School, Others...
# 126. ODTPURP1     
# 127. ODTPURP2
# 128. ODTPURP2_R
# 129. WORKTRIP
# 130. TOD_R        Time of Departure Range       e.g. AM PEAK, MIDDAY, PM PEAK...
# 131. TOD_R1
# 132. TOD_PEAK
# 133. TOTTR_R      
# 134. TRPDUR_R
# 135. TRIPDIST_R1
# 136. TRIPDIST_R2
# 137. HHSIZ_R
# 138. INCOM_R      Household Income Range        e.g. Below $30K, $30K-74.9K, $75K-99.9K, ...
# 139. HHVEH_R      

