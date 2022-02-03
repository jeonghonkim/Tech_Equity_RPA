

# Tech Equity Initiative
#   1) Creating IPUMS layers
#       https://usa.ipums.org/usa/
#       Sample: USA, ACS 2019 1-YEAR AND ACS 2014-2019 5-YEAR
#       Variables: CINETHH, CISMRTPHN, CIDATAPLN, and CIHISPEED


# rm(list=ls())
# install.packages("devtools")
# install_github("r-spatial/sf")
# install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
#                   "ggspatial", "libwgeom", "rnaturalearth", "rnaturalearthdata"))

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
})
theme_set(theme_bw())

# vignette("value-labels", package = "ipumsr")
# vignette("ipums-geography", package = "ipumsr")
# vignette("ipums-cps", package = "ipumsr")
# vignette("ipums-nhgis", package = "ipumsr")
# vignette("ipums-terra", package = "ipumsr")

# Set file path
setwd("G:/Shared drives/Projects/5035_Tech Equity/Data")

  

# 1. Read IPUMS ddi file
cps_ddi3 <- read_ipums_ddi("usa_00003.xml")
cps_data3 <- read_ipums_micro(cps_ddi3, verbose = FALSE)
cps_ddi4 <- read_ipums_ddi("usa_00004.xml")
cps_data4 <- read_ipums_micro(cps_ddi4, verbose = FALSE)



# 2. Explores data
View(cps_data3)
View(cps_data4)

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
ipums_var_desc(cps_data4, "EDUC")
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
names(cps_data3)[9] <- "CountyFIPS"
cps_data4$COUNTYFIP = stri_pad_left(cps_data4$COUNTYFIP, 3, "0")
names(cps_data4)[9] <- "CountyFIPS"

#     2) PUMAs
# Creating leadning zeros in cps_data$, and change column names same as Geo_PUMAs
cps_data3$PUMA = stri_pad_left(cps_data3$PUMA, 5, "0")
names(cps_data3)[10] <- "PUMACE10"
cps_data4$PUMA = stri_pad_left(cps_data4$PUMA, 5, "0")
names(cps_data4)[10] <- "PUMACE10"
colnames(cps_data3)
colnames(cps_data4)

# Check the count # of CountyFIPS Column & PUMACE10 Column
count_counties3 <- cps_data3 %>%
  count(CountyFIPS)
count_counties4 <- cps_data4 %>%
  count(CountyFIPS)
count_counties <- merge(count_counties3, count_counties4, by = "CountyFIPS")
View(count_counties)

count_PUMAs3 <- cps_data3 %>%
  count(PUMACE10)
count_PUMAs4 <- cps_data4 %>%
  count(PUMACE10)
count_PUMAs <- merge(count_PUMAs3, count_PUMAs4, by = "PUMACE10")
View(count_PUMAs)



#   4-2. Aggregate based on the geographies
#     1) Counties
cps_data3_agg_Counties <- aggregate(list(cps_data3$CINETHH, cps_data3$CISMRTPHN, cps_data3$CIDATAPLN, cps_data3$CIHISPEED), 
                                  by = list(cps_data3$CountyFIPS),
                                  FUN = sum, na.rm = TRUE) 
NewNames_Counties <- c('CountyFIPS', 'CINETHH', 'CISMRTPHN', 'CIDATAPLN', 'CIHISPEED')
names(cps_data3_agg_Counties) <- NewNames_Counties
View(cps_data3_agg_Counties)

#         a. Make a aggregated subset data for new variables with Personal-level: AGE
cps_data4_subset_personal_Counties <- aggregate(list(cps_data4$AGE), 
                                    by = list(cps_data4$CountyFIPS),
                                    FUN = median, na.rm = TRUE) 
NewNames_personal_Counties <- c('CountyFIPS', 'AGE')
names(cps_data4_subset_personal_Counties) <- NewNames_personal_Counties
View(cps_data4_subset_personal_Counties)






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



##############################################################################

# CPS data 4

# Cleaning CPS data 4 variables based on the geographies

# 1) Personal-level
#     Did in the previous steps

# 2) Household-level
#   A. Household Income
# Select only the household (PERNUM = 1)
colnames(cps_data4)
cps_data4_subset_household <- cps_data4 %>% 
  filter(  PERNUM == "1")
View(cps_data4_subset_household)


# Need to handle NA values 
cps_data4_subset_household_Counties <- aggregate(list(cps_data4$OWNCOST, cps_data4$RENT, cps_data4$HHINCOME), 
                                                by = list(cps_data4$CountyFIPS),
                                                FUN = median, na.rm = TRUE) 
View(cps_data4_subset_household_Counties)
NewNames_household_Counties <- c('CountyFIPS', 'AGE')
names(cps_data4_subset_personal_Counties) <- NewNames_personal_Counties
View(cps_data4_subset_personal_Counties)
  


##############################################################################


# IPUMS data practices

x_cps_ddi <- read_ipums_ddi(ipums_example("cps_00006.xml"))
x_cps_data <- read_ipums_micro(x_cps_ddi, verbose = FALSE)
x_cps_data %>%
  select_if(is.labelled)
ipums_val_labels(x_cps_data$STATEFIP)
x_cps_data1 <- x_cps_data %>%
  mutate(STATE_factor = as_factor(lbl_clean(STATEFIP)))
table(x_cps_data1$STATE_factor, useNA = "always")
x_cps_data2 <- x_cps_data1 %>%
  mutate(STATE_factor2 = as_factor(ifelse(STATEFIP == 19, NA, STATEFIP)))
x_cps_data3 <- x_cps_data2 %>%
  mutate(STATE_factor3 = as_factor(lbl_na_if(STATEFIP, ~.val == 19)))
x_cps_data4 <- x_cps_data3 %>%
  mutate(STATE_factor4 = droplevels(as_factor(STATEFIP, levels = "both")))
table(x_cps_data4$STATE_factor4, useNA = "always")

ipums_var_desc(x_cps_data4$YEAR)
x_cps_data5 <- x_cps_data4 %>%
  mutate(YEAR = ifelse(YEAR == 1962, 62, NA))
ipums_var_desc(x_cps_data5$YEAR)

ipums_var_desc(x_cps_ddi, "YEAR")
ipums_file_info(x_cps_ddi, "extract_notes") %>% cat()

cf <- ipums_example("cps_00006.xml")
read_ipums_micro(cf, vars = c("YEAR", "INCTOT"), verbose = FALSE) %>%
  names()



