

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



# 2. Explores data
View(cps_data3)

#   2-1. Variable Description
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

#   2-2. Show which variables have labels
cps_data3 %>%
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
Geo_tracts = st_read("G:/Shared drives/Projects/5035_Tech Equity/Geographies/RPA_tracts_shp.shp")

#   3-2. Check geometry
plot(st_geometry(Geo_counties))
plot(st_geometry(Geo_PUMAs))
plot(st_geometry(Geo_tracts))


    
# 4. Join

#   4-1. cps_data cleaning
#     1) Counties
# Creating leadning zeros in cps_data$COUNTYFIP, and change column names same as Geo_counties
cps_data3$COUNTYFIP = stri_pad_left(cps_data3$COUNTYFIP, 3, "0")
names(cps_data3)[9] <- "CountyFIPS"
colnames(cps_data3)
# Check the count # of CountyFIPS Column
count_counties <- cps_data3 %>%
                    count(CountyFIPS)
View(count_counties)
#     2) PUMAs
# Creating leadning zeros in cps_data$, and change column names same as Geo_PUMAs
cps_data3$PUMA = stri_pad_left(cps_data3$PUMA, 5, "0")
names(cps_data3)[10] <- "PUMACE10"
colnames(cps_data3)
# Check the count # of PUMACE10 Column
count_PUMAs <- cps_data3 %>%
                    count(PUMACE10)
View(count_PUMAs)

#   4-2. Aggregate based on the geographies
#     1) Counties
cps_data3_agg_Counties <- aggregate(list(cps_data3$CINETHH, cps_data3$CISMRTPHN, cps_data3$CIDATAPLN, cps_data3$CIHISPEED), 
                                  by = list(cps_data3$CountyFIPS),
                                  FUN = sum, na.rm = TRUE) 
NewNames_Counties <- c('CountyFIPS', 'CINETHH', 'CISMRTPHN', 'CIDATAPLN', 'CIHISPEED')
names(cps_data3_agg_Counties) <- NewNames_Counties
View(cps_data3_agg_Counties)

#     2) PUMAs
cps_data3_agg_PUMAs <- aggregate(list(cps_data3$CINETHH, cps_data3$CISMRTPHN, cps_data3$CIDATAPLN, cps_data3$CIHISPEED), 
                                 by = list(cps_data3$PUMACE10),
                                 FUN = sum, na.rm = TRUE) 

NewNames_PUMAs <- c('PUMACE10', 'CINETHH', 'CISMRTPHN', 'CIDATAPLN', 'CIHISPEED')
names(cps_data3_agg_PUMAs) <- NewNames_PUMAs
View(cps_data3_agg_PUMAs)

#   4-3. Joining the aggregated cps data to gegraphy with geography code
#     1) Counties
cps_data_Counties = merge(Geo_counties, cps_data3_agg_Counties, by = "CountyFIPS", all.x = TRUE)
View(cps_data_Counties)
#     2)  PUMAs
cps_data_PUMAs = merge(Geo_PUMAs, cps_data3_agg_PUMAs, by = "PUMACE10", all.x = TRUE)
View(cps_data_PUMAs)



# 5. Check & Export
#   5-1. Plotting the outcomes
#     1) Counties
plot(cps_data_Counties[, "CINETHH"], breaks = "quantile")
plot(cps_data_Counties[, "CISMRTPHN"], breaks = "quantile")
#     2) PUMAs
plot(cps_data_PUMAs[, "CIDATAPLN"], breaks = "quantile")
plot(cps_data_PUMAs[, "CIHISPEED"], breaks = "quantile")

#   5-2. Exporting data and layers
write.csv(cps_data, file = 'cps_data.csv')
write.csv(cps_data_Counties, file = 'IPUMS_Counties_ref.csv')
write.csv(cps_data_PUMAs, file = 'IPUMS_PUMAs_ref.csv')
st_write(cps_data_Counties, "IPUMS_Counties_shp.shp")
st_write(cps_data_PUMAs, "IPUMS_PUMAs_shp.shp")



#######################################


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



