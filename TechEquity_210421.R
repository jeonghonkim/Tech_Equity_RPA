
# rm(list=ls())
# install.packages("devtools")
# install_github("r-spatial/sf")
# install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
#                   "ggspatial", "libwgeom", "rnaturalearth", "rnaturalearthdata"))


library(ipumsr)
library(dplyr, warn.conflicts = FALSE)
library(sf)
library(devtools)
library(ggplot2)
library(mapview)
library(stringi)
library(tidyverse)
theme_set(theme_bw())

# vignette("value-labels", package = "ipumsr")
# vignette("ipums-geography", package = "ipumsr")
# vignette("ipums-cps", package = "ipumsr")
# vignette("ipums-nhgis", package = "ipumsr")
# vignette("ipums-terra", package = "ipumsr")

# Set file path
setwd("G:/Shared drives/Projects/5035_Tech Equity/Data")
  
# 1. Read ddi file
cps_ddi1 <- read_ipums_ddi("usa_00001.xml")
cps_ddi3 <- read_ipums_ddi("usa_00003.xml")
cps_data1 <- read_ipums_micro(cps_ddi1, verbose = FALSE)
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
  


# 3. Geographies

#   3-1. Importing Geographies
Geo_counties = st_read("G:/Shared drives/Projects/5035_Tech Equity/Geographies/RPA_counties_shp.shp")
Geo_PUMAs = st_read("G:/Shared drives/Projects/5035_Tech Equity/Geographies/RPA_PUMAs_shp.shp")
Geo_tracts = st_read("G:/Shared drives/Projects/5035_Tech Equity/Geographies/RPA_tracts_shp.shp")

#   3-2. Plot geometry
plot(st_geometry(Geo_counties))
plot(st_geometry(Geo_PUMAs))
plot(st_geometry(Geo_tracts))

colnames(Geo_counties)
plot(Geo_counties[, "CountyFIPS"], key.width = lcm(4), key.pos = 4)



# 4. Join by attribute

#   4-1. Counties
# Creating leadning zeros in cps_data$COUNTYFIP, and change column names same as Geo_counties
cps_data3$COUNTYFIP = stri_pad_left(cps_data3$COUNTYFIP, 3, "0")
names(cps_data3)[9] <- "CountyFIPS"
colnames(cps_data3)

#   4-2. PUMAs
# Creating leadning zeros in cps_data$, and change column names same as Geo_PUMAs
cps_data3$PUMA = stri_pad_left(cps_data3$PUMA, 5, "0")
names(cps_data3)[10] <- "PUMACE10"
colnames(cps_data3)

cps_data_PUMAs_L = merge(Geo_PUMAs, cps_data3, by = "PUMACE10")
cps_data_PUMAs_L = merge(Geo_PUMAs, cps_data[, c("PUMACE10", "CINETHH", "CISMRTPHN", "CIDATAPLN", "CIHISPEED")], by = "PUMACE10", all.x = TRUE)
cps_data_PUMAs_L









# INCOMPLETE - 5. Writing layers
write.csv(cps_data, file = 'cps_data.csv')
st_write(fin_cps_layer_counties, "IPUMS_counties_shp.shp")
st_write(fin_cps_layer_PUMAs, "IPUMS_PUMAs_shp.shp")
st_write(fin_cps_layer_tracts, "IPUMS_tracts_shp.shp")



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



#   2-3. Convert the labels to factors (and drop the unused levels)
cps_data <- cps_data %>%
  mutate(STATE_factor = as_factor(lbl_clean(STATEFIP)))
table(cps_data$STATE_factor, useNA = "always")



  # The as_factor function also has a "levels" argument that can put both the labels and values into the factor
cps_data <- cps_data %>%
  mutate(STATE_factor4 = droplevels(as_factor(STATEFIP, levels = "both")))
cps_data

table(cps_data$STATE_factor4, useNA = "always")



# 2. Other IPUMS attributes

library(ipumsr)
library(dplyr, warn.conflicts = FALSE)

# Note that you can pass in the loaded DDI into the `read_ipums_micro()`
cps_ddi <- read_ipums_ddi(ipums_example("cps_00006.xml"))
cps_data <- read_ipums_micro(cps_ddi, verbose = FALSE)

# Currently variable description is available for year
ipums_var_desc(cps_data$YEAR)

# But after using ifelse it is gone
cps_data <- cps_data %>%
  mutate(YEAR = ifelse(YEAR == 1962, 62, NA))
ipums_var_desc(cps_data$YEAR)

# So you can use the DDI
ipums_var_desc(cps_ddi, "YEAR")

# The DDI also has file level information that is not available from just the data.
ipums_file_info(cps_ddi, "extract_notes") %>% cat()



# 3. "dplyr select-style" Syntax

library(ipumsr)
library(dplyr, warn.conflicts = FALSE)

# The vars argument for `read_ipums_micro` uses this syntax
# So these are all equivalent
cf <- ipums_example("cps_00006.xml")
read_ipums_micro(cf, vars = c("YEAR", "INCTOT"), verbose = FALSE) %>%
  names()
read_ipums_micro(cf, vars = c(YEAR, INCTOT), verbose = FALSE) %>%
  names()
read_ipums_micro(cf, vars = c(one_of("YEAR"), starts_with("INC")), verbose = FALSE) %>%
  names()

# `data_layer` and `shape_layer` arguments to `read_nhgis()` and terra functions also use it.
# (Sometimes extracts have multiple files, though all examples only have one)
nf <- ipums_example("nhgis0008_csv.zip")
ipums_list_files(nf)
ipums_list_files(nf, data_layer = "nhgis0008_csv/nhgis0008_ds135_1990_pmsa.csv")
ipums_list_files(nf, data_layer = contains("ds135"))



# 4. Hierarchical data structures

library(ipumsr)
library(dplyr, warn.conflicts = FALSE)

# List data
cps <- read_ipums_micro_list(
  ipums_example("cps_00010.xml"),
  verbose = FALSE
)

cps$PERSON
cps$HOUSEHOLD

# Long data
cps <- read_ipums_micro(
  ipums_example("cps_00010.xml"),
  verbose = FALSE
)

cps

