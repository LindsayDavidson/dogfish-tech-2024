# pull dogfish data using the new function *get_all* in gfbio
# there is a branch on PBS assess that has this code.
# https://github.com/pbs-assess/Dogfish-survey/blob/main/Dogfish_data_pull.R

# Code for creating one database of all Dogfish surveys including comparisons, j hooks, and dogfish surveys
# Note
# SURVEY_SERIES_ID == 48) #comp work 2004, 2019, 2022, 2023
# SURVEY_SERIES_ID == 93) #circle hook dog surveys 2005 onwards (2005, 2008, 2011, 2014, 2019)
# SURVEY_SERIES_ID == 76) #all jhook and circle hook dog surveys 1986 onwards does not include 2004 (1986, 1989, 2005, 2008, 2011, 2014, 2019)
# SURVEY_SERIES_ID == 92) #j hook dog surveys 1986, 1989 only

# yelloweye rockfish were not sampled in earlier years. 1986/1989 maybe not 2004?
# 2004 comparison work had two gear types per set
# 2019 comparison work dropped separate lines per gear type
# 2022 comparison work has two gear types per set
# 2023 comparison work has two gear types per set  and was completed during the HBLL and DOG survey

# library -----------------------------------------------------------------
library(tidyverse)
library(sf)
library(ggplot2)
library(here)
library(sp)
library(gfdata)
remotes::install_github("pbs-assess/gfdata", ref = "trials", force = TRUE)

# pull inside sets data ------------------------------------------------
d <- get_all_survey_sets("north pacific spiny dogfish", ssid = c(76, 48, 39, 40))
unique(d$survey_abbrev)
saveRDS(d, "data-raw/dogfish_sets_getall.rds")
dim(filter(d, fishing_event_id == 5883186)) #duplicate?? should be 2 not 4

hbllgfdata <- get_survey_sets(species = "north pacific spiny dogfish", ssid = c(76, 48, 39, 40))
saveRDS(hbllgfdata, "data-raw/dogfish_sets_gfdata.rds")

#pull inside samples data
#d <- get_all_survey_samples("north pacific spiny dogfish", ssid = c(76, 48, 39, 40))
d <- get_all_survey_samples("north pacific spiny dogfish", ssid = c(76, 48, 39, 40), include_event_info = TRUE)
unique(d$survey_abbrev)
saveRDS(d, "data-raw/dogfish_samples_getall.rds")

d <- get_survey_samples("north pacific spiny dogfish", ssid = c(76, 48, 39, 40))
unique(d$survey_abbrev)
length(unique(d$fishing_event_id))
saveRDS(d, "data-raw/dogfish_samples_gfdata.rds")

