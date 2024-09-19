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

# pull inside samples data ------------------------------------------------
d <- get_all_survey_sets("north pacific spiny dogfish", ssid = c(76, 48, 39, 40))
unique(d$survey_abbrev)
saveRDS(d, "data-raw/dogfish_sets_getall.rds")
dim(filter(d, fishing_event_id == 5883186)) #duplicate?? should be 2 not 4

d <- readRDS("data-raw/dogfish_sets_getall.rds")

# final |>
#   drop_na(catch_count, lglsp_hook_count) |>
#   group_by(year, survey2) |>
#   reframe(catchsum = sum(catch_count), hooksum = sum(lglsp_hook_count)) |>
#   mutate(cpue = catchsum/hooksum) |>
#   ggplot() +
#   geom_point(aes(year, cpue, colour = survey2, group = survey2)) +
#   geom_line(aes(year, cpue, colour = survey2, group = survey2))
#
# final |>
#   drop_na(catch_count, lglsp_hook_count) |>
#   group_by(year, survey) |>
#   reframe(catchsum = sum(catch_count), hooksum = sum(lglsp_hook_count)) |>
#   mutate(cpue = catchsum/hooksum) |>
#   ggplot() +
#   geom_point(aes(year, cpue, colour = survey, group = survey)) +
#   geom_line(aes(year, cpue, colour = survey, group = survey))

# ggplot() +
#   geom_point(data = final, aes(longitude, latitude, colour = survey2)) +
#   theme_classic() +
#   geom_sf(data = bc_coast, fill = "grey90", colour = "grey70") +
#   facet_wrap(~survey2) +
#   guides(colour=guide_legend(title="Survey"))
#
# ggplot() +
#   geom_point(data = final, aes(longitude, latitude, colour = survey_abbrev)) +
#   theme_classic() +
#   geom_sf(data = bc_coast, fill = "grey90", colour = "grey70") +
#   facet_wrap(~survey_abbrev) +
#   guides(colour=guide_legend(title="Survey"))
#
#
# final |>
#   filter(survey_abbrev == "DOG") |>
#   ggplot() +
#   geom_point(aes(longitude, latitude, colour = catch_count), size = 1) +
#   theme_classic() +
#   geom_sf(data = bc_coast, fill = "grey90", colour = "grey70") +
#   facet_wrap(~year) +
#   guides(colour=guide_legend(title="Catch count")) +
#   scale_colour_viridis_c() +
#   theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
#
# final |>
#   filter(survey2 == "dog-jhook") |>
#   ggplot() +
#   geom_point(aes(longitude, latitude, colour = catch_count), size = 1) +
#   theme_classic() +
#   geom_sf(data = bc_coast, fill = "grey90", colour = "grey70") +
#   facet_wrap(~year) +
#   guides(colour=guide_legend(title="Catch count")) +
#   scale_colour_viridis_c() +
#   theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
#
