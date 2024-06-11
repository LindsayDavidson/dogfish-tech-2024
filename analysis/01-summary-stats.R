#Calculate the number of sets and the number of hooks for each year/survey type
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

library(ggplot2)
library(tidyverse)

# load data ---------------------------------------------------------------

final <- readRDS("data-generated/dogfishs_allsets_allspecies_counts.rds")
samples <- readRDS("data-raw/dogfish_samples.rds")
#take out the 2023 J hook sets as that was for the yelloweye comparative work, not dogfish
jhook <- final |> filter(year == 2023 & cat2 == "comp-dogJ-HOOK")
final <- final |> filter(!fishing_event_id %in% (jhook$fishing_event_id))
final <- filter(final, species_code == "044")

final |> group_by(survey_series_id) |> distinct(year, .keep_all = TRUE) |>
  reframe(count = n())
final |> group_by(survey_series_id, year) |> distinct() |>
  reframe()



# date --------------------------------------------------------------------

final |> group_by(year, hook_desc, hooksize_desc, survey_desc ) |>
  reframe(min = min(trip_start_date), max = max(trip_end_date))

# number of sets dropped each year ----------------------------------------

d <- filter(final, survey_series_id == 48)
sort(unique(d$year))
#d <- filter(sets, survey_series_id == 93)
#d <- filter(sets, survey_series_id == 76)
#unique(d$year)

d |>
   group_by(year, hook_desc, hooksize_desc) |>
  tally()

x <- final |>
  dplyr::select(year, cat2, lglsp_hook_count) |>
  distinct()

x <- final |>
  dplyr::select(year, cat2, soak) |>
  distinct()

# Fishing summaries -------------------------------------------------------

df |> group_by(year, hook_desc, hooksize_desc, survey_desc ) |>
  dplyr::select(year, fishing_event_id, hook_desc, hooksize_desc) |>
  distinct() |>
  tally()

df |> group_by(year, hooksize_desc, survey_desc, site_shortname) |>
  dplyr::select(year, fishing_event_id, hook_desc, hooksize_desc, site_shortname) |>
  distinct() |>
  tally()


# catch summaries ---------------------------------------------------------
final |>
  filter(species_code == "044") |>
  filter(is.na(grouping_depth_id) != TRUE) |>
  group_by(grouping_spatial_id, grouping_depth_id) |>
  reframe(sum = sum(catch_count)) |>
  ggplot() +
  geom_point(aes(grouping_depth_id, sum, group = grouping_spatial_id, colour = grouping_spatial_id)) +
  geom_line(aes(grouping_depth_id, sum, group = grouping_spatial_id, colour = grouping_spatial_id)) +
  facet_wrap(~grouping_spatial_id)

final |>
  filter(species_code == "044") |>
  filter(is.na(grouping_depth_id) != TRUE) |>
  group_by(grouping_spatial_id, year) |> #group by location only
  reframe(sum = sum(catch_count)) |>
  ggplot() +
  geom_point(aes(year, sum, group = grouping_spatial_id, colour = grouping_spatial_id)) +
  geom_line(aes(year, sum, group = grouping_spatial_id, colour = grouping_spatial_id)) +
  facet_wrap(~grouping_spatial_id)


final |>
  filter(species_code == "044") |>
  filter(is.na(grouping_depth_id) != TRUE) |>
  group_by(grouping_spatial_id, grouping_depth_id, year) |>
  reframe(yelloweye_catch_count = sum(catch_count)) |>
  filter(grouping_depth_id == 2)
