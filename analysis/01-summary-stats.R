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


# load map ----------------------------------------------------------------
library(sf)
sf::sf_use_s2(FALSE)
map_data <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")
bc_coast <- st_crop(
  map_data,
  c(xmin = -134, ymin = 46, xmax = -120, ymax = 57)
)

# load data ---------------------------------------------------------------

samps <- readRDS("output/samps_joined.rds")
sets <- readRDS("data-raw/wrangled-hbll-dog-sets.rds")

# # date --------------------------------------------------------------------

sets |> group_by(year, hook_desc, hooksize_desc, survey_series_id ) |>
  reframe(min = min(time_retrieved), max = max( time_retrieved))

# number of sets dropped each year ----------------------------------------

d <- filter(final, survey_series_id == 48)
sort(unique(d$year))
#d <- filter(sets, survey_series_id == 93)
#d <- filter(sets, survey_series_id == 76)
#unique(d$year)

d |>
   group_by(year, hook_desc, hooksize_desc) |>
  tally()

final |>
  dplyr::select(year, survey_series_id, lglsp_hook_count) |>
  distinct(.keep_all = TRUE) |>
  group_by(year, survey_series_id) |>
  reframe(min = min(lglsp_hook_count), max = max(lglsp_hook_count)) |>
  print(n = 30)

final |>
  dplyr::select(year, survey_series_id, soak) |>
  distinct(.keep_all = TRUE) |>
  group_by(year, survey_series_id) |>
  reframe(min = min(soak), max = max(soak)) |>
  print(n = 30)


# Fishing summaries -------------------------------------------------------

d |> group_by(year, hook_desc, hooksize_desc, survey_series_id ) |>
  dplyr::select(year, fishing_event_id, hook_desc, hooksize_desc) |>
  distinct() |>
  tally()

# catch summaries ---------------------------------------------------------

final |>
  group_by( grouping_depth_id) |>
  reframe(sum = sum(catch_count)) |>
  ggplot() +
  geom_point(aes(grouping_depth_id, sum)) +
  geom_line(aes(grouping_depth_id, sum))

