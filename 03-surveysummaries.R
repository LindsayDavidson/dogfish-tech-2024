#Calculate the number of sets and the number of hooks for each year/survey type
# Code for creating one database of all Dogfish surveys including comparisons, j hooks, and dogfish surveys
# Note
# SURVEY_SERIES_ID == 48) #2004, 2019, 2022, 2023 survey since it was 2 different sets with different hooks
# SURVEY_SERIES_ID == 93) # 2005 onwards dogfish survey
# SURVEY_SERIES_ID == 76) # 1986 onwards dogfish survey DROP THIS ONE
# SURVEY_SERIES_ID == 92) # 1986, 1989 survey

# yelloweye rockfish were not sampled in earlier years. 1986/1989 maybe not 2004?
# 2004 comparison work had two gear types per set
# 2019 comparison work dropped separate lines per gear type
# 2022 comparison work has two gear types per set
# 2023 comparison work has two gear types per set  and was completed during the HBLL and DOG survey

# load data ---------------------------------------------------------------
#cleaned data from 01-pull_data.R
sets <- readRDS("data/generated/sets_cleaned2.rds")
count <- readRDS("data/raw/dogfish_counts.rds")
sets_df <- readRDS("data/generated/dogfishs_allsets_allspecies_counts.rds")
#saveRDS(final, "data/raw/dogfishs_allsets_allsamples.rds")



# number of sets dropped each year ----------------------------------------

d <- filter(sets, survey_series_id == 48)
sort(unique(d$year))
#d <- filter(sets, survey_series_id == 93)
#d <- filter(sets, survey_series_id == 76)
#unique(d$year)


#take out the J hook sets as that was for the yelloweye comparative work, not dogfish
jhook <- d |> filter(hook_desc == "J-HOOK")

d |>
  filter(fishing_event_id %in% jhook$fishing_event_id) |>
  group_by(year, hook_desc, hooksize_desc) |>
  tally()

df <- d|>
  filter(!fishing_event_id %in% jhook$fishing_event_id)


# Fishing summaries -------------------------------------------------------
df |> group_by(year, hook_desc, hooksize_desc, survey_desc ) |>
  reframe(min = min(fe_end_deployment_time), max = max(fe_end_deployment_time))

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
