
# notes -------------------------------------------------------------------

#2004 is missing deployment times and therefore I can't caluculate soak time
#soak times were between 1.5-3 hours



# library -----------------------------------------------------------------

library(ggplot2)
library(tidyverse)

# load hbll data ---------------------------------------------------------------

hbll <- readRDS("data-raw/hbllsets.rds")
hbll <- hbll |> dplyr::select(survey_desc, year, fishing_event_id, latitude, longitude, depth_m, hook_count, catch_count, survey_abbrev, time_deployed, time_retrieved)

hbll <- hbll |>
  mutate(
    #time_retrieved = as.Date(time_retrieved, format = "%Y-%m-%d h:m:s"),
    julian = lubridate::yday(time_deployed),
    retrievehr = lubridate::hour(time_retrieved),
    retrievemin = lubridate::minute(time_retrieved),
    deployhr = lubridate::hour(time_deployed),
    deploymin = lubridate::minute(time_deployed)
  ) |>
  filter(is.na(retrievehr) == FALSE) |>
  filter(survey_abbrev == "HBLL INS S") |>
  mutate(
    hr_diff = (retrievehr - deployhr) * 60,
    min_diff = abs(retrievemin - deploymin),
    soak = (hr_diff + min_diff) / 60
  ) |>
  dplyr::select(!c(retrievehr, retrievemin, deployhr, deploymin, hr_diff, min_diff, time_deployed, time_retrieved))

hbll$offset <- log(hbll$hook_count * hbll$soak)
hbll$log_botdepth <- log(hbll$depth_m)
hbll$survey_abbrev <- "hbll"
hbll <- filter(hbll, !(latitude < 48.5 & longitude < -123))
hbll <- filter(hbll, !(latitude < 48.75 & longitude < -124.25))
hbll |>
  ggplot() +
  geom_point(aes(longitude, latitude))



# load dogfish data -------------------------------------------------------

final <- readRDS("data-generated/dogfish_sets_cleaned.rds") |>
  filter(species_code == "044")
unique(final$year)
final <- final |> dplyr::select(
  survey_series_desc, year, survey, fishing_event_id, latitude, longitude, depth_m, lglsp_hook_count, catch_count,
  julian, soak
) |>
  rename("survey_desc" = "survey_series_desc",  "hook_count" = "lglsp_hook_count", "survey_abbrev" = "survey")
final <- final |>
  mutate(soak = ifelse(year %in% c(2004, 2005), 2, soak))

final <- final |> filter(!is.na(soak))
final <- final |> filter(!is.na(julian))
final$offset <- log(final$hook_count * final$soak)
final$log_botdepth <- log(final$depth_m)
range(final$depth_m)
glimpse(final)
unique(final$survey_abbrev)

final |>
  group_by(survey_abbrev, year) |>
  distinct() |>
  reframe()

d <- bind_rows(final, hbll)


# summary plots -----------------------------------------------------------

d |>
  group_by(survey_abbrev, year) |>
  mutate(
    catch_count_sum = sum(catch_count),
    cpue = sum(catch_count) / sum(hook_count)
  ) |>
  ggplot() +
  geom_point(aes(year, cpue, colour = survey_abbrev))

d |>
  ggplot() +
  geom_point(aes(longitude, latitude))


d |>
  group_by(survey_abbrev, year) |>
  ggplot() +
  geom_jitter(aes(year, log(catch_count), colour = survey_abbrev))


d |>
  group_by(survey_abbrev, year) |>
  ggplot() +
  geom_jitter(aes(year, hook_count, colour = survey_abbrev))


d |>
  group_by(survey_abbrev, year) |>
  ggplot() +
  geom_jitter(aes(year, julian, colour = survey_abbrev))


d |>
  group_by(survey_abbrev, year) |>
  ggplot() +
  geom_jitter(aes(year, soak, colour = survey_abbrev))




# prediction grid from gfplot ---------------------------------------------

hbll_ins <- gfplot::hbll_inside_s_grid
plot(hbll_ins$grid$X, hbll_ins$grid$Y)
range(hbll_ins$grid)
# needs depth
range(hbll_ins$grid$X)
range(hbll_ins$grid$Y)
hbll_ins$grid <- hbll_ins$grid[!duplicated(hbll_ins$grid), ] # just checking

b <- marmap::getNOAA.bathy(lon1 = -126, lon2 = -122, lat1 = 47, lat2 = 51, resolution = 1)

bdepths <- marmap::get.depth(b, hbll_ins$grid[, c("X", "Y")], locator = FALSE) |>
  mutate(bot_depth = (depth * -1))
# rename(longitude = lon, latitude = lat) |>
# filter(bot_depth > 25) |>
# mutate(logbot_depth = log(bot_depth)) |>
# inner_join(hbll_ins$grid, by = c("X" = "X", "Y" = "Y"))

bdepths[duplicated(bdepths), ] # just checking




# mesh  -------------------------------------------------------------------




# model -------------------------------------------------------------------
