# params
# daniel looking into this, 2004 and some 2005 are missing deployment times
# 02a-data-clean-sets.R
soak2005 <- 2
bccrs <- 32609
latitude_cutoff <- 49.93883

library(sf)
library(ggplot2)
library(tidyverse)
library(sdmTMB)

# load data  ---------------------------------------------------------------

hbll <- readRDS("data-raw/hbllsets.rds")
final <- readRDS("data-generated/dogfish_sets_cleaned.rds") |>
  filter(species_code == "044") # pull just dogfish data not other species


# hbll wrangle ------------------------------------------------------------

ggplot(hbll) +
geom_point(aes(longitude, latitude)) + facet_wrap(~survey_abbrev)

ggplot(final) +
  geom_point(aes(longitude, latitude)) + facet_wrap(~survey2)

# remove two survey years that extended along the west coast VI
hbll <- filter(hbll, !(latitude < 48.5 & longitude < -123)) # only two years have the sampling around the strait
hbll <- filter(hbll, !(latitude < 48.75 & longitude < -124.25))

ggplot(hbll) +
  geom_point(aes(longitude, latitude)) + facet_wrap(~survey_abbrev)

hbll <- hbll |> dplyr::select(survey_desc, year, fishing_event_id, latitude, longitude, depth_m, hook_count, catch_count, survey_abbrev, time_deployed, time_retrieved, month)
hbll <- hbll |>
  mutate(
    # time_retrieved = as.Date(time_retrieved, format = "%Y-%m-%d h:m:s"),
    julian = lubridate::yday(time_deployed),
    retrievehr = lubridate::hour(time_retrieved),
    retrievemin = lubridate::minute(time_retrieved),
    deployhr = lubridate::hour(time_deployed),
    deploymin = lubridate::minute(time_deployed)
  ) |>
  filter(is.na(retrievehr) == FALSE) |>
  # filter(survey_abbrev == "HBLL INS S" | year == 2008) |>
  mutate(
    hr_diff = (retrievehr - deployhr) * 60,
    min_diff = abs(retrievemin - deploymin),
    soak = (hr_diff + min_diff) / 60
  ) |>
  dplyr::select(!c(retrievehr, retrievemin, deployhr, deploymin, hr_diff, min_diff, time_deployed, time_retrieved))

hbll$offset <- log(hbll$hook_count * hbll$soak)
hbll$log_botdepth <- log(hbll$depth_m)
hbll$survey2 <- "hbll"
hbll$survey3 <- hbll$survey2
ggplot(hbll, aes(longitude, latitude, colour = survey_abbrev)) +
  geom_point() +
  facet_wrap(~year)
ggplot(hbll, aes(longitude, latitude, colour = survey2)) +
  geom_point() +
  facet_wrap(~year)

# change the name of the points that fall in the southern HBLL range to HBLL S
#define the HBLL INS S northern boundary based on the years between 2013 - 2022
test <- hbll |> filter(year %in% c(2013:2022) & survey_abbrev == "HBLL INS S")
range(test$latitude)

hbll <- hbll |>
  mutate(survey_abbrev = ifelse(latitude < latitude_cutoff, "HBLL INS S", survey_abbrev))

ggplot(hbll, aes(longitude, latitude, colour = survey_abbrev)) +
  geom_point() + facet_wrap(~survey_abbrev)

ggplot(hbll, aes(longitude, latitude, colour = survey2)) +
  geom_point() + facet_wrap(~survey2)
ggplot(hbll, aes(longitude, latitude, colour = survey2)) +
  geom_point() + facet_wrap(~year)

sort(unique(hbll$year))

# dogfish data -------------------------------------------------------

final <- final |>
  dplyr::select(
    survey_series_desc, year, survey2, fishing_event_id, latitude, longitude, depth_m, lglsp_hook_count, catch_count, month,
    julian, soak
  ) |>
  rename("survey_desc" = "survey_series_desc", "hook_count" = "lglsp_hook_count")


ggplot(final, aes(longitude, latitude, colour = survey2)) +
  geom_point() + facet_wrap(~survey2)


final$survey_abbrev <- ifelse(final$survey2 %in% c("hbll", "hbll comp"), "HBLL INS S",
                              ifelse(final$survey2 == "dog comp", "dog",
                                 final$survey2))
final$survey3 <- ifelse(final$survey2 %in% c("hbll", "hbll comp"), "hbll",
                        ifelse(final$survey2 == "dog comp", "dog",
                               final$survey2))

# final$survey2 <- ifelse(final$survey2 == "hbll", "hbll",
#                               final$survey2)

final <- final |>
  mutate(soak = ifelse(year %in% c(2005), 2, soak))

# final <- final |>
#   mutate(survey2 = case_when(
#     survey_abbrev == "HBLL INS S" ~ "hbll",
#     survey_abbrev == "dog" ~ "dog",
#     survey_abbrev == "dog-jhook" ~ "dog-jhook"))
#
final <- final |> filter(!is.na(soak))
final <- final |> filter(!is.na(julian))
final$offset <- log(final$hook_count * final$soak)
final$log_botdepth <- log(final$depth_m)

final |>
  group_by(survey_abbrev, year) |>
  distinct() |>
  reframe()


ggplot(final, aes(longitude, latitude, colour = survey_abbrev)) +
  geom_point() + facet_wrap(~survey_abbrev)
ggplot(final, aes(longitude, latitude, colour = survey2)) +
  geom_point() + facet_wrap(~survey2)
ggplot(final, aes(longitude, latitude, colour = survey3)) +
  geom_point() + facet_wrap(~survey3)

d <- bind_rows(final, hbll)

# convert to UTMs
d <- add_utm_columns(d,
  ll_names = c("longitude", "latitude"),
  utm_names = c("UTM.lon", "UTM.lat"),
  utm_crs = bccrs
) |>
  mutate(UTM.lon.m = UTM.lon * 1000, UTM.lat.m = UTM.lat * 1000)

ggplot(d, aes(longitude, latitude, colour = survey2)) +
  geom_point() + facet_wrap(~survey2)
ggplot(d, aes(longitude, latitude, colour = survey3)) +
  geom_point() + facet_wrap(~survey3)
ggplot(d, aes(longitude, latitude, colour = survey_abbrev)) +
  geom_point() + facet_wrap(~survey_abbrev)

saveRDS(d, "data-raw/wrangled-hbll-dog-sets.rds") # no expansion set along the strait
unique(d$survey3)
