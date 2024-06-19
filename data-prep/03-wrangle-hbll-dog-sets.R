# load hbll data ---------------------------------------------------------------

hbll <- readRDS("data-raw/hbllsets.rds")
hbll <- hbll |> dplyr::select(survey_desc, year, fishing_event_id, latitude, longitude, depth_m, hook_count, catch_count, survey_abbrev, time_deployed, time_retrieved)

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
final <- final |>
  dplyr::select(
    survey_series_desc, year, survey, fishing_event_id, latitude, longitude, depth_m, lglsp_hook_count, catch_count,
    julian, soak
  ) |>
  rename("survey_desc" = "survey_series_desc", "hook_count" = "lglsp_hook_count", "survey_abbrev" = "survey")
# final <- final |>
#   mutate(soak = ifelse(year %in% c(2004, 2005), 2, soak))

final <- final |>
  mutate(soak = ifelse(year == 2004, 3,
                       ifelse(year == 2005, 2, soak)
  ))

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

# convert to UTMs
d <- add_utm_columns(d,
                     ll_names = c("longitude", "latitude"),
                     utm_names = c("UTM.lon", "UTM.lat"),
                     utm_crs = bccrs
) |>
  mutate(UTM.lon.m = UTM.lon * 1000, UTM.lat.m = UTM.lat * 1000)

saveRDS(d, "data-raw/wrangled-hbll-dog-sets.rds")