library(sf)
library(ggplot2)
library(tidyverse)
library(sdmTMB)

bccrs <- 32609

final <- readRDS("data-generated/dogfish_sets_cleaned_getall.rds") # usability_code filters out the comparison sets

hbll <- filter(final, survey_sep %in% c("HBLL INS N", "HBLL INS S"))
# note how the boundary has been different, also this is from the *get_all* function pulls survey locations that are not a part of the HBLL standardized survey, remove them

dog <- filter(final, !survey_sep %in% c("HBLL INS N", "HBLL INS S"))


# hbll wrangle ------------------------------------------------------------
final |>
  group_by(survey_sep, year) |>
  distinct() |>
  reframe() |>
  print(n = 40) # looks good

# remove two survey years that extended along the west coast VI
hbll <- filter(hbll, !(latitude < 48.5 & longitude < -123))
hbll <- filter(hbll, !(latitude < 48.75 & longitude < -124.25))

ggplot(hbll) +
  geom_point(aes(longitude, latitude)) +
  facet_wrap(~survey_abbrev)

ggplot(hbll) +
  geom_point(aes(longitude, latitude)) +
  facet_wrap(~survey_sep)


# put cleaned hbll and dog back together-------------------------------------------------------

final <- bind_rows(dog, hbll)

final |>
  group_by(survey_sep, year) |>
  distinct() |>
  reframe() |>
  print(n = 40) # looks good

final |>
  group_by(survey_sep, year) |>
  distinct() |>
  reframe() |>
  print(n = 100) # looks good

ggplot(final, aes(longitude, latitude, colour = survey_sep)) +
  geom_point() +
  facet_wrap(~survey_sep)

ggplot(final, aes(longitude, latitude, colour = survey_sep)) +
  geom_point() +
  facet_wrap(~survey_sep)

final |>
  group_by(survey_sep, year) |>
  distinct() |>
  tally() |>
  print(n = 40) # looks good

# convert to UTMs
d <- add_utm_columns(final,
  ll_names = c("longitude", "latitude"),
  utm_names = c("UTM.lon", "UTM.lat"),
  utm_crs = bccrs
) |>
  mutate(UTM.lon.m = UTM.lon * 1000, UTM.lat.m = UTM.lat * 1000)

d <- d |> drop_na(survey_sep)
saveRDS(d, "data-raw/wrangled-hbll-dog-sets.rds")
