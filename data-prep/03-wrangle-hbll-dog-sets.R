# params
# daniel looking into this, 2004 and some 2005 are missing deployment times
# 02a-data-clean-sets.R
# soak2005 <- 2
bccrs <- 32609
latitude_cutoff <- 49.99607

library(sf)
library(ggplot2)
library(tidyverse)
library(sdmTMB)

# load data  ---------------------------------------------------------------

final <- readRDS("data-generated/dogfish_sets_cleaned_getall.rds")
hbll <- filter(final, survey_lumped == "hbll") |> filter(usability_code == 1 ) |> filter(survey_series_og %in% c(39, 40)) # note how the boundary has been different, also this is from the *get_all* function that pulls in survey locations that are not a part of the standardized survey, remove them
hbllsets <- get_survey_sets(species = "north pacific spiny dogfish", ssid = c(39,40))

ggplot(hbll) +
  geom_point(aes(longitude, latitude)) +
  facet_wrap(~survey_abbrev)

testpe <- hbll |>
  filter(survey_abbrev == "HBLL INS N") |>
  filter(year == 2007) |>
  dplyr::select(year, fishing_event_id, survey_abbrev, catch_count)

# ggplot() +
#   geom_point(aes(longitude, latitude, colour = catch_count)) +
#   facet_wrap(~year)

test <- hbllsets |>
  filter(survey_abbrev == "HBLL INS N") |>
  filter(year == 2007) |>
  dplyr::select(year, fishing_event_id, survey_abbrev, catch_count) |>
  mutate(id = "gf")

ggplot() +
  geom_point(aes(longitude, latitude, colour = catch_count)) +
  facet_wrap(~year)

testpe2 <- left_join(testpe, test)

hbllgf <- hbllsets |> group_by(year) |> filter(survey_series_id %in% c(39)) |> drop_na(catch_count) |> reframe(sum = sum(catch_count))
hbllpe <- hbll |> filter(survey_series_id %in% c(39)) |> group_by(year) |> drop_na(catch_count) |> reframe(sumpe = sum(catch_count))

x <- left_join(hbllgf, hbllpe)
x$diff <- x$sum - x$sumpe #slight discripancies between the two different data pulls.

ggplot(x) +
  geom_line(aes(year, sum), col = "red") +
  geom_line(aes(year, sumpe)) #slight discripancies between the two different data pulls. Could the be the points that extend around may be partially. 2007 is in the in N


# hbll wrangle ------------------------------------------------------------

ggplot(final) +
  geom_point(aes(longitude, latitude)) +
  facet_wrap(~survey_sep)

ggplot(final) +
  geom_point(aes(longitude, latitude)) +
  facet_wrap(~survey_lumped)

final |>
  group_by(survey_lumped, year) |>
  distinct() |>
  reframe() |>
  print(n = 40) # looks good

# remove two survey years that extended along the west coast VI
hbll <- filter(hbll, !(latitude < 48.5 & longitude < -123)) # only two years have the sampling around the strait
hbll <- filter(hbll, !(latitude < 48.75 & longitude < -124.25))

ggplot(hbll) +
  geom_point(aes(longitude, latitude)) +
  facet_wrap(~survey_abbrev)

# change the name of the points that fall in the southern HBLL range to HBLL S
# define the HBLL INS S northern boundary based on the years between 2013 - 2022
test <- hbll |> filter(year %in% c(2013:2022) & survey_abbrev == "HBLL INS N")
range(test$latitude)

hbll <- hbll |>
  mutate(survey_sep = ifelse(survey_abbrev == "HBLL INS N" & latitude <= latitude_cutoff,
    "HBLL INS S",
    ifelse(survey_abbrev == "HBLL INS S" & latitude > latitude_cutoff,
      "HBLL INS N", survey_sep
    )
  ))

ggplot(hbll, aes(longitude, latitude, colour = survey_abbrev)) +
  geom_point() +
  facet_wrap(~survey_abbrev)

ggplot(hbll, aes(longitude, latitude, colour = survey_sep)) +
  geom_point() +
  facet_wrap(~survey_sep)
ggplot(hbll, aes(longitude, latitude, colour = survey_sep)) +
  geom_point() +
  facet_wrap(~year)
ggplot(hbll, aes(longitude, latitude, colour = survey_lumped)) +
  geom_point() +
  facet_wrap(~survey_lumped)

# put cleaned hbll and dog back together-------------------------------------------------------

dog <- filter(final, survey_lumped != "hbll")
final <- bind_rows(dog, hbll)

final |>
  group_by(survey_lumped, year) |>
  distinct() |>
  reframe() |>
  print(n = 40) # looks good

ggplot(final, aes(longitude, latitude, colour = survey_lumped)) +
  geom_point() +
  facet_wrap(~survey_lumped)
ggplot(final, aes(longitude, latitude, colour = survey_sep)) +
  geom_point() +
  facet_wrap(~survey_sep)

# convert to UTMs
d <- add_utm_columns(final,
  ll_names = c("longitude", "latitude"),
  utm_names = c("UTM.lon", "UTM.lat"),
  utm_crs = bccrs
) |>
  mutate(UTM.lon.m = UTM.lon * 1000, UTM.lat.m = UTM.lat * 1000)

d |>
  group_by(survey_lumped, year) |>
  distinct() |>
  reframe() |>
  print(n = 40) # looks good

saveRDS(d, "data-raw/wrangled-hbll-dog-sets.rds") # no expansion set along the strait
