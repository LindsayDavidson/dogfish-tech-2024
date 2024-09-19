# params
# daniel looking into this, 2004 and some 2005 are missing deployment times
# 02a-data-clean-sets.R
soak2005 <- 2
bccrs <- 32609
latitude_cutoff <- 49.99607

library(sf)
library(ggplot2)
library(tidyverse)
library(sdmTMB)

# load data  ---------------------------------------------------------------

final <- readRDS("data-generated/dogfish_sets_cleaned_getall.rds")
hbll <- filter(final, survey_lumped == "hbll") #note how the boundary has been different

# hbll wrangle ------------------------------------------------------------

ggplot(hbll) +
geom_point(aes(longitude, latitude)) + facet_wrap(~survey_abbrev)

ggplot(final) +
  geom_point(aes(longitude, latitude)) + facet_wrap(~survey_sep)

ggplot(final) +
  geom_point(aes(longitude, latitude)) + facet_wrap(~survey_lumped)

final |>
  group_by(survey_lumped, year) |>
  distinct() |>
  reframe() |>
  print(n=40) #looks good

# remove two survey years that extended along the west coast VI
hbll <- filter(hbll, !(latitude < 48.5 & longitude < -123)) # only two years have the sampling around the strait
hbll <- filter(hbll, !(latitude < 48.75 & longitude < -124.25))

ggplot(hbll) +
  geom_point(aes(longitude, latitude)) + facet_wrap(~survey_abbrev)

ggplot(hbll, aes(longitude, latitude, colour = survey_abbrev)) +
  geom_point() +
  facet_wrap(~year)
ggplot(hbll, aes(longitude, latitude, colour = survey_sep)) +
  geom_point() +
  facet_wrap(~year)

# change the name of the points that fall in the southern HBLL range to HBLL S
#define the HBLL INS S northern boundary based on the years between 2013 - 2022
test <- hbll |> filter(year %in% c(2013:2022) & survey_abbrev == "HBLL INS N")
range(test$latitude)

hbll <- hbll |>
  mutate(survey_sep = ifelse(survey_abbrev == "HBLL INS N" & latitude <= latitude_cutoff,
                                "HBLL INS S",
                                ifelse(survey_abbrev == "HBLL INS S" & latitude > latitude_cutoff,
                                "HBLL INS N", survey_sep)))

ggplot(hbll, aes(longitude, latitude, colour = survey_abbrev)) +
  geom_point() + facet_wrap(~survey_abbrev)
ggplot(hbll, aes(longitude, latitude, colour = survey_sep)) +
  geom_point() + facet_wrap(~survey_sep)
ggplot(hbll, aes(longitude, latitude, colour = survey_sep)) +
  geom_point() + facet_wrap(~year)
ggplot(hbll, aes(longitude, latitude, colour = survey_lumped)) +
  geom_point() + facet_wrap(~survey_lumped)

# put cleaned hbll and dog back together-------------------------------------------------------

dog <- filter(final, survey_lumped != "hbll")
final <- bind_rows(dog, hbll)

final |>
  group_by(survey_lumped, year) |>
  distinct() |>
  reframe() |>
  print(n=40) #looks good

ggplot(final, aes(longitude, latitude, colour = survey_abbrev)) +
  geom_point() + facet_wrap(~survey_abbrev)
ggplot(final, aes(longitude, latitude, colour = survey_lumped)) +
  geom_point() + facet_wrap(~survey_lumped)
ggplot(final, aes(longitude, latitude, colour = survey_sep)) +
  geom_point() + facet_wrap(~survey_sep)

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
  print(n=40) #looks good

saveRDS(d, "data-raw/wrangled-hbll-dog-sets.rds") # no expansion set along the strait
