# note 2004 and some 2005 are missing deployment times
# 02a-data-clean-sets.R
# soak2005 <- 2
bccrs <- 32609
latitude_cutoff <- 50.34056 #<- not sure what the "best" boundary to pick for separating n and s is I used the largest latitude in the hbll_ins_s grid at the line

library(sf)
library(ggplot2)
library(tidyverse)
library(sdmTMB)

# load data  ---------------------------------------------------------------
#final <- readRDS("data-generated/dogfish_sets_cleaned_getall.rds") |> filter(usability_code == 1)

final <- readRDS("data-generated/dogfish_sets_cleaned_getall.rds") #usability_code filters out the comparison sets
hbll <- filter(final, survey_lumped == "hbll" & survey_sep != "hbll comp") |>
  filter(survey_series_og %in% c(39, 40)) |>
  filter(usability_code  == 1) # note how the boundary has been different, also this is from the *get_all* function pulls survey locations that are not a part of the HBLL standardized survey, remove them

dog <- filter(final, survey_series_desc %in% c("Dogfish Gear/Timing Comparison Surveys", "Strait of Georgia Dogfish Longline"))


# hbll wrangle ------------------------------------------------------------
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

# # change the name of the points that fall in the southern HBLL range to HBLL S
# # define the HBLL INS S northern boundary based on the years between 2013 - 2022
# hbll <- hbll |>
#   mutate(survey_sep = ifelse(survey_abbrev %in% c("HBLL INS N", "HBLL INS S") & latitude <= latitude_cutoff,
#     "HBLL INS S",
#     ifelse(survey_abbrev %in% c("HBLL INS N", "HBLL INS S") & latitude > latitude_cutoff,
#       "HBLL INS N", survey_sep
#     )
#   ))
#
ggplot(hbll) +
  geom_point(aes(longitude, latitude)) +
  facet_wrap(~survey_sep)


# put cleaned hbll and dog back together-------------------------------------------------------

final <- bind_rows(dog, hbll)

final |>
  group_by(survey_lumped, year) |>
  distinct() |>
  reframe() |>
  print(n = 40) # looks good

final |>
  group_by(survey_sep, year) |>
  distinct() |>
  reframe() |>
  print(n = 100) # looks good

ggplot(final, aes(longitude, latitude, colour = survey_lumped)) +
  geom_point() +
  facet_wrap(~survey_lumped)

ggplot(final, aes(longitude, latitude, colour = survey_lumped)) +
  geom_point() +
  facet_wrap(~survey_sep)

test <- final |>
  group_by(survey_sep, year) |>
  distinct() |>
  reframe() |>
  tally() |>
  print(n = 40) # looks good

# convert to UTMs
d <- add_utm_columns(final,
  ll_names = c("longitude", "latitude"),
  utm_names = c("UTM.lon", "UTM.lat"),
  utm_crs = bccrs
) |>
  mutate(UTM.lon.m = UTM.lon * 1000, UTM.lat.m = UTM.lat * 1000)

d <- d |> drop_na(survey_lumped)
saveRDS(d, "data-raw/wrangled-hbll-dog-sets.rds")
