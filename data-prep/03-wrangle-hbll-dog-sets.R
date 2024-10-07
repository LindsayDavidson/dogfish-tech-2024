# params
# daniel looking into this, 2004 and some 2005 are missing deployment times
# 02a-data-clean-sets.R
# soak2005 <- 2
bccrs <- 32609
latitude_cutoff <- 50.34056 #<- not sure what the "best" boundary to pick for separating n and s is I used the largest latitude in the hbll_ins_s grid at the line

library(sf)
library(ggplot2)
library(tidyverse)
library(sdmTMB)

# load data  ---------------------------------------------------------------

final <- readRDS("data-generated/dogfish_sets_cleaned_getall.rds")
hbll <- filter(final, survey_lumped == "hbll" & survey_sep != "hbll comp") |>
  filter(usability_code == 1) |>
  filter(survey_series_og %in% c(39, 40)) # note how the boundary has been different, also this is from the *get_all* function that pulls in survey locations that are not a part of the standardized survey, remove them
dog <- filter(final, !fishing_event_id %in% c(hbll$fishing_event_id))

# #quick check of hbll gfdata pull with hbll get_all pull
# hbllgfdata <- readRDS("data-raw/dogfish_sets_gfdata.rds") |> filter(survey_series_id %in% c(39, 40))
#
# #
# hbll <- filter(hbll, fishing_event_id %in% c(hbllgfdata$fishing_event_id))
# #
# hbllgf <- hbllgfdata |>
#   group_by(year) |>
#   filter(survey_series_id %in% c(40)) |>
#   drop_na(catch_count) |>
#   reframe(sum = sum(catch_count))
# ggplot(hbllgf) +
#   geom_line(aes(year, sum), col = "red") +
#   geom_point(aes(year, sum), col = "red")
# hbllpe <- hbll |>
#   filter(survey_series_id %in% c(39)) |>
#   group_by(year) |>
#   drop_na(catch_count) |>
#   reframe(sumpe = sum(catch_count))
#
# x <- left_join(hbllgf, hbllpe)
# x$diff <- x$sum - x$sumpe # slight discrepancies between the two different data pulls.
# #
# ggplot(x) +
#   geom_line(aes(year, sum), col = "red") +
#   geom_line(aes(year, sumpe)) # slight discrepancies between the two different data pulls. Could the be the points that extend around may be partially. 2007 is in the in N and there are two extra points in the get_all data pull. THey are removed now to match the gfdata pull.


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

# put cleaned hbll and dog back together-------------------------------------------------------

final <- bind_rows(dog, hbll)

final |>
  group_by(survey_lumped, year) |>
  distinct() |>
  reframe() |>
  print(n = 40) # looks good

ggplot(final, aes(longitude, latitude, colour = survey_lumped)) +
  geom_point() +
  facet_wrap(~survey_lumped)

ggplot(final, aes(longitude, latitude, colour = survey_lumped)) +
  geom_point() +
  facet_wrap(~survey_sep)

# convert to UTMs
d <- add_utm_columns(final,
  ll_names = c("longitude", "latitude"),
  utm_names = c("UTM.lon", "UTM.lat"),
  utm_crs = bccrs
) |>
  mutate(UTM.lon.m = UTM.lon * 1000, UTM.lat.m = UTM.lat * 1000)

saveRDS(d, "data-raw/wrangled-hbll-dog-sets.rds")
