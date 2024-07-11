# Length and catch composition differences between the DOG and HBLL surveys

# library -----------------------------------------------------------------
library(gfdata)
library(gfplot)
library(tidyverse)
library(here)
library(sdmTMB)
library(sf)
library(sp)




# load data ---------------------------------------------------------------
# samps <- readRDS("output/dogfish_samps.rds")
hsamps <- readRDS("data-raw/samples-hbll-dog.rds") |>
  filter(!fishing_event_id %in% c(hbllrm$fishing_event_id))
range(hsamps$length)

dsamps <- readRDS("data-raw/dogfish_samples_cleaned.rds") |>
  filter(species_common_name == "NORTH PACIFIC SPINY DOGFISH") |>
  drop_na(total_length) |>
  filter(total_length > 0) |>
  mutate(total_length = total_length/10)
range(dsamps$total_length)
unique(dsamps$survey)

# set data (to remove fishing sets)
hbll <- readRDS("data-raw/hbllsets.rds")
final <- readRDS("data-generated/dogfish_sets_cleaned.rds") |>
  filter(species_code == "044") # pull just dogfish data not other species




# hbll clean up ------------------------------------------------------------

# remove two survey years that extended along the west coast VI
hbll1 <- filter(hbll, (latitude < 48.5 & longitude < -123.3)) # only two years have the sampling around the strait
hbll2 <- filter(hbll, (latitude < 48.75 & longitude < -124.25))
hbllrm <- bind_rows(hbll1, hbll2)
unique(hbllrm$fishing_event_id) # rm these

test <- filter(hbll, survey_abbrev == "HBLL INS S")
x <- ggplot(test) +
  geom_point(aes(longitude, latitude))
x + geom_point(data = hbllrm, aes(longitude, latitude), col = "red")


ggplot(test) +
  geom_point(aes(longitude, latitude)) + facet_wrap(~year)




# wrangle -----------------------------------------------------------------

hsamps <- hsamps |>
  filter(!fishing_event_id %in% c(hbllrm$fishing_event_id))

hsamps2 <- hsamps |> dplyr::select(
  survey_abbrev, year, month, species_common_name,
  sex, length, weight, fishing_event_id
)

dsamps <-
  dsamps |>
  mutate(
    date= as.Date(trip_start_date, format = "%Y-%m-%d"),
    julian = lubridate::yday(date),
    month = lubridate::month(date))


dsamps2 <- dsamps |>
  dplyr::select(
    survey, year, month, species_common_name,
    specimen_sex_code, total_length, round_weight, fishing_event_id
  ) |>
  rename(
    survey_abbrev = survey, sex = specimen_sex_code, length = total_length,
    weight = round_weight
  )

samps <- bind_rows(dsamps2, hsamps2)
saveRDS(samps, "output/samps_joined.rds")


# summary plot ------------------------------------------------------------
ggplot(samps, aes(year, length, col = survey_abbrev)) +
  geom_jitter()

ggplot(samps, aes(year, length, col = survey_abbrev)) +
  geom_jitter() +
  facet_wrap(~sex)


