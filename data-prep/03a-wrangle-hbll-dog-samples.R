# Length and catch composition differences between the DOG and HBLL surveys

# library -----------------------------------------------------------------
library(gfdata)
library(gfplot)
library(tidyverse)
library(here)
library(sdmTMB)
library(sf)
library(sp)


# params ------------------------------------------------------------------

latitude_cutoff <- 49.93883



# load data ---------------------------------------------------------------
# samps <- readRDS("output/dogfish_samps.rds")
hsamps <- readRDS("data-raw/samples-hbll-dog.rds") #just hbll samples
names(hsamps)

#dsamps <- readRDS("data-raw/dogfish_samples_cleaned.rds") |>
dsamps <- readRDS("data-raw/dogfish_samples_cleaned_withmaturity.rds") |>
  filter(species_common_name == "NORTH PACIFIC SPINY DOGFISH") |>
  drop_na(total_length) |>
  filter(total_length > 0) |>
  mutate(total_length = total_length/10)

# set data (to remove fishing sets)
hbll <- readRDS("data-raw/hbllsets.rds")
final <- readRDS("data-generated/dogfish_sets_cleaned.rds") |>
  filter(species_code == "044") # pull just dogfish data not other species

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

glimpse(hsamps)
hsamps <- hsamps |>
  filter(!fishing_event_id %in% c(hbllrm$fishing_event_id))
names(hsamps)

hsamps2 <- hsamps |>
  dplyr::select(
  survey_abbrev, year, month, species_common_name,
  sex, length, weight, fishing_event_id, survey_abbrev,
  trip_start_date, usability_code,
  sample_id, maturity_code, maturity_name, maturity_desc,
  maturity_convention_code, maturity_convention_desc, maturity_convention_maxvalue,
  age
) |> mutate(
  hook_desc = "CIRCLE HOOK", hooksize_desc = "13/0", activity_desc = "HBLL",
)

hsamps2 <- hsamps2 |>
  mutate(survey2 = case_when(
    survey_abbrev == "HBLL INS S" ~ "hbll",
    survey_abbrev == "HBLL INS N" ~ "hbll"
  ))

hsamps2 <- hsamps2 |>
  mutate(survey3 = case_when(
    survey_abbrev == "HBLL INS S" ~ "HBLL INS S",
    survey_abbrev == "HBLL INS N" ~ "HBLL INS N"
  ))

dsamps <-
  dsamps |>
  mutate(
    date= as.Date(trip_start_date, format = "%Y-%m-%d"),
    julian = lubridate::yday(date),
    month = lubridate::month(date))


dsamps2 <- dsamps |>
  dplyr::select(
    year, month, species_common_name, usability_code, trip_start_date,
    specimen_sex_code, total_length, round_weight, fishing_event_id, month, fishing_event_id, hooksize_desc, hook_desc, activity_desc, survey2, survey3, survey_abbrev, sample_id, specimen_id,
    maturity_code, maturity_name, maturity_desc,
    maturity_convention_code, maturity_convention_desc, maturity_convention_maxvalue,
    specimen_age
  ) |>
  rename(
    sex = specimen_sex_code,
    age = specimen_age,
    length = total_length,
    weight = round_weight
  )

samps <- bind_rows(dsamps2, hsamps2)

saveRDS(samps, "output/samps_joined.rds")


# summary plot ------------------------------------------------------------
ggplot(samps, aes(year, length, col = survey_abbrev)) +
  geom_jitter() + facet_wrap(~survey2)

ggplot(samps, aes(year, maturity_code, col = survey_abbrev)) +
  geom_jitter() + facet_wrap(~survey3)

ggplot(samps, aes(year, maturity_code, col = survey_abbrev)) +
  geom_jitter() + facet_wrap(~survey3)

ggplot(samps, aes(year, length, col = survey_abbrev)) +
  geom_jitter() +
  facet_wrap(~sex)

ggplot(samps, aes(year, length, col = hook_desc)) +
  geom_jitter() +
  facet_wrap(~sex)

