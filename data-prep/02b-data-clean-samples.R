# library -----------------------------------------------------------------
library(sf)
library(ggplot2)
library(tidyverse)
library(sp)
library(lubridate)

spring <- seq(12, 25, 1)
summer <- seq(26, 38, 1)
winter <- c(seq(1, 11, 1), 52)
fall <- seq(39, 51, 1)

# load data ---------------------------------------------------------------

samps <- readRDS("data-raw/dogfish_samples_getall.rds")
sets <- readRDS("data-generated/dogfish_sets_cleaned_getall.rds") |>
  dplyr::select(year, fishing_event_id, week) |>
  distinct()
samps <- left_join(samps, sets)
samps |> filter(is.na(week) == TRUE) |> tally()

samplesf <- samps |>
  mutate(survey_sep = case_when(
    survey_abbrev == "HBLL INS S" ~ "HBLL INS S",
    survey_abbrev == "HBLL INS N" ~ "HBLL INS N",
    year %in% c(1986, 1989) ~ "dog-jhook",
    year %in% c(2005, 2008, 2011, 2014) & survey_abbrev == "DOG" ~ "dog",
    year == 2019 & survey_abbrev == "DOG" ~ "dog",
    year == 2019 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" &
      hooksize_desc == "13/0" ~ "hbll comp",
    year == 2019 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" &
      hooksize_desc == "14/0" ~ "dog comp",
    # year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" & hooksize_desc == "14/0" ~ "dog",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" &
      hooksize_desc == "12/0" ~ "dog-jhook",
    # year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" & hooksize_desc == "13/0" ~ "hbll comp",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" &
      hooksize_desc == "14/0" & month == 9 & day >= 27 ~ "dog comp",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" &
      hooksize_desc == "13/0" & month == 9 & day >= 27 ~ "hbll comp",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" &
      hooksize_desc == "13/0" & month == 9 & day < 27 ~ "hbll comp",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" &
      hooksize_desc == "14/0" & month == 9 & day < 27 ~ "dog comp",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" &
      hooksize_desc == "13/0" & month == 8 ~ "hbll comp",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" &
      hooksize_desc == "14/0" & month == 8 ~ "dog comp",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" &
      hooksize_desc == "13/0" & month == 10 ~ "hbll comp",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" &
      hooksize_desc == "14/0" & month == 10 ~ "dog comp",
    year == 2022 & hooksize_desc == "13/0" & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" ~ "hbll comp",
    year == 2022 & hooksize_desc == "14/0" & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" ~ "dog comp",
    year == 2004 & hooksize_desc == "14/0" & survey_abbrev == "OTHER" ~ "dog comp",
    year == 2004 & hooksize_desc == "12/0" & survey_abbrev == "OTHER" ~ "dog-jhook"
  ))

samplesf <- samplesf |>
  mutate(survey_timing = case_when(
    survey_abbrev == "HBLL INS S" ~ "HBLL INS S",
    survey_abbrev == "HBLL INS N" ~ "HBLL INS N",
    year %in% c(1986, 1989) ~ "dog-jhook",
    year %in% c(2005, 2008, 2011, 2014) & survey_abbrev == "DOG" ~ "dog",
    year == 2019 & survey_abbrev == "DOG" ~ "dog",
    year == 2019 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" &
      hooksize_desc == "13/0" ~ "hbll",
    year == 2019 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" &
      hooksize_desc == "14/0" ~ "hbll",
    # year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" & hooksize_desc == "14/0" ~ "dog",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" &
      hooksize_desc == "12/0" ~ "dog-jhook",
    # year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" & hooksize_desc == "13/0" ~ "hbll comp",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" &
      hooksize_desc == "14/0" & month == 9 & day >= 27 ~ "dog",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" &
      hooksize_desc == "13/0" & month == 9 & day >= 27 ~ "dog",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" &
      hooksize_desc == "13/0" & month == 9 & day < 27 ~ "hbll",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" &
      hooksize_desc == "14/0" & month == 9 & day < 27 ~ "hbll",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" &
      hooksize_desc == "13/0" & month == 8 ~ "hbll",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" &
      hooksize_desc == "14/0" & month == 8 ~ "hbll",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" &
      hooksize_desc == "13/0" & month == 10 ~ "dog",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" &
      hooksize_desc == "14/0" & month == 10 ~ "dog",
    year == 2022 & hooksize_desc == "13/0" & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" ~ "hbll",
    year == 2022 & hooksize_desc == "14/0" & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" ~ "hbll",
    year == 2004 & hooksize_desc == "14/0" & survey_abbrev == "OTHER" ~ "dog comp",
    year == 2004 & hooksize_desc == "12/0" & survey_abbrev == "OTHER" ~ "dog-jhook"
  ))

#add season based on week of survey and definitions above
samplesf <- samplesf |>
  mutate(season = ifelse(week %in% spring, "2", ifelse(week %in% summer, "3", ifelse(week %in% fall, "4", "1"))))

# so I can put the different surveys into a model and account for julian date after (the seasonal component of the comparison work)
samplesf <- samplesf |>
  mutate(survey_lumped = case_when(
    survey_sep %in% c("HBLL INS S", "HBLL INS N", "hbll", "hbll comp") ~ "hbll",
    survey_sep == "dog-jhook" ~ "dog-jhook",
    survey_sep %in% c("dog", "dog comp") ~ "dog"
  ))


x <- filter(samplesf, is.na(survey_sep) == TRUE)
unique(x$survey_abbrev)
unique(x$fishing_event_id) # why does this one fishing event not have a parent event id??, this is also not found in the sets dataframe

samplesf <-
  samplesf |>
  mutate(
    date = as.Date(sample_date, format = "%Y-%m-%d"),
    julian = lubridate::yday(date),
    month = lubridate::month(date)
  )

# remove for now
samples <- filter(samplesf, fishing_event_id != 5490376) #<- check this
saveRDS(samples, "data-raw/dogfish_samples_cleaned.rds")
