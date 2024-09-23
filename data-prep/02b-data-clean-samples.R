# library -----------------------------------------------------------------
library(sf)
library(ggplot2)
library(tidyverse)
library(sp)

# load data ---------------------------------------------------------------

samps <- readRDS("data-raw/dogfish_samples_getall.rds")
# samples <- readRDS("data-raw/dogfish_samples.rds")
# sets <- readRDS("data-raw/dogfish_sets.rds")
# catchcount <- readRDS("data-raw/dogfish_counts.rds")
#
# names(samples) <- tolower(names(samples))
# names(sets) <- tolower(names(sets))


# get hook info into samples df -------------------------------------------
# sets_fei <- sets |>
#   filter(survey_series_id != 76) |> # get rid of 76 as its a duplicate of survey series but does not include comp work
#   dplyr::select(fishing_event_id, fe_sub_level_id, hook_desc, hooksize_desc, year)
#
# samples1 <- samples |>
#   filter(!year %in% c(2004, 2022, 2023)) |>
#   left_join(sets_fei)
#
# samples2 <- samples |>
#   filter(year %in% c(2004, 2022, 2023)) |>
#   left_join(sets_fei, by = c(
#     # "fishing_event_id" = "fishing_event_id",
#     "fe_sub_level_id" = "fe_sub_level_id",
#     "fe_parent_event_id" = "fishing_event_id",
#     "year" = "year"
#   ))
#
# samples <- bind_rows(samples1, samples2)
#
# x <- filter(samples, is.na(hook_desc) == TRUE)
# unique(samples$survey_desc)
# unique(x$year)

# samples <- samples |>
#   mutate(survey3 = case_when(
#     year %in% c(1986, 1989) ~ "dog-jhook",
#     year %in% c(2005, 2008, 2011, 2014) ~ "dog",
#     year == 2019 & survey_desc == "2019 Strait of Georgia Longline Dogfish Survey" ~ "dog",
#     year == 2019 & survey_desc == "2019 Dogfish Gear/Timing Comparison Survey" & hooksize_desc == "13/0" ~ "HBLL INS S comp",
#     year == 2019 & survey_desc == "2019 Dogfish Gear/Timing Comparison Survey" & hooksize_desc == "14/0" ~ "dog comp",
#     year == 2023 & survey_desc == "2023 Dogfish Gear Comparison Survey" & hooksize_desc == "14/0" ~ "dog",
#     year == 2023 & survey_desc == "2023 Dogfish Gear Comparison Survey" & hooksize_desc == "13/0" ~ "HBLL INS S comp",
#     year == 2023 & survey_desc == "The 2023 Summer Dogfish gear comparison survey on the Neocaligus." & hooksize_desc == "14/0" ~ "dog comp",
#     year == 2023 & survey_desc == "The 2023 Summer Dogfish gear comparison survey on the Neocaligus." & hooksize_desc == "13/0" ~ "HBLL INS S comp",
#     year == 2023 & survey_desc == "2023 Dogfish Gear Comparison Survey" & hooksize_desc == "12/0" ~ "dog-jhook",
#     year == 2022 & hooksize_desc == "13/0" ~ "HBLL INS S",
#     year == 2022 & hooksize_desc == "14/0" ~ "dog comp",
#     year == 2004 & hooksize_desc == "14/0" ~ "dog",
#     year == 2004 & hooksize_desc == "12/0" ~ "dog-jhook"
#   ))
#
#
# samples <- samples |>
#   mutate(survey2 = case_when(
#     survey3 == "dog-jhook" ~ "dog-jhook",
#     survey3 == "dog" ~ "dog",
#     survey3 == "HBLL INS S comp" ~ "hbll",
#     survey3 == "dog comp" ~ "dog",
#     survey3 == "HBLL INS S" ~ "hbll"
#     ))
#
# samples <- samples |>
#   mutate(survey_abbrev = case_when(
#     survey3 == "dog-jhook" ~ "dog-jhook",
#     survey3 == "dog" ~ "dog",
#     survey3 == "HBLL INS S comp" ~ "HBLL INS S",
#     survey3 == "dog comp" ~ "dog",
#     survey3 == "HBLL INS S" ~ "HBLL INS S"
#   ))

glimpse(samps)

samplesf <- samps |>
  mutate(survey_sep = case_when(
    survey_abbrev == "HBLL INS S" ~ "HBLL INS S",
    survey_abbrev == "HBLL INS N" ~ "HBLL INS N",
    year %in% c(1986, 1989) ~ "dog-jhook",
    year %in% c(2005, 2008, 2011, 2014) ~ "dog",
    year == 2019 & survey_abbrev == "DOG" ~ "dog",
    year == 2019 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" & hooksize_desc == "13/0" ~ "hbll comp",
    year == 2019 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" & hooksize_desc == "14/0" ~ "dog comp",
    # year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" & hooksize_desc == "14/0" ~ "dog",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" & hooksize_desc == "12/0" ~ "dog-jhook",
    # year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" & hooksize_desc == "13/0" ~ "hbll comp",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" & hooksize_desc == "14/0" & month == 9 & day >= 27 ~ "dog",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" & hooksize_desc == "13/0" & month == 9 & day >= 27 ~ "hbll comp",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" & hooksize_desc == "13/0" & month == 9 & day < 27 ~ "HBLL INS N",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" & hooksize_desc == "14/0" & month == 9 & day < 27 ~ "dog comp",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" & hooksize_desc == "13/0" & month == 8 ~ "HBLL INS N",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" & hooksize_desc == "14/0" & month == 8 ~ "dog comp",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" & hooksize_desc == "13/0" & month == 10 ~ "hbll comp",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" & hooksize_desc == "14/0" & month == 10 ~ "dog",
    year == 2022 & hooksize_desc == "13/0" ~ "hbll comp",
    year == 2022 & hooksize_desc == "14/0" ~ "dog comp",
    year == 2004 & hooksize_desc == "14/0" ~ "dog",
    year == 2004 & hooksize_desc == "12/0" ~ "dog-jhook"
  ))

#so I can put the different surveys into a model and account for julian date after (the seasonal component of the comparison work)
samplesf <- samplesf |>
  mutate(survey_lumped = case_when(
    survey_sep == "HBLL INS S" ~ "hbll",
    survey_sep == "HBLL INS N" ~ "hbll",
    survey_sep == "dog-jhook" ~ "dog-jhook",
    survey_sep == "dog" ~ "dog",
    survey_sep == "hbll comp" ~ "hbll",
    survey_sep == "dog comp" ~ "dog",
    survey_sep == "hbll" ~ "hbll"
  ))


x <- filter(samplesf, is.na(survey_sep) == TRUE)
unique(x$survey_abbrev)
unique(x$fishing_event_id) # why does this one fishing event not have a parent event id??, this is also not found in the sets dataframe

# remove for now
samples <- filter(samplesf, fishing_event_id != 5490376) #<- check this
saveRDS(samples, "data-raw/dogfish_samples_cleaned.rds")

# dog <- filter(samples, species_common_name == "NORTH PACIFIC SPINY DOGFISH")
# # maturity information
# library(gfplot)
# data("maturity_assignment")
# data("maturity_short_names") # males maturity code = 90, female maturity code is >= 77
# maturity_assignment
# maturity_short_names
#
# dog_maturity_short_names <- filter(maturity_short_names, maturity_convention_desc == "DOGFISH")
# #if maturities aren't looked at give a maturity_code == 0
# dog <- dog |> mutate(maturity_code = ifelse(is.na(maturity_code) == TRUE, 0, maturity_code))
# dsurvey_bio2 <- left_join(dog, dog_maturity_short_names, by = c("specimen_sex_code" = "sex", "maturity_code" = "maturity_code"))
# dsurvey_bio2 <- dsurvey_bio2 |> mutate(maturity_convention_code = ifelse(maturity_code == 0, 9, maturity_convention_code))
# dsurvey_bio2 <- dsurvey_bio2 |> mutate(maturity_convention_desc = ifelse(maturity_code == 0, "MATURITIES NOT LOOKED AT", maturity_convention_desc))
# dsurvey_bio2 <- dsurvey_bio2 %>% rename_at('Maturity Convention Max Value', ~'maturity_convention_maxvalue')
# dsurvey_bio2 <- dsurvey_bio2 |> mutate(maturity_convention_maxvalue = ifelse(maturity_code == 0, 0, maturity_convention_maxvalue))
#
# #there are a lot of codes I don't know what they are
# #codes <- unique(dog_maturity_short_names$maturity_code)
# #dsurvey_bio3 <- dsurvey_bio2 |> filter(maturity_code %in% codes)
# saveRDS(dsurvey_bio2, "data-raw/dogfish_samples_cleaned_withmaturity.rds")
