# library -----------------------------------------------------------------
library(sf)
library(ggplot2)
library(tidyverse)
library(sp)

# load data ---------------------------------------------------------------

samples <- readRDS("data-raw/dogfish_samples.rds")
sets <- readRDS("data-raw/dogfish_sets.rds")
catchcount <- readRDS("data-raw/dogfish_counts.rds")

names(samples) <- tolower(names(samples))
names(sets) <- tolower(names(sets))


# get hook info into samples df -------------------------------------------
sets_fei <- sets |>
  filter(survey_series_id != 76) |> # get rid of 76 as its a duplicate of survey series but does not include comp work
  dplyr::select(fishing_event_id, fe_sub_level_id, hook_desc, hooksize_desc, year)

samples1 <- samples |>
  filter(!year %in% c(2004, 2022, 2023)) |>
  left_join(sets_fei)

samples2 <- samples |>
  filter(year %in% c(2004, 2022, 2023)) |>
  left_join(sets_fei, by = c(
    # "fishing_event_id" = "fishing_event_id",
    "fe_sub_level_id" = "fe_sub_level_id",
    "fe_parent_event_id" = "fishing_event_id",
    "year" = "year"
  ))

samples <- bind_rows(samples1, samples2)

x <- filter(samples, is.na(hook_desc) == TRUE)
unique(samples$survey_desc)
unique(x$year)

samples <- samples |>
  mutate(survey3 = case_when(
    year %in% c(1986, 1989) ~ "dog-jhook",
    year %in% c(2005, 2008, 2011, 2014) ~ "dog",
    year == 2019 & survey_desc == "2019 Strait of Georgia Longline Dogfish Survey" ~ "dog",
    year == 2019 & survey_desc == "2019 Dogfish Gear/Timing Comparison Survey" & hooksize_desc == "13/0" ~ "HBLL INS S comp",
    year == 2019 & survey_desc == "2019 Dogfish Gear/Timing Comparison Survey" & hooksize_desc == "14/0" ~ "dog comp",
    year == 2023 & survey_desc == "2023 Dogfish Gear Comparison Survey" & hooksize_desc == "14/0" ~ "dog",
    year == 2023 & survey_desc == "2023 Dogfish Gear Comparison Survey" & hooksize_desc == "13/0" ~ "HBLL INS S comp",
    year == 2023 & survey_desc == "The 2023 Summer Dogfish gear comparison survey on the Neocaligus." & hooksize_desc == "14/0" ~ "dog comp",
    year == 2023 & survey_desc == "The 2023 Summer Dogfish gear comparison survey on the Neocaligus." & hooksize_desc == "13/0" ~ "HBLL INS S comp",
    year == 2023 & survey_desc == "2023 Dogfish Gear Comparison Survey" & hooksize_desc == "12/0" ~ "dog-jhook",
    year == 2022 & hooksize_desc == "13/0" ~ "HBLL INS S",
    year == 2022 & hooksize_desc == "14/0" ~ "dog comp",
    year == 2004 & hooksize_desc == "14/0" ~ "dog",
    year == 2004 & hooksize_desc == "12/0" ~ "dog-jhook"
  ))


samples <- samples |>
  mutate(survey2 = case_when(
    survey3 == "dog-jhook" ~ "dog-jhook",
    survey3 == "dog" ~ "dog",
    survey3 == "HBLL INS S comp" ~ "hbll",
    survey3 == "dog comp" ~ "dog",
    survey3 == "HBLL INS S" ~ "hbll"
    ))

samples <- samples |>
  mutate(survey_abbrev = case_when(
    survey3 == "dog-jhook" ~ "dog-jhook",
    survey3 == "dog" ~ "dog",
    survey3 == "HBLL INS S comp" ~ "HBLL INS S",
    survey3 == "dog comp" ~ "dog",
    survey3 == "HBLL INS S" ~ "HBLL INS S"
  ))

x <- filter(samples, is.na(survey3) == TRUE)
unique(x$survey_desc)
unique(x$fishing_event_id) # why does this one fishing event not have a parent event id??, this is also not found in the sets dataframe

# remove for now
samples <- filter(samples, fishing_event_id != 5490376)
saveRDS(samples, "data-raw/dogfish_samples_cleaned.rds")

dog <- filter(samples, species_common_name == "NORTH PACIFIC SPINY DOGFISH")
# maturity information
library(gfplot)
data("maturity_assignment")
data("maturity_short_names") # males maturity code = 90, female maturity code is >= 77
maturity_assignment
maturity_short_names

dog_maturity_short_names <- filter(maturity_short_names, maturity_convention_desc == "DOGFISH")
dsurvey_bio2 <- left_join(dog, dog_maturity_short_names, by = c("specimen_sex_code" = "sex", "maturity_code" = "maturity_code"))
saveRDS(dsurvey_bio2, "data-raw/dogfish_samples_cleaned_withmaturity.rds")
