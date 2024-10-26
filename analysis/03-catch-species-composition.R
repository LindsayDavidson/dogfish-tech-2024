#answer (1) if catch composition are different between seasons (same hooks but different surveys)
#and (2) if catch composition is different between hooks (diff hooks same survey)

# library -----------------------------------------------------------------

library(ggplot2)
library(tidyverse)
library(here)

# load data ---------------------------------------------------------------

#samples <- readRDS("data-raw/dogfish_samples_cleaned.rds") |>
#  filter(species_code == "044")

catch_d <- readRDS("data-raw/dogfish_allspeciescatch.rds")
info <- readRDS("data-raw/dogfish_sets.rds")

names(catch_d) <- tolower(names(catch_d))
catch_d <- catch_d |> drop_na(species_science_name) |>
  filter(species_common_name !=
           "ALL SPECIES")
length(unique(catch_d$species_science_name))

catch_h <- readRDS("data-raw/hbll_allspeciescatch.rds")
info <- readRDS("data-raw/HBLL_sets.rds")
names(catch_h) <- tolower(names(catch_h))
catch_h <- catch_h |> drop_na(species_science_name) |>
  filter(species_common_name !=
           "ALL SPECIES")
length(unique(catch_h$species_science_name))

catch_h <- catch_h |>
  dplyr::select(catch_count, species_science_name, species_code, species_common_name) |>
  rename(catch_count_hbll = catch_count) |> #take a mean across sets?
  group_by(species_science_name, species_common_name) |>
  drop_na(catch_count_hbll) |>
  reframe(count_h = sum(catch_count_hbll))

catch_d <- catch_d |>
  dplyr::select(catch_count, species_science_name, species_code, species_common_name) |>
  rename(catch_count_dog = catch_count) |>
  group_by(species_science_name, species_common_name) |>
  drop_na(catch_count_dog) |>
  reframe(count_d = sum(catch_count_dog))
catch <- full_join(catch_h, catch_d)
catch <- catch |>
  drop_na(count_d) |>
  mutate(diff = count_d - count_h)
