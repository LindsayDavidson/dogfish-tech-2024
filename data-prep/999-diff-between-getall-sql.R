
# library -----------------------------------------------------------------
library(tidyverse)
library(sf)
library(ggplot2)
library(here)
library(sp)
library(gfdata)
remotes::install_github("pbs-assess/gfdata", ref = "trials")


# pull inside samples data ------------------------------------------------
df <- readRDS("data-raw/dogfish_sets.rds")
count <- readRDS("data-raw/dogfish_counts.rds")
names(count) <- tolower(names(count))
names(df) <- tolower(names(df))
count <- filter(count, species_common_name == "NORTH PACIFIC SPINY DOGFISH")

compsurveys <- df |>
  filter(survey_series_id == 48 & year %in% c(2004, 2022, 2023)) |>
  left_join(count, by = c("fishing_event_id" = "fe_parent_event_id", "fe_sub_level_id" = "fe_sub_level_id")) |>
  select(-fishing_event_id.y) |>
  mutate(catch_count = ifelse(is.na(catch_count) == TRUE, 0, catch_count))

d <- readRDS("data-raw/dogfish_sets_getall.rds") #get all function

# whats the diff between get_all and sql pull --------------------------------
dtest <- d |>
  filter(! survey_abbrev %in% c("HBLL INS S", "HBLL INS N")) |>
  filter(year %in% c(2004, 2022, 2023)) |>
  rename(catch_count_pe = catch_count) |>
  dplyr::select(year, fishing_event_id, hooksize_desc, hook_desc, catch_count_pe)

ldtest <- compsurveys |>
  rename(catch_count_ld = catch_count) |>
  dplyr::select(year, fishing_event_id, hooksize_desc, fe_sub_level_id, fe_parent_event_id, hook_desc, catch_count_ld)

comb <- left_join(dtest, ldtest)
comb$diff <- comb$catch_count_pe/comb$catch_count_ld #an issue for 2022 and 2023 but not 2004


# whats the diff between get_all and sql pull not comp work--------------------------------

dtest <- d |>
  filter(!survey_abbrev %in% c("HBLL INS S", "HBLL INS N")) |>
  #filter(year ==2023) |>
  filter(year %in% c(1986, 1989)) |>
  rename(catch_count_pe = catch_count) |>
  dplyr::select(year, fishing_event_id, hooksize_desc, hook_desc, catch_count_pe)

ldtest <- df |>
  filter(survey_series_id == 76 & year %in% c(1986, 1989)) |>
  #left_join(count, by = c("fishing_event_id" = "fe_parent_event_id", "fe_sub_level_id" = "fe_sub_level_id")) |>
  left_join(count) |>
  #select(-fishing_event_id.y) |>
  rename(catch_count_ld = catch_count) |>
  mutate(catch_count_ld = ifelse(is.na(catch_count_ld) == TRUE, 0, catch_count_ld)) |>
  dplyr::select(year, fishing_event_id, hooksize_desc, fe_sub_level_id, fe_parent_event_id, hook_desc, catch_count_ld)
comb <- left_join(dtest, ldtest)
comb$diff <- comb$catch_count_pe/comb$catch_count_ld #looks good

# whats the diff between get_all and sql pull 2019 ------------------------------

dtest <- d |>
  filter(!survey_abbrev %in% c("HBLL INS S", "HBLL INS N")) |>
  #filter(year ==2023) |>
  filter(year  == 2019) |>
  rename(catch_count_pe = catch_count) |>
  dplyr::select(year, fishing_event_id, hooksize_desc, hook_desc, catch_count_pe)

ldtest <- df |>
  filter(survey_series_id %in% c(48, 93) & year == 2019) |>
  left_join(count) |>
  rename(catch_count_ld = catch_count) |>
  mutate(catch_count_ld = ifelse(is.na(catch_count_ld) == TRUE, 0, catch_count_ld)) |>
  dplyr::select(year, fishing_event_id, hooksize_desc, fe_sub_level_id, fe_parent_event_id, hook_desc, catch_count_ld)
comb <- left_join(dtest, ldtest)
comb$diff <- comb$catch_count_pe/comb$catch_count_ld #looks good for the comp and survey work






