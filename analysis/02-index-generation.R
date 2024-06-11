
# library -----------------------------------------------------------------

library(ggplot2)
library(tidyverse)

# load data ---------------------------------------------------------------

final <- readRDS("data-generated/dogfishs_allsets_allspecies_counts.rds")
samples <- readRDS("data-raw/dogfish_samples.rds")
#take out the 2023 J hook sets as that was for the yelloweye comparative work, not dogfish
jhook <- final |> filter(year == 2023 & cat2 == "comp-dogJ-HOOK")
final <- final |> filter(!fishing_event_id %in% (jhook$fishing_event_id))
final <- filter(final, species_code == "044")

final |> group_by(survey_series_id) |> distinct(year, .keep_all = TRUE) |>
  reframe(count = n())
final |> group_by(survey_series_id, year) |> distinct() |>
  reframe()

