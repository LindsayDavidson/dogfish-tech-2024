# library -----------------------------------------------------------------
library(ggplot2)
library(tidyverse)



# load data ---------------------------------------------------------------
final <- readRDS("data-generated/dogfishs_allsets_allspecies_counts.rds")
samples <- readRDS("data-raw/dogfish_samples.rds")

jhook <- final |> filter(year == 2023 & cat2 == "comp-dogJ-HOOK")
final <- final |> filter(!fishing_event_id %in% (jhook$fishing_event_id))
final <- filter(final, species_code == "044")


# Plots - set level summary plots -----------------------------------------
final |>
  ggplot() +
  geom_jitter(aes(year, soak, colour = cat2)) +
  theme_classic()
ggsave("Figures/soak-time-variability.png", width = 6, height = 4)

final |>
  ggplot() +
  geom_jitter(aes(year, julian, colour = cat2)) +
  theme_classic() + scale_colour_viridis_d()
ggsave("Figures/julian-variability.png", width = 6, height = 4)

final |>
  ggplot() +
  geom_jitter(aes(grouping_depth_id, catch_count, group = cat2, colour = cat2)) +
  facet_wrap(~year) + theme_classic() + scale_colour_viridis_d(option = "mako")
ggsave("Figures/raw-catch-count-by-survey-depth.png", width = 6, height = 4)

final |>
  group_by(cat2) |>
  ggplot() +
  geom_jitter(aes(year, log(catch_count), group = cat2, colour = cat2)) +
  theme_classic() + scale_colour_viridis_d(option = "mako")
ggsave("Figures/raw-catch-count-by-survey.png", width = 6, height = 4)

df |>
  filter(year %in% c(2005, 2008, 2011, 2014, 2019) & category == "dog") |>
  ggplot() +
  geom_point(aes(grouping_depth_id, catch_count, group = grouping_spatial_id, colour = grouping_spatial_id)) +
  geom_line(aes(grouping_depth_id, catch_count, group = grouping_spatial_id, colour = grouping_spatial_id)) +
  facet_wrap(~year) + theme_classic() + scale_colour_viridis_d(option = "mako")
ggsave("Figures/dog-survey-by-site.png", width = 4, height = 4)

# ggplot(df, aes(grouping_depth_id, catch_count, group = hooksize_desc, colour = hooksize_desc)) +
#   geom_jitter() +
#   facet_wrap(~year, scales = "free_y")

final |>
  group_by(grouping_depth_id, year, hooksize_desc) |>
  drop_na(soak) |>
  reframe(sum = sum(catch_count) / sum(lglsp_hook_count * soak)) |>
  ggplot(aes(grouping_depth_id, sum, group = hooksize_desc, colour = hooksize_desc)) +
  geom_point() +
  geom_line() +
  facet_wrap(~year, scales = "free_y") + theme_classic() + scale_colour_viridis_d(option = "mako")



# Plots - sample level summary plots --------------------------------------

samples |> group_by(year, category) |>
  ggplot() +
  geom_jitter(aes(year, total_length, colour = category)) + theme_classic()

samples |> group_by(year, category) |>
  ggplot() +
  geom_jitter(aes(year, total_length, colour = category)) +
  facet_wrap(~specimen_sex_code)
ggsave("Figures/length-variability.png", width = 4, height = 4)

samples |>
  filter(year %in% c(2005, 2008, 2011, 2014, 2019) & category == "dog") |>
  filter(specimen_sex_code %in% c(1,2)) |>
  ggplot() +
  geom_jitter(aes(year, total_length), alpha = 0.5) +
  facet_wrap(~specimen_sex_code) + theme_classic()
ggsave("Figures/length-variability-dogsurv.png", width = 4, height = 4)
