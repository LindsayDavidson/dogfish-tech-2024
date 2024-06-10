# library -----------------------------------------------------------------
library(ggplot2)
library(tidyverse)



# load data ---------------------------------------------------------------
final <- readRDS("data-generated/dogfishs_allsets_allspecies_counts.rds")
final <- final |> mutate(cat2 = paste0(category, hook_desc))

#drop comp j hook in 2023
jhook <- final |> filter(year == 2023 & cat2 == "comp-dogJ-HOOK")
final <- final |> filter(!fishing_event_id %in% (jhook$fishing_event_id))

# Plots - set level summary plots -----------------------------------------
#df <- filter(final, species_code == "442")
df <- filter(final, species_code == "044")
df <- df |> mutate(cpue = catch_count / (lglsp_hook_count * soak))

ggplot(df, aes(grouping_depth_id, catch_count, group = year, colour = year)) +
  geom_jitter() +
  geom_line() +
  facet_wrap(~grouping_spatial_id)

ggplot(df, aes(grouping_depth_id, catch_count, group = cat2, colour = cat2)) +
  geom_point() +
  #geom_line() +
  facet_wrap(~year)

ggplot(df, aes(grouping_depth_id, log(catch_count), group = cat2, colour = cat2)) +
  geom_jitter() +
  #geom_line() +
  facet_wrap(~year) + theme_classic()

final |>
  group_by(year, grouping_depth_id, cat2) |>
  reframe(catch_count = sum(catch_count)) |>
  ggplot() +
  geom_point(aes(grouping_depth_id, catch_count, group = cat2, colour = cat2)) +
  geom_line(aes(grouping_depth_id, catch_count, group = cat2,  colour = cat2)) +
  facet_wrap(~year) + theme_classic()

final |>
  ggplot() +
  geom_jitter(aes(grouping_depth_id, (catch_count), group = cat2, colour = cat2)) +
  facet_wrap(~year) + theme_classic() + scale_colour_viridis_d(option = "plasma")

final |>
  group_by(cat2) |>
  #reframe(catch_count = sum(catch_count)) |>
  ggplot() +
  geom_jitter(aes(year, log(catch_count), group = cat2, colour = cat2)) +
  #geom_line(aes(year, catch_count, group = cat2,  colour = cat2)) +
  theme_classic()

final |>
  group_by(cat2) |>
  #reframe(catch_count = sum(catch_count)) |>
  ggplot() +
  geom_jitter(aes(year, log(catch_count),  colour = cat2)) +
  #geom_line(aes(year, catch_count, group = cat2,  colour = cat2)) +
  theme_classic()

ggplot(df, aes(grouping_depth_id, catch_count, group = year, colour = year)) +
  geom_point() +
  geom_line() +
  facet_wrap(~grouping_spatial_id)

ggplot(df, aes(grouping_depth_id, catch_count, group = grouping_spatial_id, colour = grouping_spatial_id)) +
  geom_point() +
  geom_line() +
  facet_wrap(~year)

ggplot(df, aes(grouping_depth_id, catch_count, group = hooksize_desc, colour = hooksize_desc)) +
  geom_jitter() +
  facet_wrap(~year, scales = "free_y")

df |>
  group_by(grouping_depth_id, year, hooksize_desc) |>
  drop_na(soak) |>
  reframe(sum = sum(catch_count) / sum(lglsp_hook_count * soak)) |>
  ggplot(aes(grouping_depth_id, sum, group = hooksize_desc, colour = hooksize_desc)) +
  geom_point() +
  geom_line() +
  facet_wrap(~year, scales = "free_y")



d |>
  ggplot() +
  geom_point(aes(year, soak)) +
  theme_classic()

d |>
  filter(species_code == "027") |>
  filter(specimen_sex_code %in% c(1, 2)) |>
  ggplot() +
  geom_point(aes((grouping_depth_id), total_length,
    colour = as.factor(hooksize_desc)
  )) +
  geom_line(aes((grouping_depth_id), total_length,
    colour = as.factor(hooksize_desc)
  )) +
  # geom_boxplot(position = position_dodge(1)) +
  facet_wrap(~ specimen_sex_code + year, nrow = 2) +
  theme_classic()

d |>
  filter(species_code == "044") |>
  filter(specimen_sex_code %in% c(1, 2)) |>
  ggplot() +
  geom_boxplot(aes((grouping_depth_id), total_length,
    colour = as.factor(hooksize_desc)
  )) +
  # geom_boxplot(position = position_dodge(1)) +
  facet_wrap(~ specimen_sex_code + year, nrow = 2) +
  theme_classic()

d |>
  filter(species_code == "044") |>
  filter(specimen_sex_code %in% c(1, 2)) |>
  ggplot() +
  geom_boxplot(aes(as.factor(year), total_length,
    colour = as.factor(hooksize_desc)
  )) +
  # geom_boxplot(position = position_dodge(1)) +
  facet_wrap(~ specimen_sex_code + grouping_depth_id, nrow = 2) +
  theme_classic()
