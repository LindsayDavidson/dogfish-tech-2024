final <- readRDS("data/generated/dogfishs_allsets_allspecies_counts.rds")

# how many rockfish captured at depth two across sites?
final |>
  filter(species_code == "442") |>
  filter(is.na(grouping_depth_id) != TRUE) |>
  group_by(grouping_spatial_id, grouping_depth_id) |>
  reframe(yelloweye_catch_count = sum(catch_count), yelloweye_mean_catchcount = mean(catch_count)) |>
  filter(grouping_depth_id == 2)

final |>
  filter(species_code == "442") |>
  filter(is.na(grouping_depth_id) != TRUE) |>
  group_by(grouping_spatial_id, grouping_depth_id, year) |>
  reframe(yelloweye_catch_count = sum(catch_count)) |>
  filter(grouping_depth_id == 2)

final |>
  filter(species_code == "442") |>
  filter(is.na(grouping_depth_id) != TRUE) |>
  group_by(grouping_spatial_id, grouping_depth_id) |>
  reframe(sum = sum(catch_count)) |>
  ggplot() +
  geom_point(aes(grouping_depth_id, sum, group = grouping_spatial_id, colour = grouping_spatial_id)) +
  geom_line(aes(grouping_depth_id, sum, group = grouping_spatial_id, colour = grouping_spatial_id)) +
  facet_wrap(~grouping_spatial_id)

# 2019 hook comparison catch and effort results
final |>
  filter(species_code == "044") |>
  filter(year == 2019 & survey_series_id == 48) |>
  filter(is.na(grouping_depth_id) != TRUE) |>
  group_by(hooksize_desc, grouping_depth_id) |>
  reframe(
    sum = sum(catch_count),
    sumeffort = sum(lglsp_hook_count * soak),
    sumcpue = sum / sumeffort
  ) |>
  drop_na(hooksize_desc) |>
  ggplot() +
  geom_point(aes(grouping_depth_id, sum,
                 group = as.factor(hooksize_desc),
                 colour = as.factor(hooksize_desc)
  ), size = 2) +
  geom_line(aes(grouping_depth_id, sum,
                group = as.factor(hooksize_desc),
                colour = as.factor(hooksize_desc)
  ), size = 1)

final |>
  filter(species_code == "044") |>
  filter(year == 2019 & survey_series_id == 48) |>
  filter(is.na(grouping_depth_id) != TRUE) |>
  group_by(hooksize_desc, grouping_depth_id, fishing_event_id) |>
  reframe(cpue = sum(catch_count / sum(lglsp_hook_count * soak))) |>
  group_by(hooksize_desc, grouping_depth_id) |>
  reframe(sumcpue = sum(cpue)) |>
  drop_na(hooksize_desc) |>
  ggplot() +
  geom_point(aes(grouping_depth_id, sumcpue,
                 group = as.factor(hooksize_desc),
                 colour = as.factor(hooksize_desc)
  ), size = 2) +
  geom_line(aes(grouping_depth_id, sumcpue,
                group = as.factor(hooksize_desc),
                colour = as.factor(hooksize_desc)
  ), size = 1)

# MERGE SETS AND SAMPLES ---------------------------------------------------------
#sets <- readRDS("data/generated/dogfishs_allsets_allspecies_clean.rds")
sets <- readRDS("data/generated/sets_cleaned2.rds")
samples <- readRDS("data/raw/dogfish_samples.rds")

names(sets) <- tolower(names(sets))
names(samples) <- tolower(names(samples))

regsurveys <- samples |>
  filter(survey_series_id %in% c(93, 92)) |>
  inner_join(sets)

compsurveys <- samples |>
  filter(survey_series_id == 48 & year != 2019) |>
  left_join(sets, by = c(
    "fe_parent_event_id" = "fishing_event_id",
    "fe_sub_level_id" = "fe_sub_level_id",
    "survey_series_id" = "survey_series_id",
    "year" = "year",
    "trip_id" = "trip_id",
    "fe_major_level_id" = "fe_major_level_id"
  )) |>
  select(-"fe_parent_event_id.y")

compsurveys2019 <- samples |>
  filter(survey_series_id == 48 & year == 2019) |>
  left_join(sets)

final <- rbind(regsurveys, compsurveys, compsurveys2019)
unique(final$grouping_desc)

saveRDS(final, "data/raw/dogfishs_allsets_allsamples.rds")



# SUMMARY FIGURES - SETS --------------------------------------------------
sets <- readRDS("data/generated/dogfishs_allsets_allspecies_counts.rds")

df <- filter(sets, species_code == "442")
df <- filter(sets, species_code == "044")

sets |>
  filter(species_code == "442") |>
  group_by(year, grouping_depth_id) |>
  summarize(count = sum(catch_count)) |>
  print(n = 35)

df <- df |> mutate(cpue = catch_count / (lglsp_hook_count * soak))
glimpse(df)
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

df |>
  group_by(grouping_depth_id, year, hooksize_desc) |>
  reframe(sum = sum(catch_count)) |>
  ggplot(aes(grouping_depth_id, sum, group = hooksize_desc, colour = hooksize_desc)) +
  geom_point() +
  geom_line() +
  facet_wrap(~year, scales = "free_y")

# SUMMARY FIGURES - samples ---------------------------------------------------------
d <- readRDS("data/raw/dogfishs_allsets_allsamples.rds")

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

