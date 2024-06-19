# pull data ---------------------------------------------------------------

d <- readRDS("data-raw/wrangled-hbll-dog-sets.rds")


# summary plots -----------------------------------------------------------

d |>
  group_by(survey_abbrev, year) |>
  mutate(
    catch_count_sum = sum(catch_count),
    cpue = sum(catch_count) / sum(exp(offset)),
    cpue_hk = sum(catch_count) / sum(hook_count)
  ) |>
  ggplot() +
  geom_point(aes(year, cpue_hk, colour = survey_abbrev)) +
  geom_line(aes(year, cpue_hk, colour = survey_abbrev))

d |>
  group_by(survey_abbrev, year) |>
  mutate(
    catch_count_sum = sum(catch_count),
    cpue = sum(catch_count) / sum(hook_count)
  ) |>
  ggplot() +
  geom_point(aes(year, catch_count_sum, colour = survey_abbrev)) +
  geom_line(aes(year, catch_count_sum, colour = survey_abbrev))


d |>
  ggplot() +
  geom_point(aes(longitude, latitude, colour = survey_abbrev), alpha = 0.5) +
  facet_wrap(~year) +
  theme_classic() +
  scale_color_manual(values = c("#e69b99", "#24492e", "#2c6184"))
ggsave("Figures/summary_surveylocations.png", width = 5, height = 5)

d |>
  ggplot() +
  geom_point(aes(longitude, latitude, colour = catch_count, size = catch_count, alpha = 0.5)) +
  facet_wrap(~year) +
  scale_colour_viridis_c(trans = "sqrt") +
  theme_classic()
ggsave("Figures/summary_surveylocationscatches.png", width = 5, height = 5)


d |>
  group_by(survey_abbrev, year) |>
  ggplot() +
  geom_jitter(aes(year, log(catch_count), colour = survey_abbrev), alpha = 0.5) +
  theme_classic() +
  scale_color_manual(values = c("#e69b99", "#24492e", "#2c6184"))
ggsave("Figures/summary_logcatches.png", width = 5, height = 5)


d |>
  group_by(survey_abbrev, year) |>
  ggplot() +
  geom_jitter(aes(year, hook_count, colour = survey_abbrev), alpha = 0.5) +
  theme_classic() +
  scale_color_manual(values = c("#e69b99", "#24492e", "#2c6184"))
ggsave("Figures/summary_hookscounts.png", width = 5, height = 5)


d |>
  group_by(survey_abbrev, year) |>
  ggplot() +
  geom_jitter(aes(year, julian, colour = survey_abbrev, alpha = 0.5)) +
  theme_classic() +
  scale_color_manual(values = c("#e69b99", "#24492e", "#2c6184"))
ggsave("Figures/summary_julian.png", width = 5, height = 5)


d |>
  group_by(survey_abbrev, year) |>
  ggplot() +
  geom_jitter(aes(year, soak, colour = survey_abbrev), alpha = 0.5) +
  theme_classic() +
  scale_color_manual(values = c("#e69b99", "#24492e", "#2c6184"))
ggsave("Figures/summary_soak.png", width = 5, height = 5)


d |>
  group_by(survey_abbrev, year) |>
  ggplot() +
  geom_jitter(aes(year, log(soak), colour = catch_count, size = catch_count)) +
  scale_colour_viridis_c()


d |>
  group_by(survey_abbrev, year) |>
  ggplot() +
  geom_jitter(aes(year, offset, colour = survey_abbrev))

d |>
  group_by(survey_abbrev, year) |>
  ggplot() +
  geom_jitter(aes(log_botdepth, log(catch_count), colour = survey_abbrev), alpha = 0.5) +
  facet_wrap(~year) +
  theme_classic() +
  scale_color_manual(values = c("#e69b99", "#24492e", "#2c6184"))
ggsave("Figures/summary_depth.png", width = 6, height = 5)

