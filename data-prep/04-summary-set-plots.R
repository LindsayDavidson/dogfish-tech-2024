#params
cols <- c("#e69b99", "#24492e", "#015b58",  "#2c6184","#89689d" )



# pull data ---------------------------------------------------------------

d <- readRDS("data-raw/wrangled-hbll-dog-sets.rds") #no west coast VI expansion set
#d <- readRDS("data-raw/wrangled-hbll-dog-sets-hblls.rds") #no expansion set, no hbll north execpt for the 2008 year

# summary plots -----------------------------------------------------------

d |>
  group_by(survey_abbrev, year) |>
  mutate(
    catch_count_sum = sum(catch_count),
    cpue = sum(catch_count) / sum(exp(offset)),
    cpue_hk = sum(catch_count) / sum(hook_count)
  ) |>
  ggplot() +
  geom_point(aes(year, cpue, colour = survey_abbrev)) +
  geom_line(aes(year, cpue, colour = survey_abbrev)) + theme_classic() +
  scale_color_manual(values = cols)
ggsave("Figures/summary_cpuetrends.png", width = 6, height = 5)


d |>
  #filter(survey_abbrev == "hbll") |>
  group_by(survey_abbrev, year) |>
  ggplot() +
  geom_jitter(aes(year, julian, colour = catch_count))  +
  theme_classic() + facet_wrap(~survey_abbrev) + scale_colour_viridis_c()
  #geom_line(aes(year, catch_count, colour = survey_abbrev))

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
  facet_wrap(~year + survey_abbrev) +
  theme_classic() +
  scale_color_manual(values = cols)
ggsave("Figures/summary_surveylocations.png", width = 10, height = 10)

d |>
  ggplot() +
  geom_point(aes(longitude, latitude, colour = catch_count, size = catch_count, alpha = 0.5)) +
  facet_wrap(~year + survey_abbrev) +
  scale_colour_viridis_c(trans = "sqrt") +
  theme_classic()
ggsave("Figures/summary_surveylocationscatches.png", width = 10, height = 10)

d |>
  ggplot() +
  geom_point(aes(longitude, latitude, colour = catch_count, size = catch_count, alpha = 0.5)) +
  facet_wrap(~year) +
  scale_colour_viridis_c(trans = "sqrt") +
  theme_classic()
ggsave("Figures/summary_surveylocationscatches_yronly.png", width = 8, height = 6)

test <- filter(d, year == 2023 & survey_abbrev %in% c("hbll", "HBLL INS S"))
test <- test |> mutate(time = ifelse(julian > 265, "late", "early"))
test2 <- filter(d, year == 2021 & survey_abbrev == "HBLL INS S")
test2$time <- "early"
test3 <- bind_rows(test, test2)
test3 |>
  ggplot() +
  geom_jitter(aes(year, (catch_count), colour = time), alpha = 0.5) +
  theme_classic()


d |>
  group_by(survey_abbrev, year) |>
  ggplot() +
  geom_jitter(aes(year, log(catch_count), colour = survey_abbrev), alpha = 0.5) +
  theme_classic() +
  scale_color_manual(values = cols) + facet_wrap(~survey_abbrev)
ggsave("Figures/summary_catches.png", width = 6, height = 5)


d |>
  group_by(survey_abbrev, year) |>
  ggplot() +
  geom_jitter(aes(year, hook_count, colour = survey_abbrev), alpha = 0.5) +
  theme_classic() +
  scale_color_manual(values = cols)
ggsave("Figures/summary_hookscounts.png", width = 5, height = 5)


d |>
  group_by(survey_abbrev, year) |>
  ggplot() +
  geom_jitter(aes(year, julian, colour = survey_abbrev), alpha = 0.5) +
  theme_classic() +
  guides(colour="none") +
  scale_color_manual(values = cols) + facet_wrap(~survey_abbrev)
ggsave("Figures/summary_julian.png", width = 5, height = 5)


d |>
  group_by(survey_abbrev, year) |>
  ggplot() +
  geom_jitter(aes(year, soak, colour = survey_abbrev), alpha = 0.5) +
  theme_classic() +
  scale_color_manual(values = cols)
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
  scale_color_manual(values = cols)
ggsave("Figures/summary_depth.png", width = 6, height = 5)

