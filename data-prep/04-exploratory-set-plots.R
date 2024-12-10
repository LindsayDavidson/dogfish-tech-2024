#expoloratory plots, nothing for the manuscript

library(dplyr)
library(ggplot2)
library(sf)
library(tidyr)
sf::sf_use_s2(FALSE)

# params
cols <- c("#e69b99", "#24492e", "#015b58", "#2c6184", "#89689d")
cols <- c("#d7191c", "#fdae61", "#2c6184", "#2c7bb6")



# load data ---------------------------------------------------------------

d <- readRDS("data-raw/wrangled-hbll-dog-sets.rds") # no west coast VI expansion set
# d <- readRDS("data-raw/wrangled-hbll-dog-sets-hblls.rds") #no expansion set, no hbll north except for the 2008 year, note 2004 got dropped when we dropped NAs in soak time
d <- filter(d, soak >= 0)
d <- filter(d, is.na(soak) != TRUE) # get rid of 2004 that has no soak time

# load map ----------------------------------------------------------------
map_data <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")
bc_coast <- st_crop(
  map_data,
  c(xmin = -130, ymin = 48.5, xmax = -122, ymax = 51.5)
)

# summary plots -----------------------------------------------------------

d$depth_m
d <- d |>
  group_by(year) |>
  mutate(id = seq(1, n(), 1))

d |> #<- variability in depth for hbll surveys? no
  drop_na(depth_m) |>
  # filter(year == 2021) |>
  filter(survey_abbrev %in% c("HBLL INS N", "HBLL INS S")) |>
  ggplot(aes((id), depth_m, ymin = depth_begin, ymax = depth_end, group = year)) +
  geom_pointrange(aes(x = (id) - 0.25), size = 0.2, pch = 5, alpha = 0.6) +
  facet_wrap(~year, scales = "free_x")

d |> #design based cpue trends acros survey types
  group_by(year, survey_lumped) |>
  drop_na(catch_count) |>
  drop_na(offset) |>
  summarise(catch = sum(catch_count), effort = sum(offset), na.omit = TRUE) |>
  mutate(cpue = catch / effort) |>
  ggplot() +
  geom_point(aes(year, cpue, group = survey_lumped, colour = survey_lumped)) +
  geom_line(aes(year, cpue, group = survey_lumped, colour = survey_lumped))

d |>
  group_by(year, survey_abbrev) |>
  drop_na(catch_count) |>
  drop_na(offset) |>
  summarise(catch = sum(catch_count), effort = sum(offset), na.omit = TRUE) |>
  mutate(cpue = catch / effort) |>
  ggplot() +
  geom_point(aes(year, cpue, group = survey_abbrev, colour = survey_abbrev)) +
  geom_line(aes(year, cpue, group = survey_abbrev, colour = survey_abbrev))

ggplot() +
  geom_point(data = d, aes(longitude, latitude, colour = survey_lumped)) +
  theme_classic() +
  geom_sf(data = bc_coast, fill = "grey90", colour = "grey70") +
  facet_wrap(~survey_lumped) +
  guides(colour = guide_legend(title = "Survey"))

ggplot() +
  geom_point(data = d, aes(longitude, latitude, colour = survey_sep)) +
  theme_classic() +
  geom_sf(data = bc_coast, fill = "grey90", colour = "grey70") +
  facet_wrap(~survey_lumped) +
  guides(colour = guide_legend(title = "Survey"))

d |>
  group_by(survey_lumped, year) |>
  distinct() |>
  reframe() |>
  print(n = 40) # looks good

d |>
  filter(survey_sep == "HBLL INS S") |>
  ggplot() +
  geom_point(aes(longitude, latitude, colour = catch_count), size = 1) +
  theme_classic() +
  geom_sf(data = bc_coast, fill = "grey90", colour = "grey70") +
  facet_wrap(~year) +
  guides(colour = guide_legend(title = "Catch count")) +
  scale_colour_viridis_c() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5))

d |>
  #filter(survey_sep == "HBLL INS N") |>
  filter(survey_abbrev == "HBLL INS N") |>
  ggplot() +
  geom_point(aes(longitude, latitude, colour = catch_count), size = 1) +
  theme_classic() +
  geom_sf(data = bc_coast, fill = "grey90", colour = "grey70") +
  facet_wrap(~year) +
  guides(colour = guide_legend(title = "Catch count")) +
  scale_colour_viridis_c() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5))

d |>
  filter(survey_sep %in% c("dog")) |>
  ggplot() +
  geom_point(aes(longitude, latitude, colour = catch_count), size = 1) +
  theme_classic() +
  geom_sf(data = bc_coast, fill = "grey90", colour = "grey70") +
  facet_wrap(~year) +
  guides(colour = guide_legend(title = "Catch count")) +
  scale_colour_viridis_c() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5))

d |>
  filter(survey_lumped == "dog-jhook") |>
  ggplot() +
  geom_point(aes(longitude, latitude, colour = catch_count), size = 1) +
  theme_classic() +
  geom_sf(data = bc_coast, fill = "grey90", colour = "grey70") +
  facet_wrap(~year) +
  guides(colour = guide_legend(title = "Catch count")) +
  scale_colour_viridis_c() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5))


d |>
  filter(survey_sep != "hbll comp") |>
  filter(survey_sep != "dog comp") |>
  group_by(survey_abbrev, year) |>
  drop_na(offset) |>
  drop_na(catch_count) |>
  mutate(
    catch_count_sum = sum(catch_count),
    cpue = sum(catch_count) / sum(exp(offset)),
    cpue_hk = sum(catch_count) / sum(lglsp_hook_count)
  ) |>
  ggplot() +
  geom_point(aes(year, cpue, colour = survey_sep, size = 1)) +
  geom_line(aes(year, cpue, colour = survey_sep), size = 1) +
  theme_classic() +
  # scale_color_manual(values = cols) +
  guides(colour = guide_legend(title = "Survey")) +
  scale_colour_viridis_d()

d |>
  filter(survey_sep != "hbll comp") |>
  filter(survey_sep != "dog comp") |>
  group_by(survey_sep, year) |>
  drop_na(offset) |>
  drop_na(catch_count) |>
  mutate(
    catch_count_sum = sum(catch_count),
    cpue = sum(catch_count) / sum(exp(offset)),
    cpue_hk = sum(catch_count) / sum(lglsp_hook_count)
  ) |>
  ggplot() +
  geom_point(aes(year, cpue, colour = survey_sep, size = 1)) +
  geom_line(aes(year, cpue, colour = survey_sep), size = 1) +
  theme_classic() +
  # scale_color_manual(values = cols) +
  guides(colour = guide_legend(title = "Survey")) +
  scale_colour_viridis_d()

d |>
  filter(survey_sep != "hbll comp") |>
  filter(survey_sep != "dog comp") |>
  filter(survey_abbrev != "OTHER") |>
  # filter(survey_abbrev == "hbll") |>
  group_by(survey_abbrev, year) |>
  ggplot() +
  geom_jitter(aes(year, julian, colour = catch_count, size = catch_count, alpha = 0.5)) +
  theme_classic() +
  facet_wrap(~survey_abbrev, ) +
  scale_colour_viridis_c()

d |>
  ggplot() +
  geom_point(aes(longitude, latitude, colour = survey_sep)) +
  facet_wrap(~ year + survey_sep) +
  theme_classic() +
  # scale_color_manual(values = cols) +
  guides(colour = guide_legend(title = "Survey")) +
  geom_sf(data = bc_coast, fill = "grey90", colour = "grey70")

d |>
  ggplot() +
  geom_point(aes(longitude, latitude, colour = catch_count, fill = catch_count), alpha = 0.5, size = 0.5) +
  facet_wrap(~ year + survey_lumped) +
  scale_colour_viridis_c(trans = "sqrt") +
  scale_fill_viridis_c(trans = "sqrt") +
  theme_classic() +
  geom_sf(data = bc_coast, fill = "grey90", colour = "grey70")

d |>
  ggplot() +
  geom_point(aes(longitude, latitude, colour = catch_count, fill = catch_count, size = catch_count), alpha = 0.5) +
  facet_wrap(~ year + survey_lumped) +
  scale_colour_viridis_c(trans = "sqrt") +
  scale_fill_viridis_c(trans = "sqrt") +
  theme_classic()

d |>
  ggplot() +
  geom_point(aes(longitude, latitude, colour = catch_count, size = catch_count, alpha = 0.5)) +
  facet_wrap(~year) +
  scale_colour_viridis_c(trans = "sqrt") +
  theme_classic() +
  geom_sf(data = bc_coast, fill = "grey90", colour = "grey70")

d |>
  group_by(survey_abbrev, year) |>
  ggplot() +
  geom_jitter(aes(year, log(catch_count), colour = survey_abbrev), alpha = 0.5) +
  theme_classic() +
  # scale_color_manual(values = cols) +
  facet_wrap(~survey_abbrev)


d |>
  group_by(survey_abbrev, year) |>
  ggplot() +
  geom_jitter(aes(year, lglsp_hook_count, colour = survey_abbrev), alpha = 0.5) +
  theme_classic()


d |>
  group_by(survey_sep, survey_abbrev, year) |>
  ggplot() +
  geom_jitter(aes(year, julian, colour = survey_sep), alpha = 0.5) +
  theme_classic() +
  guides(size = "none") +
  # scale_color_manual(values = cols) +
  guides(colour = guide_legend(title = "Survey"))


d |>
  group_by(survey_abbrev, year) |>
  ggplot() +
  geom_jitter(aes(year, soak, colour = survey_abbrev), alpha = 0.5) +
  theme_classic()


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
  filter(survey_abbrev == "dog") |>
  group_by(survey_abbrev, year) |>
  ggplot() +
  geom_jitter(aes(log_botdepth, log(catch_count), colour = survey_abbrev), alpha = 0.5) +
  facet_wrap(~year) +
  theme_classic()

