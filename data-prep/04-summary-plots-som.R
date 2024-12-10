library(dplyr)
library(ggplot2)
library(sf)
library(tidyr)
sf::sf_use_s2(FALSE)

# params
cols <- c("#e69b99", "#24492e", "#015b58", "#2c6184", "#89689d")
cols <- c("#d7191c", "#fdae61", "#2c6184", "#2c7bb6")



d <- readRDS("data-raw/wrangled-hbll-dog-sets.rds") # no west coast VI expansion set
# d <- readRDS("data-raw/wrangled-hbll-dog-sets-hblls.rds") #no expansion set, no hbll north except for the 2008 year, note 2004 got dropped when we dropped NAs in soak time
d <- filter(d, soak >= 0)
d <- filter(d, is.na(soak) != TRUE) # get rid of 2004 that has no soak time

map_data <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")
bc_coast <- st_crop(
  map_data,
  c(xmin = -130, ymin = 48.5, xmax = -122, ymax = 51.5)
)

# summary plots -----------------------------------------------------------

d$survey_sep <- factor(d$survey_sep, levels = c("HBLL INS N", "HBLL INS S", "hbll comp", "dog comp", "dog-jhook", "dog"), labels = c("HBLL N", "HBLL S", "HBLL comp", "DOG comp", "DOG J-hook", "DOG"))

d$survey_abbrev <- factor(d$survey_abbrev, levels = c("HBLL INS N", "HBLL INS S", "OTHER", "DOG"), labels = c("HBLL N", "HBLL S", "OTHER", "DOG"))

d <- d |>
  group_by(year) |>
  mutate(id = seq(1, n(), 1))

d |> #<- bigger axes labels
  filter(survey_sep != "DOG comp") |>
  filter(survey_sep != "HBLL comp") |>
  filter(survey_sep != "DOG J-hook") |>
  ggplot() +
  geom_point(aes(longitude, latitude), colour = "grey20") +
  theme_classic() +
  geom_sf(data = bc_coast, fill = "grey90", colour = "grey70") +
  facet_grid(rows = vars(survey_sep)) +
  guides(colour = guide_legend(title = "Survey")) +
  scale_colour_viridis_d() +
  guides(fill = "none", colour = "none") +
  labs(y = "Latitude", x = "Longitude") +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12)
  )
ggsave("Figures/summary_locations.png", width = 15, height = 8)


d |>
  filter(survey_abbrev == "HBLL INS S") |>
  ggplot() +
  geom_point(aes(longitude, latitude, colour = catch_count), size = 1) +
  theme_classic() +
  geom_sf(data = bc_coast, fill = "grey90", colour = "grey70") +
  facet_wrap(~year) +
  guides(colour = guide_legend(title = "Catch count")) +
  scale_colour_viridis_c() +
  labs(y = "Latitude", x = "Longitude") +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12)
  )
ggsave("Figures/summary_locations_hbllinss.png", width = 10, height = 10)


d |>
  filter(survey_abbrev == "HBLL INS N") |>
  ggplot() +
  geom_point(aes(longitude, latitude, colour = catch_count), size = 1) +
  theme_classic() +
  geom_sf(data = bc_coast, fill = "grey90", colour = "grey70") +
  facet_wrap(~year) +
  guides(colour = guide_legend(title = "Catch count")) +
  scale_colour_viridis_c() +
  labs(y = "Latitude", x = "Longitude") +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12)
  )
ggsave("Figures/summary_locations_hbllinsn_raw.png", width = 10, height = 10)


d |>
  filter(survey_sep %in% c("DOG")) |>
  ggplot() +
  geom_point(aes(longitude, latitude, colour = catch_count), size = 1) +
  theme_classic() +
  geom_sf(data = bc_coast, fill = "grey90", colour = "grey70") +
  facet_wrap(~year, ncol = 2) +
  guides(colour = guide_legend(title = "Catch count")) +
  scale_colour_viridis_c() +
  labs(y = "Latitude", x = "Longitude") +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12)
  )
ggsave("Figures/summary_locations_dog.png", width = 10, height = 8)

d |>
  filter(survey_lumped == "dog-jhook") |>
  ggplot() +
  geom_point(aes(longitude, latitude, colour = catch_count), size = 1) +
  theme_classic() +
  geom_sf(data = bc_coast, fill = "grey90", colour = "grey70") +
  facet_wrap(~year) +
  guides(colour = guide_legend(title = "Catch count")) +
  scale_colour_viridis_c() +
  labs(y = "Latitude", x = "Longitude") +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12)
  )
ggsave("Figures/summary_locations_dogjhook.png", width = 10, height = 5)


# d |>
#   filter(survey_sep != "hbll comp") |>
#   filter(survey_sep != "dog comp") |>
#   group_by(survey_sep, year) |>
#   drop_na(offset) |>
#   drop_na(catch_count) |>
#   mutate(
#     catch_count_sum = sum(catch_count),
#     cpue = sum(catch_count) / sum(exp(offset)),
#     cpue_hk = sum(catch_count) / sum(lglsp_hook_count)
#   ) |>
#   ggplot() +
#   geom_point(aes(year, cpue, colour = survey_sep, size = 1)) +
#   geom_line(aes(year, cpue, colour = survey_sep), size = 1) +
#   theme_classic() +
#   # scale_color_manual(values = cols) +
#   guides(colour = guide_legend(title = "Survey")) +
#   scale_colour_viridis_d()
# ggsave("Figures/summary_cpuetrends.png", width = 5, height = 4)

d |>
  filter(survey_sep != "hbll comp") |>
  filter(survey_sep != "dog comp") |>
  filter(survey_abbrev != "OTHER") |>
  # filter(survey_abbrev == "hbll") |>
  group_by(survey_abbrev, year) |>
  ggplot() +
  geom_jitter(aes(year, julian, colour = catch_count, size = catch_count), alpha = 0.5) +
  theme_classic() +
  facet_wrap(~survey_abbrev) +
  scale_colour_viridis_c() +
  labs(y = "Julian day", x = "Year") +
  guides(size = "none", ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12)
  )
ggsave("Figures/summary_julian.png", width = 10, height = 4)

d |>
  ggplot() +
  geom_point(aes(longitude, latitude, colour = survey_sep)) +
  facet_wrap(~ year + survey_sep) +
  theme_classic() +
  # scale_color_manual(values = cols) +
  guides(colour = guide_legend(title = "Survey")) +
  geom_sf(data = bc_coast, fill = "grey90", colour = "grey70")
# ggsave("Figures/summary_surveylocations.png", width = 10, height = 10)

d |>
  ggplot() +
  geom_point(aes(longitude, latitude, colour = catch_count, fill = catch_count), alpha = 0.5, size = 0.5) +
  facet_wrap(~ year + survey_lumped) +
  scale_colour_viridis_c(trans = "sqrt") +
  scale_fill_viridis_c(trans = "sqrt") +
  theme_classic() +
  geom_sf(data = bc_coast, fill = "grey90", colour = "grey70")
# ggsave("Figures/summary_surveylocationscatches.png", width = 10, height = 10)

d |>
  ggplot() +
  geom_point(aes(longitude, latitude, colour = catch_count, fill = catch_count, size = catch_count), alpha = 0.5) +
  facet_wrap(~ year + survey_lumped) +
  scale_colour_viridis_c(trans = "sqrt") +
  scale_fill_viridis_c(trans = "sqrt") +
  theme_classic()
# geom_sf(data = bc_coast, fill = "grey90", colour = "grey70")
# ggsave("Figures/summary_surveylocationscatches_nomap.png", width = 10, height = 10)

d |>
  ggplot() +
  geom_point(aes(longitude, latitude, colour = catch_count, size = catch_count, alpha = 0.5)) +
  facet_wrap(~year) +
  scale_colour_viridis_c(trans = "sqrt") +
  theme_classic() +
  geom_sf(data = bc_coast, fill = "grey90", colour = "grey70")
# ggsave("Figures/summary_surveylocationscatches_yronly.png", width = 8, height = 6)

d |>
  filter(survey_sep != "hbll comp") |>
  filter(survey_sep != "dog comp") |>
  filter(survey_abbrev != "OTHER") |>
  group_by(survey_sep, survey_abbrev, year) |>
  ggplot() +
  geom_jitter(aes(year, julian, colour = survey_sep), alpha = 0.5) +
  theme_classic() +
  guides(size = "none") +
  scale_colour_viridis_d() +
  # scale_color_manual(values = cols) +
  guides(colour = guide_legend(title = "Survey")) +
  labs(y = "Julian day", x = "Year") +
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12)
  )
ggsave("Figures/summary_julian_oneplot.png", width = 5, height = 4)
