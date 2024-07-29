#params
cols <- c("#e69b99", "#24492e", "#015b58",  "#2c6184","#89689d" )
cols <- c("#d7191c", "#fdae61", "#2c6184",  "#2c7bb6" )



# load data ---------------------------------------------------------------

d <- readRDS("data-raw/wrangled-hbll-dog-sets.rds") #no west coast VI expansion set
#d <- readRDS("data-raw/wrangled-hbll-dog-sets-hblls.rds") #no expansion set, no hbll north execpt for the 2008 year, note 2004 got dropped when we dropped NAs in soak time


# load map ----------------------------------------------------------------
library(sf)
sf::sf_use_s2(FALSE)
map_data <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")
bc_coast <- st_crop(
  map_data,
  c(xmin = -130, ymin = 48.5, xmax = -122, ymax = 51.5)
)

# summary plots -----------------------------------------------------------

ggplot() +
  geom_point(data = d, aes(longitude, latitude, colour = survey2)) +
  theme_classic() +
  geom_sf(data = bc_coast, fill = "grey90", colour = "grey70") +    facet_wrap(~survey2) +
  guides(colour=guide_legend(title="Survey"))

ggplot() +
  geom_point(data = d, aes(longitude, latitude, colour = survey_abbrev)) +
  theme_classic() +
  geom_sf(data = bc_coast, fill = "grey90", colour = "grey70") +    facet_wrap(~survey_abbrev) +
  guides(colour=guide_legend(title="Survey"))
ggsave("Figures/summary_locations.png", width = 5, height = 4)


d |>
  filter(survey_abbrev == "HBLL INS S") |>
  ggplot() +
  geom_point(aes(longitude, latitude, colour = catch_count), size = 1) +
  theme_classic() +
  geom_sf(data = bc_coast, fill = "grey90", colour = "grey70") +    facet_wrap(~year) +
  guides(colour=guide_legend(title="Catch count")) +
  scale_colour_viridis_c() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
ggsave("Figures/summary_locations_hbllinss.png", width = 5, height = 4)


d |>
  filter(survey_abbrev == "HBLL INS N") |>
  ggplot() +
  geom_point(aes(longitude, latitude, colour = catch_count), size = 1) +
  theme_classic() +
  geom_sf(data = bc_coast, fill = "grey90", colour = "grey70") +    facet_wrap(~year) +
  guides(colour=guide_legend(title="Catch count")) +
  scale_colour_viridis_c() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
ggsave("Figures/summary_locations_hbllinsn.png", width = 5, height = 4)


d |>
  filter(survey_abbrev == "dog") |>
  ggplot() +
  geom_point(aes(longitude, latitude, colour = catch_count), size = 1) +
  theme_classic() +
  geom_sf(data = bc_coast, fill = "grey90", colour = "grey70") +    facet_wrap(~year) +
  guides(colour=guide_legend(title="Catch count")) +
  scale_colour_viridis_c() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
ggsave("Figures/summary_locations_dog.png", width = 5, height = 4)

d |>
  filter(survey_abbrev == "dog-jhook") |>
  ggplot() +
  geom_point(aes(longitude, latitude, colour = catch_count), size = 1) +
  theme_classic() +
  geom_sf(data = bc_coast, fill = "grey90", colour = "grey70") +    facet_wrap(~year) +
  guides(colour=guide_legend(title="Catch count")) +
  scale_colour_viridis_c() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
ggsave("Figures/summary_locations_dogjhook.png", width = 5, height = 4)



d |>
  filter(survey2 != "hbll comp") |>
  filter(survey2 != "dog comp") |>
  group_by(survey_abbrev, year) |>
  mutate(
    catch_count_sum = sum(catch_count),
    cpue = sum(catch_count) / sum(exp(offset)),
    cpue_hk = sum(catch_count) / sum(hook_count)
  ) |>
  ggplot() +
  geom_point(aes(year, cpue, colour = survey_abbrev, size = 1)) +
  geom_line(aes(year, cpue, colour = survey_abbrev), size = 1) + theme_classic() +
  #scale_color_manual(values = cols) +
  guides(colour=guide_legend(title="Survey")) +
  scale_colour_viridis_d()
ggsave("Figures/summary_cpuetrends.png", width = 5, height = 4)

d |>
  group_by(survey2, year) |>
  mutate(
    catch_count_sum = sum(catch_count),
    cpue = sum(catch_count) / sum(exp(offset)),
    cpue_hk = sum(catch_count) / sum(hook_count)
  ) |>
  ggplot() +
  geom_point(aes(year, cpue, colour = survey3, size = 1)) +
  geom_line(aes(year, cpue, colour = survey3), size = 1) + theme_classic() +
  #scale_color_manual(values = cols) +
  guides(colour=guide_legend(title="Survey")) +
  scale_colour_viridis_d()

d |>
  filter(survey2 != "hbll comp") |>
  filter(survey2 != "dog comp") |>
  #filter(survey_abbrev == "hbll") |>
  group_by(survey_abbrev, year) |>
  ggplot() +
  geom_jitter(aes(year, julian, colour = catch_count, size = catch_count, alpha = 0.5))  +
  theme_classic() + facet_wrap(~survey_abbrev) + scale_colour_viridis_c()
  #geom_line(aes(year, catch_count, colour = survey_abbrev))

d |>
  ggplot() +
  geom_point(aes(longitude, latitude, colour = survey2)) +
  facet_wrap(~year + survey2) +
  theme_classic() +
  #scale_color_manual(values = cols) +
  guides(colour=guide_legend(title="Survey")) +
  geom_sf(data = bc_coast, fill = "grey90", colour = "grey70")
ggsave("Figures/summary_surveylocations.png", width = 10, height = 10)

d |>
  ggplot() +
  geom_point(aes(longitude, latitude, colour = catch_count, fill = catch_count), alpha = 0.5, size = 0.5) +
  facet_wrap(~year + survey2) +
  scale_colour_viridis_c(trans = "sqrt") +
  scale_fill_viridis_c(trans = "sqrt") +
  theme_classic() +
  geom_sf(data = bc_coast, fill = "grey90", colour = "grey70")
ggsave("Figures/summary_surveylocationscatches.png", width = 10, height = 10)

d |>
  ggplot() +
  geom_point(aes(longitude, latitude, colour = catch_count, fill = catch_count, size = catch_count), alpha = 0.5) +
  facet_wrap(~year + survey2) +
  scale_colour_viridis_c(trans = "sqrt") +
  scale_fill_viridis_c(trans = "sqrt") +
  theme_classic()
  #geom_sf(data = bc_coast, fill = "grey90", colour = "grey70")
ggsave("Figures/summary_surveylocationscatches_nomap.png", width = 10, height = 10)

d |>
  ggplot() +
  geom_point(aes(longitude, latitude, colour = catch_count, size = catch_count, alpha = 0.5)) +
  facet_wrap(~year) +
  scale_colour_viridis_c(trans = "sqrt") +
  theme_classic() +
  geom_sf(data = bc_coast, fill = "grey90", colour = "grey70")
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
  #scale_color_manual(values = cols) +
  facet_wrap(~survey_abbrev)
ggsave("Figures/summary_catches.png", width = 6, height = 5)


d |>
  group_by(survey_abbrev, year) |>
  ggplot() +
  geom_jitter(aes(year, hook_count, colour = survey_abbrev), alpha = 0.5) +
  theme_classic()
  #scale_color_manual(values = cols)
ggsave("Figures/summary_hookscounts.png", width = 5, height = 4)


d |>
  group_by(survey2,survey_abbrev, year) |>
  ggplot() +
  geom_jitter(aes(year, julian, colour = survey2), alpha = 0.5) +
  theme_classic() +
  guides(size="none") +
  #scale_color_manual(values = cols) +
  guides(colour=guide_legend(title="Survey"))
  #facet_wrap(~survey_abbrev, scales = "free")
ggsave("Figures/summary_julian.png", width = 5, height = 4)


d |>
  group_by(survey_abbrev, year) |>
  ggplot() +
  geom_jitter(aes(year, soak, colour = survey_abbrev), alpha = 0.5) +
  theme_classic()
  #scale_color_manual(values = cols)
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
  filter(survey_abbrev == "dog") |>
  group_by(survey_abbrev, year) |>
  ggplot() +
  geom_jitter(aes(log_botdepth,log(catch_count), colour = survey_abbrev), alpha = 0.5) +
  facet_wrap(~year) +
  theme_classic()
  #scale_color_manual(values = cols)
ggsave("Figures/summary_depth.png", width = 6, height = 5)

d |>
  filter(survey2 == "hbll") |>
  group_by(survey_abbrev, year) |>
  ggplot() +
  geom_jitter(aes(log_botdepth,log(catch_count), colour = survey_abbrev), alpha = 0.5) +
  facet_wrap(~year) +
  theme_classic()
  #scale_color_manual(values = cols)

