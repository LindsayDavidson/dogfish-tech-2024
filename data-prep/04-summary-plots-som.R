library(dplyr)
library(ggplot2)
library(sf)
library(tidyr)
remotes::install_github("pbs-assess/gfdata", ref = "trials", force = TRUE)
remotes::install_github("pbs-assess/gfplot")
library(gfdata)
library(gfplot)
sf::sf_use_s2(FALSE)

hbll_ins_s <- gfplot::hbll_inside_s_grid
hbll_ins_s$grid$area <- "hbll_s"
hbll_ins_n <- gfplot::hbll_inside_n_grid
hbll_ins_n$grid$area <- "hbll_n"
hbll_ins <- rbind(hbll_ins_n$grid, hbll_ins_s$grid)

ggplot() +
  geom_point(
    data = hbll_ins_s$grid,
    aes(x = X, y = Y),
    fill = "gray90", color = "black", shape = 15, size = 1
  )


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
  c(xmin = -130, ymin = 48.5, xmax = -123, ymax = 51)
)

# summary plots -----------------------------------------------------------

d$survey_sep <- factor(d$survey_sep, levels = c("HBLL INS N", "HBLL INS S", "hbll comp", "dog comp", "dog-jhook", "dog"), labels = c("HBLL N", "HBLL S", "HBLL comp", "DOG comp", "DOG J-hook", "DOG"))

d$survey_abbrev <- factor(d$survey_abbrev, levels = c("HBLL INS N", "HBLL INS S", "OTHER", "DOG"), labels = c("HBLL N", "HBLL S", "OTHER", "DOG"))

d <- d |>
  group_by(year) |>
  mutate(id = seq(1, n(), 1))

hblls <- st_crop(
  map_data,
  c(xmin = -126, ymin = 48.4, xmax = -122.5, ymax = 50.5)
)

hblln <- st_crop(
  map_data,
  c(xmin = -128, ymin = 49, xmax = -123.6, ymax = 51.5)
)


a <- d |>
  filter(survey_sep != "DOG comp") |>
  filter(survey_sep != "HBLL comp") |>
  filter(survey_sep != "DOG J-hook") |>

  filter(survey_sep %in% c("HBLL N")) |>

  ggplot() +

  geom_sf(data = hblln, fill = "grey90", colour = "grey70") +

  geom_point(aes(longitude, latitude, colour = catch_count), shape = 15) +
  theme_classic() +
  geom_point(
    data = hbll_ins_n$grid,
    aes(x = X, y = Y),
    fill = "gray90", color = "grey50", alpha = 0.5,shape = 15, size = 1
  )  +
  scale_colour_viridis_c(option = "magma", direction = -1, guide = NULL) +
  labs(y = "Latitude", x = "Longitude") +

  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.line = element_blank(),
    axis.text = element_text(size = 10),
    panel.border = element_rect(colour = "black", fill=NA, size=0.5)
  )

b <- d |>
  filter(survey_sep != "DOG comp") |>
  filter(survey_sep != "HBLL comp") |>
  filter(survey_sep != "DOG J-hook") |>

  filter(survey_sep %in% c("HBLL S")) |>

  ggplot() +

  geom_sf(data = hblls, fill = "grey90", colour = "grey70") +

  geom_point(aes(longitude, latitude, colour = catch_count), shape = 15) +
  theme_classic() +
  geom_point(
    data = hbll_ins_s$grid,
    aes(x = X, y = Y),
    fill = "gray90", color = "grey50", alpha = 0.5,shape = 15, size = 1
  )  +
  scale_colour_viridis_c(option = "magma", direction = -1, guide = NULL) +
  labs(y = "Latitude", x = "Longitude") +

  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.line = element_blank(),
    axis.text = element_text(size = 10),
    panel.border = element_rect(colour = "black", fill=NA, size=0.5)
  )



cv <- cowplot::plot_grid(
  a, b,
  ncol = 2,
  nrow = 1,
  labels = c("(a)", "(b)"), # Labels for each plot
  # align = "hv",
  rel_heights = c(1, 1),
  rel_widths = rep(1)
)

cv
ggsave("figures/summary_locations_hbllsn.png", width = 9, height = 4)


d |>
  filter(survey_abbrev %in% c("HBLL S","HBLL N")) |>
  ggplot() +
  geom_point(aes(longitude, latitude, colour = catch_count), size = 1, alpha = 0.5, shape = 15) +
  theme_classic() +
  geom_sf(data = bc_coast, fill = "grey90", colour = "grey70") +
  facet_wrap(~year, ncol = 4) +
  guides(colour = guide_legend(title = "Catch count")) +
  scale_colour_viridis_c() +
  labs(y = "Latitude", x = "Longitude") +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 8),
    strip.text = element_text(size = 8)
  )
ggsave("Figures/summary_locations_hbllinss.png", width = 5, height = 6)


x <- d |>
  filter(survey_lumped == "dog-jhook")
y <- d |>
  filter(survey_sep %in% c("DOG", "DOG comp"))
dd <- rbind(x, y)

dd |>
  #filter(survey_sep %in% c("DOG", "DOG comp")) |>
  filter(year %in% c(1989, 1986, 2005, 2008, 2011, 2014, 2019, 2023, 2004)) |>
  ggplot() +
  geom_point(aes(longitude, latitude, colour = catch_count), size = 1) +
  theme_classic() +
  geom_sf(data = hblls, fill = "grey90", colour = "grey70") +
  facet_wrap(~year, ncol = 2) +
  guides(colour = guide_legend(title = "Catch count")) +
  scale_colour_viridis_c() +
  labs(y = "Latitude", x = "Longitude") +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 8),
    strip.text = element_text(size = 8)
  )
ggsave("Figures/summary_locations_dog.png", width = 6, height = 6)



d |>
  filter(survey_sep != "hbll comp") |>
  filter(survey_sep != "dog comp") |>
  filter(survey_abbrev != "OTHER") |>
  # filter(survey_abbrev == "hbll") |>
  group_by(survey_abbrev, year) |>
  ggplot() +
  geom_jitter(aes(year, julian, colour = catch_count, size = catch_count), alpha = 0.25) +
  theme_classic() +
  facet_wrap(~survey_abbrev) +
  scale_colour_viridis_c() +
  labs(y = "Julian day", x = "Year") +
  guides(size = "none") +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12)
  ) +
  labs(colour = "Catch count")
ggsave("Figures/summary_julian.png", width = 9, height = 3)



