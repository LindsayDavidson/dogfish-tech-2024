library(ggplot2)
library(sf)
library(tidyr)
library(gfplot)
sf::sf_use_s2(FALSE)

hbll_ins_s <- gfplot::hbll_inside_s_grid
hbll_ins_s$grid$area <- "hbll_s"
hbll_ins_n <- gfplot::hbll_inside_n_grid
hbll_ins_n$grid$area <- "hbll_n"
hbll_ins <- rbind(hbll_ins_n$grid, hbll_ins_s$grid)

hbll_ins_utm <- sdmTMB::add_utm_columns(hbll_ins,
                                        ll_names = c("X", "Y"),
                                        utm_names = c("UTM.lon", "UTM.lat"),
                                        utm_crs = 32609
) |>
  mutate(UTM.lon.m = UTM.lon * 1000, UTM.lat.m = UTM.lat * 1000)


grid_dog <- gfplot::dogfish_grid$grid %>%
  mutate(UTM.lon = X / 1e3, UTM.lat = Y / 1e3) |>
  distinct()


map_data <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")

hbllboth <- st_crop(
  map_data,
  c(xmin = -128, ymin = 48.4, xmax = -122.5, ymax = 51.5)
)

hbllboth <- st_transform(hbllboth, crs = 32609)

hbllgrid <-
  ggplot() +

  geom_sf(data = hbllboth, fill = "grey90", colour = "grey70") +

  theme_classic() +
  geom_point(
    data = hbll_ins_utm,
    aes(x = UTM.lon.m, y = UTM.lat.m),
    fill = "gray100", color = "grey50", alpha = 0.25,shape = 15, size = 1
  )  +

  geom_point(
    data = grid_dog,
    aes(x = X, y = Y),
    fill = NA, color = "red", alpha = 0.15,shape = 15, size = 0.1
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

hbllgrid

ggsave("figures/hbll_dog_grid.png",hbllgrid, width = 6, height = 4)
