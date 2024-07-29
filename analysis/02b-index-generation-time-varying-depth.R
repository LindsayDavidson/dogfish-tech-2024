library(sdmTMB)
library(tidyverse)


# data ---------------------------------------------------------------
# # all without 2004 comp work ----------------------------------------------
#
# d <- readRDS("data-raw/wrangled-hbll-dog-sets.rds") |>
#   filter(year != 2004)
# range(d$depth_m)
# d$log_botdepth2 <- d$log_botdepth * d$log_botdepth
# str(d$month)
# grid <- grid <- readRDS(
#   #"output/prediction-grid-hbll-n-s-dog-1-km.rds")
#   "output/prediction-grid-hbll-n-s-dog-2-km.rds")# from convex hull to have deeper depths
# grid$log_botdepth2 <- grid$log_botdepth * grid$log_botdepth
# grid$area_km2 <- as.numeric(grid$area_km)
# years <- seq(min(d$year), 2023, 1)
# grid <- purrr::map_dfr(years, ~ tibble(grid, year = .x))
# grid$survey2 <- "hbll"
# grid$julian <- mean(d$julian)
# grid$month <- 7
# path <- "output/fit-tv-sog-hblldog_no2004.rds"
# pathind <- "output/ind-tv-sog-hblldog_no2004.rds"
# sort(unique(d$year))
# extratime <- c(1987, 1988, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2004, 2006, 2017, 2020)
#
# plot(grid$longitude, grid$latitude)
# points(d$longitude, d$latitude, col = "red")
#

# hbll n and s only -------------------------------------------------------------

d <- readRDS("data-raw/wrangled-hbll-dog-sets.rds") |>
  filter(survey_abbrev %in% c("HBLL INS S", "HBLL INS N"))
mean <- mean(d$log_botdepth)
d$log_botdepth2 <- d$log_botdepth^2
d$log_botdepth_cent <- d$log_botdepth - mean
d$log_botdepth_cent2 <- d$log_botdepth_cent * d$log_botdepth_cent
grid <- readRDS("output/prediction-grid-hbll-n-s.rds") |>
  filter(area %in% c("hbll_s", "hbll_n"))
grid$log_botdepth2 <- grid$log_botdepth^2
grid$log_botdepth_cent <- grid$log_botdepth - mean
grid$log_botdepth_cent2 <- grid$log_botdepth_cent * grid$log_botdepth_cent
# from gfdata HBLL n and south merged
years <- seq(min(d$year), 2023, 1)
sort(unique(d$year))
grid <- purrr::map_dfr(years, ~ tibble(grid, year = .x))
path <- "output/fit-tv--sog-hbll-ns.rds"
pathind <- "output/ind-tv-sog-hbll-ns.rds"
extratime <- c(2006,2017,2020)


# time varying model ------------------------------------------------------

mesh <- make_mesh(d, c("UTM.lon", "UTM.lat"), cutoff = 5)
plot(mesh)

ggplot() +
  geom_point(data = d |> arrange(year), aes(UTM.lon, UTM.lat
                                            , colour = year), size = 1,
             # ), size = 0.25, alpha = 0.25,
             pch = 16) +
  inlabru::gg(mesh$mesh,
              edge.color = "grey60",
              edge.linewidth = 0.15,
              # interior = TRUE,
              # int.color = "blue",
              int.linewidth = 0.25,
              exterior = FALSE,
              # ext.color = "black",
              ext.linewidth = 0.5) +
  scale_colour_viridis_c(direction = -1) +
  labs(colour = "Year") +
  xlab("UTM (km)") + ylab("UTM (km)") + coord_fixed(expand = FALSE) +
  theme_classic() +
  theme(legend.position = "inside", legend.position.inside = c(0.2, 0.25))

fittv <- sdmTMB(
  formula = catch_count ~ 0 + as.factor(year),
  offset = "offset",
  time = "year",
  time_varying = ~ 0 + log_botdepth + log_botdepth2,
  #time_varying_type = "rw0",
  spatiotemporal = "AR1", #off or AR1 left over variation from the time varying process is ar1
  silent = FALSE,
  spatial = "on",
  family = nbinom2(),
  mesh = mesh,
  data = d,
  do_index = FALSE
)

sanity(fittv)
fittv$sd_report

saveRDS(fittv, path)

# time varying depth plot
nd <- expand.grid(
  log_botdepth =
    seq(min(d$log_botdepth), max(d$log_botdepth), length.out = 500)
)
nd$log_botdepth2 <- nd$log_botdepth^2
nd$offset <- 0
# nd$survey_name <- "GOA"
# nd$year <- 2021L # L: integer to match original data

year <- unique(d$year)
nd <- purrr::map_dfr(year, function(.x) {
  dplyr::mutate(nd, year = .x)
})

p <- predict(fittv, newdata = nd, se_fit = TRUE, re_form = NA)
#p$depth_m <- exp(p$log_botdepth_cent + mean) * -1
p$depth_m <- exp(p$log_botdepth)
p$mean_logbot_depth <- mean
saveRDS(p, pathind)

p |>
  ggplot() +
  geom_line(aes((depth_m ), exp(est), group = as.factor(year), colour = year)) +
  scale_colour_viridis_c() +
  theme_classic()
ggsave("Figures/fit-tv-predicteddepth.jpg", width = 4, height = 3)

p %>%
  group_by(year) %>%
  summarize(max_est = max(est), xint = (depth_m[est == max_est])) |>
  # filter(year > 2003) |>
  ggplot() +
  theme_classic() +
  geom_line(aes(year, xint, colour = year, group = year)) +
  geom_point(aes(year, xint, colour = year, group = year))

ggplot(p, aes(depth_m, exp(est),
              ymin = exp(est - 1.96 * est_se),
              ymax = exp(est + 1.96 * est_se),
              group = as.factor(year)
)) +
  geom_line(aes(colour = year), lwd = 1) +
  geom_ribbon(aes(fill = year), alpha = 0.1) +
  scale_colour_viridis_c() +
  scale_fill_viridis_c() +
  scale_x_continuous(labels = function(x) round(exp(x * pcod$depth_sd[1] + pcod$depth_mean[1]))) +
  coord_cartesian(expand = F) +
  labs(x = "Depth (m)", y = "Biomass density (kg/km2)")
