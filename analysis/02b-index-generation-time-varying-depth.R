library(sdmTMB)
library(tidyverse)


# data ---------------------------------------------------------------
# all without 2004 comp work ----------------------------------------------

d <- readRDS("data-raw/wrangled-hbll-dog-sets.rds") |>
  filter(year != 2004)
range(d$depth_m)
d$log_botdepth2 <- d$log_botdepth * d$log_botdepth
str(d$month)
grid <- grid <- readRDS(
  #"output/prediction-grid-hbll-n-s-dog-1-km.rds")
  "output/prediction-grid-hbll-n-s-dog-2-km.rds")# from convex hull to have deeper depths
grid$log_botdepth2 <- grid$log_botdepth * grid$log_botdepth
grid$area_km2 <- as.numeric(grid$area_km)
years <- seq(min(d$year), 2023, 1)
grid <- purrr::map_dfr(years, ~ tibble(grid, year = .x))
grid$survey2 <- "hbll"
grid$julian <- mean(d$julian)
grid$month <- 7
path <- "output/fit-tv-sog-hblldog_no2004.rds"
pathind <- "output/ind-tv-sog-hblldog_no2004.rds"
sort(unique(d$year))
extratime <- c(1987, 1988, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2004, 2006, 2017, 2020)

plot(grid$longitude, grid$latitude)
points(d$longitude, d$latitude, col = "red")


# hbll n and s only -------------------------------------------------------------

d <- readRDS("data-raw/wrangled-hbll-dog-sets.rds") |> filter(survey_abbrev %in% c("HBLL INS S", "HBLL INS N"))
range(d$depth_m)
ggplot(d, aes(depth_m, catch_count)) + geom_point()
ggplot(d, aes(log_botdepth, catch_count)) + geom_point()
ggplot(d, aes(longitude, latitude, colour = depth_m)) + geom_point()

d$log_botdepth2 <- d$log_botdepth * d$log_botdepth
grid <- readRDS
  ("output/prediction-grid-hbll-n-s.rds") |>
  filter(area %in% c("hbll_s", "hbll_n")) |> filter(log_botdepth >0 ) |> filter(depth > -110)
grid$
  grid$log_botdepth2 <- grid$log_botdepth * grid$log_botdepth
# from gfdata HBLL n and south merged
years <- seq(min(d$year), 2022, 1)
sort(unique(d$year))
# grid <- purrr::map_dfr(unique(d$year), ~ tibble(grid, year = .x))
grid <- purrr::map_dfr(years, ~ tibble(grid, year = .x))
grid$julian <- mean(d$julian)
ggplot(grid, aes(log_botdepth, year)) + geom_point()

path <- "output/fit-sog-hbll-s.rds"
pathind <- "output/ind-sog-hbll-s.rds"
extratime <- c(2006,2007, 2010, 2012, 2014, 2016, 2017, 2019, 2020)

plot(grid$lon, grid$lat)
points(d$longitude, d$latitude, col = "red")


# time varying model ------------------------------------------------------

fittv <- sdmTMB(
  formula = catch_count ~ 1 + logbot_depth_cent + logbot_depth_cent2 + as.factor(survey3),
  offset = "offset",
  time = "year",
  time_varying = ~ 1 + logbot_depth_cent + logbot_depth_cent2,
  time_varying_type = "rw0",
  spatiotemporal = "off", # left over variation from the time varying process is ar1
  silent = FALSE,
  spatial = "on",
  # priors = sdmTMBpriors(
  #   b = normal(location = c(NA, 0, 0), scale = c(NA, 1, 1))
  # ),
  # control = sdmTMBcontrol(
  #   newton_loops = 1L,
  #   start = list(
  #     ln_tau_V = matrix(log(0.1), 3, 2)
  #   ),
  #   map = list(
  #     ln_tau_V = factor(as.vector(matrix(c(1, NA, NA, 2, NA, NA), 3, 2)))
  #   )
  # ),
  family = nbinom2(), # goa,nw1, time varying imm, bc mature males
  mesh = mesh,
  data = d,
  do_index = FALSE
)

sanity(fittv)
fittv$sd_report
tvpath

saveRDS(fittv, tvpath)

# time varying depth plot
nd <- expand.grid(
  logbot_depth_cent =
    seq(min(d$logbot_depth_cent), max(d$logbot_depth_cent), length.out = 500)
)
nd$logbot_depth_cent2 <- nd$logbot_depth_cent^2
nd$offset <- 0
# nd$survey_name <- "GOA"
# nd$year <- 2021L # L: integer to match original data

year <- unique(d$year)
nd <- purrr::map_dfr(year, function(.x) {
  dplyr::mutate(nd, year = .x)
})

unique(nd$year)
unique(d$year)
unique(grid$year)

p <- predict(fittv, newdata = nd, se_fit = TRUE, re_form = NA)
ptvpath
p$depth_m <- exp(p$logbot_depth_cent + mean_logbot_depth) * -1
p$mean_logbot_depth <- mean_logbot_depth
saveRDS(p, ptvpath)