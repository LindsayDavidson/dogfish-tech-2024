# notes
# 2004 is missing deployment times and therefore I can't calculate soak time
# soak times were between 1.5-3 hours

library(ggplot2)
library(tidyverse)
library(sdmTMB)

latitude_cutoff <- 50.34056
bccrs <- 32609
# loc = "HBLL INS N"
# loc = "HBLL INS S"

sf::sf_use_s2(FALSE)
map_data <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")
coast <- sf::st_crop(
  map_data,
  c(xmin = -175, ymin = 20, xmax = -115, ymax = 70)
)

coast_proj <- sf::st_transform(coast, crs = bccrs)

df <- readRDS("data-raw/wrangled-hbll-dog-sets.rds") |>
  drop_na(catch_count) |>
  drop_na(offset) |>
  drop_na(julian)
df$log_botdepth2 <- df$log_botdepth * df$log_botdepth
df$julian_c <- df$julian - 172
depth <- df |>
  dplyr::select(depth_m, grouping_depth_id) |>
  distinct()
df <- df |>
  mutate(depth_bin = case_when(
    depth_m <= 70 ~ 1,
    depth_m > 70 & depth_m <= 110 ~ 2,
    depth_m > 110 & depth_m <= 165 ~ 3,
    depth_m > 165 & depth_m <= 220  ~ 4,
    depth_m > 220 ~ 5 ))

grid <- readRDS("output/prediction-grid-hbll-n-s-dog-2-km.rds") # ("output/prediction-grid-hbll-n-s-dog-1-km.rds")

grid$area_km2 <- as.numeric(grid$area_km)
grid$depth_m <- grid$depth * -1
grid$julian_c <- 36
grid$survey_lumped <- "hbll"
grid$julian <- mean(df$julian)
grid$month <- 8
grid <- grid |>
  mutate(depth_bin = case_when(
    depth_m <= 70 ~ 1,
    depth_m > 70 & depth_m <= 110 ~ 2,
    depth_m > 110 & depth_m <= 165 ~ 3,
    depth_m > 165 & depth_m <= 220  ~ 4,
    depth_m > 220 ~ 5 ))

# everything except for dogfish comp work in 2004 - used ---------------------------------------------------------------
d <- df # |>
#   filter(year != 2004 & survey_series_desc != "Dogfish Gear/Timing Comparison Surveys") #<- 2004 was removed above with the offset can't be na.
years <- seq(min(d$year), 2023, 1)
# grid <- purrr::map_dfr(unique(d$year), ~ tibble(grid, year = .x))
grid <- purrr::map_dfr(years, ~ tibble(grid, year = .x))
model <- "hblldog_no2004"
formula = catch_count ~ poly(log_botdepth, 2) + as.factor(survey_lumped)
formula_julian_int = catch_count ~ poly(log_botdepth, 2) * poly(julian_c, 2) + as.factor(survey_lumped)
formula_julian = catch_count ~ poly(log_botdepth, 2) + poly(julian_c, 2) + as.factor(survey_lumped)
formula_month = catch_count ~ poly(log_botdepth, 2) * month + as.factor(survey_lumped)
formula_depthbin = catch_count ~ depth_bin * poly(julian,2) + as.factor(survey_lumped)
sort(unique(d$year))
extratime <- c(1987, 1988, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2006, 2017, 2020)
spatial = "off"

# # hbll n and s and dog circle no jhook ----------------------------------------------
# d <- df |>
#   filter(!year %in% c(2004, 1986, 1989)) |>
#   filter(survey_sep != "dog-jhook") # |>
# # filter(survey_series_desc != "Dogfish Gear/Timing Comparison Surveys") #I should probably include this
# years <- seq(min(d$year), 2023, 1)
# # grid <- purrr::map_dfr(unique(d$year), ~ tibble(grid, year = .x))
# grid <- purrr::map_dfr(years, ~ tibble(grid, year = .x))
# model <- "hblldog_nojhook"
# formula = catch_count ~ poly(log_botdepth, 2) + as.factor(survey_lumped)
# extratime <- c(2004, 2006, 2017, 2020)
#
# # hbll n only -------------------------------------------------------------
#
# d <- readRDS("data-raw/wrangled-hbll-dog-sets.rds") |>
#   filter(survey_abbrev == "HBLL INS N") |>
#   filter(survey_desc != "Dogfish Gear/Timing Comparison Surveys")
# grid <- readRDS("output/prediction-grid-hbll-n-s.rds") |>
#   filter(area == "hbll_n") # from gfdata HBLL n and south merged
# grid <- grid |>
#   filter(lat >= latitude_cutoff) |>
#   filter(bot_depth <= 150) |>
#   filter(bot_depth >= 35)
# years <- seq(min(d$year), 2023, 1)
# sort(unique(d$year))
# # grid <- purrr::map_dfr(unique(d$year), ~ tibble(grid, year = .x))
# grid <- purrr::map_dfr(years, ~ tibble(grid, year = .x))
# grid$survey_type <- "HBLL INS N"
# grid$julian <- mean(d$julian)
# model = "hbll-n"
# formula = catch_count ~ poly(log_botdepth, 2)
# # path <- "output/fit-sog-hbll-n.rds"
# # pathind <- "output/ind-sog-hbll-n.rds"
# extratime <- c(2005, 2006, 2009, 2011, 2013, 2015, 2017, 2018, 2020, 2022)
#
#
# # hbll s only -------------------------------------------------------------
#
# d <- readRDS("data-raw/wrangled-hbll-dog-sets.rds") |>
#   filter(survey_abbrev == "HBLL INS S") |>
#   filter(survey_desc != "Dogfish Gear/Timing Comparison Surveys")
# grid <- readRDS("output/prediction-grid-hbll-n-s.rds") |>
#   filter(area == "hbll_S") # from gfdata HBLL n and south merged
# grid <- grid |>
#   filter(lat < latitude_cutoff) |>
#   filter(bot_depth <= 150) |>
#   filter(bot_depth >= 35)
# years <- seq(min(d$year), 2023, 1)
# sort(unique(d$year))
# # grid <- purrr::map_dfr(unique(d$year), ~ tibble(grid, year = .x))
# grid <- purrr::map_dfr(years, ~ tibble(grid, year = .x))
# grid$survey_type <- "HBLL INS S"
# grid$julian <- mean(d$julian)
# model = "hbll-s"
# formula = catch_count ~ poly(log_botdepth, 2)
# #check this
# extratime <- c(2005, 2006, 2009, 2011, 2013, 2015, 2017, 2018, 2020, 2022)
#
#
#

# hbll n and s only -------------------------------------------------------------
# d <- readRDS("data-raw/dogfish_sets_gfdata.rds") |> filter(survey_series_id %in% c(39, 40)) |>
#   drop_na(depth_m) |>
#   drop_na(julian) |>
#   drop_na(catch_count)
# d$offset <- d$hook_count * 2 #compare results with the gfdata base?

d <- df |>
  #filter(survey_abbrev %in% c("HBLL INS S", "HBLL INS N")) |>
  filter(survey_lumped %in% c("hbll")) |>
  drop_na(offset) |>
  drop_na(depth_m) |>
  drop_na(julian) |>
  drop_na(catch_count)
grid <- readRDS("output/prediction-grid-hbll-n-s.rds") |>
  filter(area %in% c("hbll_S", "hbll_n")) # from gfdata HBLL n and south merged
years <- seq(min(d$year), 2023, 1)
sort(unique(d$year))
# grid <- purrr::map_dfr(unique(d$year), ~ tibble(grid, year = .x))
grid <- purrr::map_dfr(years, ~ tibble(grid, year = .x))
grid$survey_type <- "hbll"
grid$julian <- mean(d$julian)
model = "hbll-n-s"
formula = catch_count ~ poly(log_botdepth, 2)
formula_depthbin= catch_count ~ depth_bin * poly(julian,2)
sort(unique(d$year))
extratime <- c(2006, 2017, 2020)
spatial = "on"



# circle dog survey only ---------------------------------------------------------
# need a prediction grid for just DOG points use the grid I made a trucate to the max latitude
d <- df |>
  # filter(activity_desc == "STRAIT OF GEORGIA DOGFISH LONGLINE SURVEY") |> #this gets rid of 2023
  # filter(hook_desc == "CIRCLE HOOK")
  filter(survey_sep %in% c("dog comp", "dog")) |>
  filter(month > 9) #i did this to drop the comp work that happened in summer, keep in and include julian if wanted
sort(unique(d$year)) #these are the correct dogfish survey year + 2023
sort(unique(d$month))
max(d$latitude)
years <- seq(min(d$year), max(d$year), 1)
# grid <- purrr::map_dfr(unique(d$year), ~ tibble(grid, year = .x))
grid <- purrr::map_dfr(years, ~ tibble(grid, year = .x))
grid$survey_type <- "DOG"
grid <- grid |> filter(latitude < 50)
model = "dog-circle"
formula = catch_count ~ poly(log_botdepth, 2)
formula_depthbin = catch_count ~ poly(depth_bin,2) * julian
extratime <- c(2006, 2007, 2009, 2010, 2012, 2013, 2015, 2016, 2017, 2018, 2020,2021,2022)
spatial = "off"
x <- ggplot(grid, aes(longitude, latitude)) + geom_point()
x + geom_point(data = d, aes(longitude, latitude), colour = "red")

# plots to check data -----------------------------------------------------

ggplot(d, aes(longitude, latitude, colour = survey_abbrev)) +
  geom_point() +
  theme_classic()

ggplot(d, aes(longitude, latitude, colour = survey_lumped)) +
  geom_point() +
  theme_classic()

d |>
  ggplot() +
  geom_jitter(aes(year, catch_count, colour = survey_lumped, size = catch_count))

d |>
  ggplot() +
  geom_jitter(aes(year, offset, colour = survey_lumped, size = catch_count))

d |>
  ggplot() +
  geom_jitter(aes(year, logbot_depth, colour = survey_lumped, size = catch_count))

d |>
  ggplot() +
  geom_point(aes(longitude, latitude, colour = catch_count / offset)) +
  facet_wrap(~year) +
  scale_colour_viridis_c()

d |>
  ggplot() +
  geom_point(aes(log_botdepth, catch_count, colour = survey_lumped))

d |>
  ggplot() +
  geom_point(aes(catch_count, offset, colour = survey_lumped))

d |>
  ggplot() +
  geom_point(aes(catch_count, log_botdepth, colour = survey_lumped))

d |>
  ggplot() +
  geom_point(aes(longitude, latitude, colour = catch_count)) +
  facet_wrap(~year)

d |>
  group_by(year) |>
  summarise(catch = sum(catch_count / offset)) |>
  ggplot() +
  geom_point(aes(year, catch)) +
  geom_line(aes(year, catch))

d |>
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

range(grid$bot_depth)
range(grid$log_botdepth)
range(grid$bot_depth)
range(d$depth_m)
range(d$log_botdepth)
range(d$catch_count)
range(d$julian)

d <- d |> drop_na(catch_count)

# index generation function --------

indexfunc <- function(d) {
  mesh <- make_mesh(d, c("UTM.lon", "UTM.lat"), cutoff = 5)
  # mesh <- make_mesh(d, c("UTM.lon", "UTM.lat"), cutoff = 2) #hbll s only
  plot(mesh)
  mesh$mesh$n

  ggplot() +
    geom_point(
      data = d |> arrange(year), aes(UTM.lon, UTM.lat, colour = year),
      size = 1,
      # ), size = 0.25, alpha = 0.25,
      pch = 16
    ) +
    inlabru::gg(mesh$mesh,
      edge.color = "grey60",
      edge.linewidth = 0.15,
      # interior = TRUE,
      # int.color = "blue",
      int.linewidth = 0.25,
      exterior = FALSE,
      # ext.color = "black",
      ext.linewidth = 0.5
    ) +
    scale_colour_viridis_c(direction = -1) +
    labs(colour = "Year") +
    xlab("UTM (km)") +
    ylab("UTM (km)") +
    coord_fixed(expand = FALSE) +
    theme_classic() +
    theme(legend.position = "inside", legend.position.inside = c(0.2, 0.25))
  ggsave(paste0("Figures/trawl-model-mesh-", model, ".pdf"), width = 6, height = 6)

  # ggplot() +
  #   inlabru::gg(mesh$mesh) +
  #   geom_point(data = d, aes(UTM.lon, UTM.lat), size = 0.5, alpha = 0.7, pch = 21) +
  #   xlab("UTM (km)") +
  #   ylab("UTM (km)") +
  #   coord_fixed()
  # # ggsave("Figures/mesh.pdf", width = 6, height = 6)

  unique(sort(d$year))
  unique(sort(d$survey_lumped))

  fit <- sdmTMB(
    formula = formula,
    data = d,
    time = "year",
    offset = "offset",
    mesh = mesh,
    spatial = spatial,
    spatiotemporal = "rw",
    family = nbinom2(),
    silent = FALSE,
    share_range = FALSE,
    extra_time = extratime
    # do_index = TRUE,
    # predict_args = list(newdata = grid),
    # index_args = list(area = grid$area_km2)
  )

  fit$sd_report
  sanity(fit)
  saveRDS(fit, file = paste0("output/fit-sog-", model, ".rds"))

  pred <- predict(fit, grid, return_tmb_object = TRUE, response = TRUE)
  index <- get_index(pred, bias_correct = TRUE)
  index <- index |> mutate(model = ifelse(year %in% unique(d$year), "yrs_surved", "yrs_interp"))
  saveRDS(index, file = paste0("output/ind-sog-", model, ".rds"))

  # fitjulian2 <- update(fit, formula = formula_julian_int)
  # sanity(fitjulian2)
  # fitjulian2$sd_report
  # saveRDS(fitjulian2, file = paste0("output/fit-sog-", model, "-julian-interaction.rds"))

  # pred <- predict(fitjulian2, grid, return_tmb_object = TRUE, response = TRUE)
  # index <- get_index(pred, bias_correct = TRUE)
  # index <- index |> mutate(model = ifelse(year %in% unique(d$year), "yrs_surved", "yrs_interp"))
  # saveRDS(index, file = paste0("output/ind-sog-", model, "-julian-interaction.rds"))
  #
  # fitjulian <- update(fit, formula = formula_julian)
  # sanity(fitjulian)
  # fitjulian$sd_report
  # saveRDS(fitjulian, file = paste0("output/fit-sog-", model, "-julian.rds"))
  #
  # pred <- predict(fitjulian, grid, return_tmb_object = TRUE, response = TRUE)
  # index <- get_index(pred, bias_correct = TRUE)
  # index <- index |> mutate(model = ifelse(year %in% unique(d$year), "yrs_surved", "yrs_interp"))
  # saveRDS(index, file = paste0("output/ind-sog-", model, "-month.rds"))

  # fitmonth <- update(fit, formula = formula_month)
  # sanity(fitmonth)
  # fitmonth$sd_report
  # saveRDS(fitmonth, file = paste0("output/fit-sog-", model, "-month-int.rds"))
  #
  # pred <- predict(fitmonth, grid, return_tmb_object = TRUE, response = TRUE)
  # index <- get_index(pred, bias_correct = TRUE)
  # index <- index |> mutate(model = ifelse(year %in% unique(d$year), "yrs_surved", "yrs_interp"))
  # saveRDS(index, file = paste0("output/ind-sog-", model, "-month.rds"))
  #
  fitdepthbin<- update(fit, formula = formula_depthbin)
  sanity(fitdepthbin)
  fitdepthbin$sd_report
  saveRDS(fitdepthbin, file = paste0("output/fit-sog-", model, "-depthbin-julian-int.rds"))

  pred <- predict(fitdepthbin, grid, return_tmb_object = TRUE, response = TRUE)
  index <- get_index(pred, bias_correct = TRUE)
  index <- index |> mutate(model = ifelse(year %in% unique(d$year), "yrs_surved", "yrs_interp"))
  saveRDS(index, file = paste0("output/ind-sog-", model, "-depthbin-julian-int.rds"))

  yearlabs <- as.list(index |> filter(model == "yrs_surved") |> reframe(year = year))
  yearlabs <- yearlabs$year

  ggplot(index, aes(year, (est))) +
    geom_line(col = "#8D9999") +
    geom_point(col = "#8D9999") +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.4, fill = "#8D9999") +
    theme_classic() +
    scale_x_continuous(breaks = c(years))

  ggplot(index, aes(year, log(est), ymin = log(lwr), ymax = log(upr))) +
    # geom_pointrange(data = filter(index, model == "yrs_interp"), mapping = aes(x = year - 0.25), size = 0.2, pch = 5, colour = "grey40", alpha = 0.6) +
    geom_pointrange(data = filter(index, model == "yrs_surved"), mapping = aes(x = year - 0.25), size = 0.2, pch = 5, colour = "red", alpha = 0.6) +
    theme_classic() +
    scale_x_continuous(breaks = c(yearlabs)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}


# run functions -----------------------------------------------------------


indexfunc(d)
