#design based cpue and depth through time
# 1. 0 to 55m,
# 2. 56 to 110m,
# 3. 111 to 165m,
# 4. 166 to 220m and
# 5. deeper than 220m.


# all without 2004 comp work ----------------------------------------------

d <- readRDS("data-raw/wrangled-hbll-dog-sets.rds") |>
  filter(year != 2004) |>
  filter(survey_abbrev == "dog")
range(d$depth_m)
d$log_botdepth2 <- d$log_botdepth * d$log_botdepth
str(d$month)
d <- d |>
  mutate(cpue = catch_count/(exp(offset)))
d <- d |>
  mutate(depth_group = ifelse(depth_m %in% c(0:55), 1,
                              ifelse(depth_m %in% c(56:110), 2,
                                     ifelse(depth_m %in% c(111:165), 3,
                                            ifelse(depth_m %in% c(166:220), 4,
                                                   ifelse(depth_m >220, 5, NA))))))
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

ggplot(d, aes(depth_m, cpue, group = as.factor(year), colour = as.factor(year))) +
  geom_point() +
  geom_line()

d |>
  group_by(depth_group, year) |>
  mutate(meancpue = mean(cpue)) |>
  ggplot() +
  geom_point(aes(depth_group, meancpue, group = as.factor(year), colour = as.factor(year))) +
  geom_line(aes(depth_group, meancpue, group = as.factor(year), colour = as.factor(year))) +
  scale_colour_viridis_d()

d |>
  #group_by(depth_group, year) |>
  #mutate(meancpue = mean(cpue)) |>
  ggplot() +
  geom_jitter(aes(depth_group, cpue, group = as.factor(year), colour = as.factor(year))) +
  geom_smooth(aes(depth_group, cpue, group = as.factor(year), colour = as.factor(year)), fill = 'grey', alpha = 0.25) +
  scale_colour_viridis_d()
ggsave("Figures/dog-timevaryingdepth.jpg", width = 5, height = 3)
