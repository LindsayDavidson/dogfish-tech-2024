
grid <- readRDS("output/prediction-grid-hbll-n-s.rds") |> #use this one or the other??
  filter(area %in% c("hbll_S", "hbll_n")) # from gfdata HBLL n and south merged
years <- seq(min(d$year), max(d$year), 1)
sort(unique(d$year))
# grid <- purrr::map_dfr(unique(d$year), ~ tibble(grid, year = .x))
grid <- purrr::map_dfr(years, ~ tibble(grid, year = .x))
grid$julian <- mean(d$julian)
model = "hbll-n-s"
formula = catch_count ~ poly(log_botdepth, 2)
#formula_depthbin= catch_count ~ depth_bin * poly(julian,2)
sort(unique(d$year))
extratime <- c(2006, 2017, 2020)
spatial = "on"
range(grid$bot_depth)
grid <- grid |>
  mutate(depth_bin = case_when(
    bot_depth <= 70 ~ 1,
    bot_depth > 70 & bot_depth <= 110 ~ 2,
    bot_depth > 110 & bot_depth <= 165 ~ 3,
    bot_depth > 165 & bot_depth <= 220  ~ 4,
    bot_depth > 220 ~ 5 ))
# grid <- filter(grid, bot_depth %in% 10:150)
# grid <- grid |> #4 points are deeper than 110, instead of dropping ill change the categorizations
#   mutate(depth_bin = case_when(
#     depth_m <= 70 ~ 1,
#     depth_m > 70 & depth_m <= 150 ~ 2))
