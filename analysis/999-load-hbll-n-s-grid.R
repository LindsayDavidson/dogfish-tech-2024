
grid <- readRDS("output/prediction-grid-hbll-n-s.rds") |> #use this one or the other??
  filter(area %in% c("hbll_s", "hbll_n")) # from gfdata HBLL n and south merged
#grid$julian <- mean(d$julian)


grid <- grid |>
  mutate(depth_bin = case_when(
    bot_depth <= 70 ~ 1,
    bot_depth > 70 & bot_depth <= 110 ~ 2,
    bot_depth > 110 & bot_depth <= 165 ~ 3,
    bot_depth > 165 & bot_depth <= 220  ~ 4,
    bot_depth > 220 ~ 5 ))

ggplot(grid, aes(UTM.lon, UTM.lat)) + geom_point()

# grid <- filter(grid, bot_depth %in% 10:150)
# grid <- grid |> #4 points are deeper than 110, instead of dropping ill change the categorizations
#   mutate(depth_bin = case_when(
#     depth_m <= 70 ~ 1,
#     depth_m > 70 & depth_m <= 150 ~ 2))
