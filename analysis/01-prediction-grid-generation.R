# prediction grid from gfplot ---------------------------------------------

hbll_ins <- gfplot::hbll_inside_s_grid
plot(hbll_ins$grid$X, hbll_ins$grid$Y)
points(d$longitude, d$latitude, col = "red")
range(hbll_ins$grid)
# needs depth
range(hbll_ins$grid$X)
range(hbll_ins$grid$Y)
hbll_ins$grid <- hbll_ins$grid[!duplicated(hbll_ins$grid), ] # just checking

b <- marmap::getNOAA.bathy(lon1 = -126, lon2 = -122, lat1 = 47, lat2 = 51, resolution = 1)

bdepths <- marmap::get.depth(b, hbll_ins$grid[, c("X", "Y")], locator = FALSE) |>
  mutate(bot_depth = (depth * -1))
# rename(longitude = lon, latitude = lat) |>
# filter(bot_depth > 25) |>
# mutate(logbot_depth = log(bot_depth)) |>
# inner_join(hbll_ins$grid, by = c("X" = "X", "Y" = "Y"))

bdepths[duplicated(bdepths), ] # just checking

grid <- filter(bdepths, depth < 0)
plot(hbll_ins$grid$X, hbll_ins$grid$Y)
points(grid$lon, grid$lat, col = "red")

grid <- add_utm_columns(grid,
                        ll_names = c("lon", "lat"),
                        utm_names = c("UTM.lon", "UTM.lat"),
                        utm_crs = bccrs
) |>
  mutate(UTM.lon.m = UTM.lon * 1000, UTM.lat.m = UTM.lat * 1000) |>
  mutate(log_botdepth = log(bot_depth))
saveRDS(grid, 'output/prediction-grid-sog.rds')
