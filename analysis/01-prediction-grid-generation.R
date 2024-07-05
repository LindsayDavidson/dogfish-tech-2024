# prediction grid from gfplot ---------------------------------------------

hbll_ins_s <- gfplot::hbll_inside_s_grid
hbll_ins_s$grid$area <- "hbll_s"
hbll_ins_n <- gfplot::hbll_inside_n_grid
hbll_ins_n$grid$area <- "hbll_n"

hbll_ins  <- rbind(hbll_ins_n$grid, hbll_ins_s$grid)

# needs depth
range(hbll_ins$X)
range(hbll_ins$Y)
hbll_ins <- hbll_ins[!duplicated(hbll_ins), ] # just checking

b <- marmap::getNOAA.bathy(lon1 = -128, lon2 = -122, lat1 = 47, lat2 = 51, resolution = 1)

bdepths <- marmap::get.depth(b, hbll_ins[, c("X", "Y")], locator = FALSE) |>
  mutate(bot_depth = (depth * -1)) |>
# rename(longitude = lon, latitude = lat) |>
# filter(bot_depth > 25) |>
# mutate(logbot_depth = log(bot_depth)) |>
  inner_join(hbll_ins, by = c("lon" = "X" ,"lat" =  "Y"))

bdepths[duplicated(bdepths), ] # just checking
grid <- filter(bdepths, depth < 0)

grid <- add_utm_columns(grid,
                        ll_names = c("lon", "lat"),
                        utm_names = c("UTM.lon", "UTM.lat"),
                        utm_crs = bccrs
) |>
  mutate(UTM.lon.m = UTM.lon * 1000, UTM.lat.m = UTM.lat * 1000) |>
  mutate(log_botdepth = log(bot_depth))

saveRDS(grid, 'output/prediction-grid-sog.rds')
