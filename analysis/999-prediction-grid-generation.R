library(tidyverse)
library(PBSdata)
library(sdmTMB)

#for now make one grid but need to make a grid for north, south, and then dogfish areas (? or would the south grid work?)
#params

bccrs = 32609
buffersize = 8
gridsize = 500 #0.25 km2 grid cell, was 2000 m before, too big bc of depth centroid issues
path_extent <- "output/PredictionGridExtent.shp"
path_center <- "output/PredictionGridCentres.shp"
path_final <- paste0("output/prediction-grid-hbll-n-s-dog-", gridsize/1000, "-km.rds")
mindepth <- 5
maxdepth <- 350


# # depth file from Dana used in yelloweye assessment
# x <- readRDS("data-raw/hbll-inside-grid_water-area.rds")
# ggplot(x, aes(longitude, latitude, colour = depth)) + geom_point()
# ggplot(x, aes(longitude, latitude, colour = rock100)) + geom_point()
#
# #depth file from Sean on slack
# y <- readRDS("data-raw/complete_research_blocks_table.rds")
# y <- y |> dplyr::select(starts_with("ZonalStats_bathy100.M"),
#                         exported_research_blocks.LATITUDE,
#                         exported_research_blocks.LONGITUDE, exported_research_blocks.SS_ALT_DES) |>
#   filter(exported_research_blocks.SS_ALT_DES == "INS S") |>
#   rename(latitude = exported_research_blocks.LATITUDE,
#          longitude = exported_research_blocks.LONGITUDE,
#          depth_m_gis = ZonalStats_bathy100.MEAN)
# ggplot(y, aes(longitude, latitude, colour = depth_m_gis)) + geom_point()
# ggplot(y, aes(depth_m_gis, longitude, colour = depth_m_gis)) + geom_point()
#
# z <- left_join(x, y)
# grid <- grid |> dplyr::select(-area)
# ggplot(z, aes(longitude, latitude, colour = depth)) + geom_point()
# ggplot(z, aes(depth, depth_m_gis)) + geom_point()


# prediction grid from gfplot ---------------------------------------------
hbll_ins_s <- gfplot::hbll_inside_s_grid
hbll_ins_s$grid$area <- "hbll_s"
hbll_ins_n <- gfplot::hbll_inside_n_grid
hbll_ins_n$grid$area <- "hbll_n"
hbll_ins <- rbind(hbll_ins_n$grid, hbll_ins_s$grid)

# needs depth
range(hbll_ins$X)
range(hbll_ins$Y)
range(hbll_ins_n$grid$Y)
range(hbll_ins_s$grid$Y)

hbll_ins <- hbll_ins[!duplicated(hbll_ins), ] # just checking
ggplot(hbll_ins, aes(Y, X)) + geom_point() +
  geom_point(data = )

b <- marmap::getNOAA.bathy(lon1 = -128, lon2 = -122, lat1 = 47, lat2 = 51, resolution = 1)

bdepths <- marmap::get.depth(b, hbll_ins[, c("X", "Y")], locator = FALSE) |>
  mutate(bot_depth = (depth * -1)) |>
  #filter(bot_depth  < 120) |>
  #filter(bot_depth > 10) |>
# rename(longitude = lon, latitude = lat) |>
# filter(bot_depth > 25) |>
# mutate(logbot_depth = log(bot_depth)) |>
  inner_join(hbll_ins, by = c("lon" = "X" ,"lat" =  "Y"))

bdepths[duplicated(bdepths), ] # just checking
grid <- filter(bdepths, depth < 0)
range(grid$bot_depth)

ggplot(grid, aes(lon, lat, colour = bot_depth)) + geom_point()
grid |> filter(bot_depth > 120) |> tally()
grid |> filter(bot_depth < 10) |> tally()

#grid <- filter(grid, bot_depth  < 120)
grid <- filter(grid, bot_depth > 10)

grid <- add_utm_columns(grid,
                        ll_names = c("lon", "lat"),
                        utm_names = c("UTM.lon", "UTM.lat"),
                        utm_crs = bccrs
) |>
  mutate(UTM.lon.m = UTM.lon * 1000, UTM.lat.m = UTM.lat * 1000) |>
  mutate(log_botdepth = log(bot_depth))

range(grid$depth)
range(grid$bot_depth)
range(grid$log_botdepth)
range(grid$area)

saveRDS(grid, 'output/prediction-grid-hbll-n-s.rds')


# prediction grid for HBLL north and south and DOG Strait of Georgia ----------------------------

d <- readRDS("data-raw/wrangled-hbll-dog-sets.rds") # no expansion set along the strait

# make the grid function
gridfunc <- function(d) {
  d_sf <- st_as_sf(d, coords = c("UTM.lon.m", "UTM.lat.m"), crs = bccrs)
  d_sf_crs <- st_transform(d_sf, crs = bccrs)

  hull <- concaveman::concaveman(d_sf)
  hulls <- st_buffer(hull, buffersize)

  plot(st_geometry(hulls), col = "blue")

  # change resolution here
  polygony <- st_make_grid(hulls, square = TRUE, cellsize = c(gridsize, gridsize)) %>%
    st_sf() %>%
    mutate(cell_ID = row_number())
  center <- st_centroid(polygony)

  center_extent <- st_intersection(st_geometry(hulls), st_geometry(center))
  grid_extent <- st_intersection(st_geometry(hulls), st_geometry(polygony))
  test_center <- st_sf(center_extent)
  grid_extent <- st_sf(grid_extent)
  grid_extent$area_km <- st_area(grid_extent) / 1000000 # m to km
  range(grid_extent$area_km)

  st_write(grid_extent, path_extent, append = FALSE)
  st_write(test_center, path_center, append = FALSE)
  testcentre <- st_read(path_center)

  testcentre2 <- st_transform(testcentre, crs = "+proj=latlon +datum=WGS84")
  df <- testcentre2 %>%
    mutate(
      lat = unlist(purrr::map(testcentre2$geometry, 2)),
      long = unlist(purrr::map(testcentre2$geometry, 1))
    ) |>
    st_drop_geometry()

  # browser()

  # # Use get.depth to get the depth for each point
  # suppressWarnings(bio_depth <- getNOAA.bathy(lon1 = -170, lon2 = -120, lat1 = 30, lat2 = 70, resolution = 1, keep = TRUE))
  suppressWarnings(bio_depth <- marmap::getNOAA.bathy(lon1 = -180, lon2 = -110, lat1 = 40, lat2 = 60, resolution = 1, keep = TRUE))

  df_depths <- marmap::get.depth(bio_depth, df[, c("long", "lat")], locator = FALSE) %>%
    mutate(bot_depth = (depth * -1)) %>%
    rename(longitude = lon, latitude = lat) %>%
    filter(bot_depth > mindepth & bot_depth < maxdepth) %>%
    mutate(log_botdepth = log(bot_depth)) %>%
    inner_join(df, by = c("longitude" = "long", "latitude" = "lat")) |>
    dplyr::select(-FID) |>
    distinct(.keep_all = TRUE)

  df_depths[duplicated(df_depths), ] # just checking

  grid1 <- add_utm_columns(df_depths,
                           ll_names = c("longitude", "latitude"),
                           utm_names = c("UTM.lon", "UTM.lat"),
                           utm_crs = bccrs
  ) %>%
    mutate(UTM.lon.m = UTM.lon * 1000, UTM.lat.m = UTM.lat * 1000)

  # join the points back to the grid so I can predict on the grid
  grid2 <- st_join(grid_extent,
                   st_as_sf(grid1, coords = c("UTM.lon.m", "UTM.lat.m"), crs = bccrs),
                   join = st_contains
  ) %>%
    drop_na(depth) %>%
    st_drop_geometry()

  grid2 <- grid2 |>
    mutate(UTM.lon.m = UTM.lon * 1000, UTM.lat.m = UTM.lat * 1000)

  grid4_ras <- grid2 %>%
    mutate(across(c(UTM.lon, UTM.lat), \(x) round(x, digits = 2)))
  grid4_ras$area_km <- grid4_ras$area_km
  saveRDS(grid4_ras, path_final)
}

# run the grid function
gridfunc(d)

