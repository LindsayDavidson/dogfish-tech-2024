# notes
# 2004 is missing deployment times and therefore I can't calculate soak time
# soak times were between 1.5-3 hours

library(ggplot2)
library(tidyverse)
library(sdmTMB)

bccrs <- 32609
# loc = "HBLL INS N"
# loc = "HBLL INS S"


# map ---------------------------------------------------------------------
sf::sf_use_s2(FALSE)
map_data <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")
coast <- sf::st_crop(
  map_data,
  c(xmin = -175, ymin = 20, xmax = -115, ymax = 70)
)

coast_proj <- sf::st_transform(coast, crs = bccrs)



# data ---------------------------------------------------------------
# pick one

# all
d <- readRDS("data-raw/wrangled-hbll-dog-sets.rds")
sort(unique(d$year))
grid <- readRDS("output/prediction-grid-hbll-n-s-dog.rds") # from convex hull to have deeper depths
grid$area_km2 <- as.numeric(grid$area_km)
years <- seq(min(d$year), 2023, 1)
# grid <- purrr::map_dfr(unique(d$year), ~ tibble(grid, year = .x))
grid <- purrr::map_dfr(years, ~ tibble(grid, year = .x))
grid$survey_type <- "hbll"
grid$julian <- mean(d$julian)
path <- "output/fit-sog-hblldog.rds"
pathind <- "output/ind-sog-hblldog.rds"
extratime <- c(1987, 1988, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2006, 2007, 2017, 2020)

plot(grid$longitude, grid$latitude)
points(d$longitude, d$latitude, col = "red")

# all without 2004 comp work
d <- readRDS("data-raw/wrangled-hbll-dog-sets.rds") |>
  filter(year != 2004)
grid <- readRDS("output/prediction-grid-hbll-n-s-dog.rds") # from convex hull to have deeper depths
grid$area_km2 <- as.numeric(grid$area_km)
years <- seq(min(d$year), 2023, 1)
# grid <- purrr::map_dfr(unique(d$year), ~ tibble(grid, year = .x))
grid <- purrr::map_dfr(years, ~ tibble(grid, year = .x))
grid$survey_type <- "hbll"
grid$julian <- mean(d$julian)
grid$month <- 7
path <- "output/fit-sog-hblldog_no2004.rds"
pathind <- "output/ind-sog-hblldog_no2004.rds"
sort(unique(d$year))
extratime <- c(1987, 1988, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2004, 2006, 2017, 2020)

plot(grid$longitude, grid$latitude)
points(d$longitude, d$latitude, col = "red")

# hbll n only
d <- readRDS("data-raw/wrangled-hbll-dog-sets.rds") |> filter(survey_abbrev == "HBLL INS N")
grid <- readRDS("output/prediction-grid-hbll-n-s.rds") |> filter(area == "hbll_n") # from gfdata HBLL n and south merged
min(d$latitude)
grid <- grid |> filter(lat >=50.30182) |> filter(bot_depth <= 150) |> filter(bot_depth >= 35)
years <- seq(min(d$year), 2023, 1)
sort(unique(d$year))
# grid <- purrr::map_dfr(unique(d$year), ~ tibble(grid, year = .x))
grid <- purrr::map_dfr(years, ~ tibble(grid, year = .x))
grid$survey_type <- "HBLL INS N"
grid$julian <- mean(d$julian)
path <- "output/fit-sog-hbll-n.rds"
pathind <- "output/ind-sog-hbll-n.rds"
extratime <- c(2005, 2006, 2009, 2011, 2013, 2015, 2017, 2018, 2020, 2022)

plot(grid$lon, grid$lat)
points(d$longitude, d$latitude, col = "red")

# hblls s only
d <- readRDS("data-raw/wrangled-hbll-dog-sets.rds") |> filter(survey_abbrev == "HBLL INS S")
grid <- readRDS("output/prediction-grid-hbll-n-s.rds") |> filter(area == "hbll_s") # from gfdata HBLL n and south merged
years <- seq(min(d$year), 2023, 1)
sort(unique(d$year))
# grid <- purrr::map_dfr(unique(d$year), ~ tibble(grid, year = .x))
grid <- purrr::map_dfr(years, ~ tibble(grid, year = .x))
grid$survey_type <- "HBLL INS S"
grid$julian <- mean(d$julian)
path <- "output/fit-sog-hbll-s.rds"
pathind <- "output/ind-sog-hbll-s.rds"
extratime <- c(2004,2006,2017,2020)

plot(grid$lon, grid$lat)
points(d$longitude, d$latitude, col = "red")

# dog survey only - need a prediction grid for just DOG points
d <- readRDS("data-raw/wrangled-hbll-dog-sets.rds") |> filter(survey_abbrev %in% c("HBLL INS S", "dog", "dog-jhook"))
max(d$latitude)
grid <- readRDS("output/prediction-grid-hbll-n-s-dog.rds") |> filter(latitude <= 50.3208)

years <- seq(min(d$year), 2023, 1)
# grid <- purrr::map_dfr(unique(d$year), ~ tibble(grid, year = .x))
grid <- purrr::map_dfr(years, ~ tibble(grid, year = .x))
grid$survey_type <- "hbll"
grid$julian <- mean(d$julian)
path <- "output/fit-sog-dog-hblls.rds"
pathind <- "output/ind-sog-dog-hblls.rds"
sort(unique(d$year))
extra_time <- c(1987, 1988, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2006, 2017, 2020)

plot(grid$longitude, grid$latitude)
points(d$longitude, d$latitude, col = "red")


# dog survey only - need a prediction grid for just DOG points
d <- readRDS("data-raw/wrangled-hbll-dog-sets.rds") |> filter(survey_abbrev %in% c("dog"))
max(d$latitude)
range(d$year)
grid <- d |>  dplyr::select(latitude, longitude, log_botdepth) |>
  distinct(latitude, longitude, log_botdepth)
years <- seq(min(d$year), 2023, 1)
#grid <- purrr::map_dfr(unique(d$year), ~ tibble(grid, year = .x))
grid <- purrr::map_dfr(years, ~ tibble(grid, year = .x))
grid$survey_type <- "dog"
grid$julian <- mean(d$julian)
path <- "output/fit-sog-dog.rds"
pathind <- "output/ind-sog-dog.rds"
sort(unique(d$year))
extra_time <- c(1987, 1988, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2006, 2007, 2009, 2010, 2012, 2013, 2015, 2016, 2017, 2018, 2020, 2021)
extra_time <- c(2006, 2007, 2009, 2010, 2012, 2013, 2015, 2016, 2017, 2018, 2020, 2021)

plot(grid$longitude, grid$latitude)
points(d$longitude, d$latitude, col = "red")



# rm 2004 calibration work??
# rm <- filter(d, year == 2004 & survey_abbrev %in% c("dog-jhook", "dog")) #the catch rates are so #low and I don't know what the soak time was
# d <- filter(d, !(fishing_event_id %in% rm$fishing_event_id))

ggplot(d, aes(longitude, latitude, colour = survey_abbrev)) +
  geom_point() + theme_classic()

d |>
  ggplot( ) +
  geom_jitter(aes(year, catch_count, colour = survey_type, size = catch_count))

d |>
  ggplot( ) +
  geom_point(aes(log_botdepth, catch_count,   colour = survey_type))

d |>
  ggplot( ) +
  geom_point(aes(catch_count, offset,  colour = survey_type))

d |>
  ggplot( ) +
  geom_point(aes(catch_count, log_botdepth,  colour = survey_type))

d |>
  group_by(year) |>
  summarise(catch = sum(catch_count/offset)) |>
  ggplot( ) +
  geom_point(aes(year, catch)) +
  geom_line(aes(year, catch))

range(grid$bot_depth)
range(grid$log_botdepth)
range(d$depth_m)
range(d$log_botdepth)

# make this a function so that I can create index for HBLL N, HBLL --------

indexfunc <- function(d) {
  mesh <- make_mesh(d, c("UTM.lon", "UTM.lat"), cutoff = 5)
  #mesh <- make_mesh(d, c("UTM.lon", "UTM.lat"), cutoff = 2) #hbll n only
  plot(mesh)
  mesh$mesh$n



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
  # ggsave(paste0("figs/trawl-model-mesh-",min_edge,"-", max_edge,".pdf"), width = 6, height = 6)
  ggsave("Figures/model-mesh.png", width = 6, height = 6)


  # ggplot() +
  #   inlabru::gg(mesh$mesh) +
  #   geom_point(data = d, aes(UTM.lon, UTM.lat), size = 0.5, alpha = 0.7, pch = 21) +
  #   xlab("UTM (km)") +
  #   ylab("UTM (km)") +
  #   coord_fixed()
  # # ggsave("Figures/mesh.pdf", width = 6, height = 6)

  unique(sort(d$year))
  unique(sort(d$survey_type))

  fit <- sdmTMB(
    formula =
    catch_count ~ poly(log_botdepth, 2) + as.factor(survey_type),
    #catch_count ~ poly(log_botdepth, 2) , #hbll n model, hbll s model
    data = d,
    time = "year",
    offset = "offset",
    mesh = mesh,
    spatial = "on",
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
  # fitjul <- update(fit, formula = catch_count ~ poly(log_botdepth, 2) + as.factor(survey_type) + poly(julian, 2))
   fitmonth <- update(fit, formula = catch_count ~ poly(log_botdepth, 2) + as.factor(survey_type) + poly(month, 2))
   sanity(fitmonth)
   fitmonth$sd_report
   # saveRDS(fitmonth, file = "output/fit-sog-hblldog-month.rds")
   fitmonth <- readRDS(file = "output/fit-sog-hblldog-month.rds")

   pred <- predict(fitmonth, grid, return_tmb_object = TRUE, response = TRUE)
   index <- get_index(pred, bias_correct = TRUE)
   saveRDS(index, file = "output/ind-sog-hblldog-month.rds")

   saveRDS(fit, file = path)
  # saveRDS(fitjul, file = "output/fit-sog-hblldog-julian.rds")

  pred <- predict(fit, grid, return_tmb_object = TRUE, response = TRUE)
  index <- get_index(pred, bias_correct = TRUE)

  # fit <- readRDS(file = path)

  index <- index |> mutate(model = ifelse(year %in% unique(d$year), "yrs_surved", "yrs_interp"))
  saveRDS(index, file = pathind)

  yearlabs <- as.list(index |> filter(model == "yrs_surved") |> reframe(year = year))
  yearlabs <- yearlabs$year
    #c(1986, 1989, 2005, 2008, 2009, 2011, 2013, 2014, 2015, 2018, 2019, 2021, 2022, 2023)

  ggplot(index, aes(year, est / 10000)) +
    geom_line(col = "#8D9999") +
    geom_point(col = "#8D9999") +
    geom_ribbon(aes(ymin = lwr / 10000, ymax = upr / 10000), alpha = 0.4, fill = "#8D9999") +
    theme_classic() +
    scale_x_continuous(breaks = c(years))

  ggplot(index, aes(year, est, ymin = lwr, ymax = upr)) +
    # geom_pointrange(data = filter(index, model == "yrs_interp"), mapping = aes(x = year - 0.25), size = 0.2, pch = 5, colour = "grey40", alpha = 0.6) +
    geom_pointrange(data = filter(index, model == "yrs_surved"), mapping = aes(x = year - 0.25), size = 0.2, pch = 5, colour = "red", alpha = 0.6) +
    theme_classic() +
    scale_x_continuous(breaks = c(yearlabs)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))


}
