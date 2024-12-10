# notes
# 2004 is missing deployment times and therefore I can't calculate soak time
# soak times were between 1.5-3 hours in 2004 so I don't know how to account for that

#models are int only and include depth
#no model includes julian date
#hbll models don't include work done during dog survey
#dog models don't include work done during HBLL survey



# load library and params----------------------------------------------

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

# load data----------------------------------------------

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
    depth_m > 165 & depth_m <= 220 ~ 4,
    depth_m > 220 ~ 5
  ))

grid <- readRDS("output/prediction-grid-hbll-n-s-dog-2-km.rds") # ("output/prediction-grid-hbll-n-s-dog-1-km.rds")
grid$log_botdepth2 <- grid$log_botdepth * grid$log_botdepth
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
    depth_m > 165 & depth_m <= 220 ~ 4,
    depth_m > 220 ~ 5
  ))



# run index ---------------------------------------------------------------

# params
# model <- "hblldog_no2004"
 model = "hbll-n-s"
# model = "hbll-n"
# model = "hbll-s"
# model <- "dog" #<- use nbinom2
# model <- "dogcircle" #<- use nbinom2

if (model == "hblldog_no2004") { #<- everything except for dogfish comp work in 2004
  d <- df #<- 2004 was removed above with the offset can't be na.
  years <- seq(min(d$year), 2023, 1)
  grid <- purrr::map_dfr(years, ~ tibble(grid, year = .x))
  formula <- catch_count ~ 1 + as.factor(survey_lumped)
  formuladepth <- catch_count ~ log_botdepth + log_botdepth2 + as.factor(survey_lumped)
}

if (model == "hbll-n-s") {
  d <- df |>
    filter(survey_lumped %in% c("hbll")) |> # should I removed survey_abbrev = other?
    drop_na(offset) |>
    drop_na(depth_m) |>
    drop_na(julian) |>
    drop_na(catch_count)

  grid <- readRDS("output/prediction-grid-hbll-n-s.rds") |>
    filter(area %in% c("hbll_s", "hbll_n")) # from gfdata HBLL n and south merged
  # grid <- readRDS(paste0("output/prediction-grid-hbll-n-s-dog-", "0.5", "-km.rds"))
  # range(grid$bot_depth)  #<- does this grid (smaller grid size) reduce the error bars??
  years <- seq(min(d$year), 2023, 1)
  grid <- purrr::map_dfr(years, ~ tibble(grid, year = .x))
  grid$log_botdepth2 <- grid$log_botdepth * grid$log_botdepth
  formula <- catch_count ~ 1
  formuladepth <- catch_count ~ log_botdepth + log_botdepth2
}

if (model == "hbll-s") {
  d <- df |>
    filter(survey_sep %in% c("HBLL INS S")) |> # should survey_abbrev != other?
    drop_na(offset) |>
    drop_na(depth_m) |>
    drop_na(catch_count)
  grid <- readRDS("output/prediction-grid-hbll-n-s.rds") |>
    filter(area %in% c("hbll_s")) # from gfdata HBLL n and south merged
  grid$log_botdepth2 <- grid$log_botdepth * grid$log_botdepth
  years <- seq(min(d$year), max(d$year), 1)
  grid <- purrr::map_dfr(years, ~ tibble(grid, year = .x))
  formula <- catch_count ~ 1
  formuladepth <- catch_count ~ log_botdepth + log_botdepth2
}

if (model == "hbll-n") {
  d <- df |>
    filter(survey_sep %in% c("HBLL INS N")) |> # should survey_abbrev != other?
    drop_na(offset) |>
    drop_na(depth_m) |>
    drop_na(catch_count)
  grid <- readRDS("output/prediction-grid-hbll-n-s.rds") |>
    filter(area %in% c("hbll_n")) # from gfdata HBLL n and south merged
  grid$log_botdepth2 <- grid$log_botdepth * grid$log_botdepth
  years <- seq(min(d$year), 2023, 1)
  grid <- purrr::map_dfr(years, ~ tibble(grid, year = .x))
  formula <- catch_count ~ 1
  formuladepth <- catch_count ~ log_botdepth + log_botdepth2
}

if (model == "dog") {
  d <- df |>
    filter(survey_sep %in% c("dog comp", "dog", "dog-jhook")) |>
    filter(month > 9) # i did this to drop the comp work that happened in summer, keep in and include julian if wanted
  years <- seq(min(d$year), max(d$year), 1)
  grid <- purrr::map_dfr(years, ~ tibble(grid, year = .x))
  grid <- grid |> filter(latitude < max(d$latitude))
  grid$survey_lumped <- "dog"
  formula <- catch_count ~ 1 + as.factor(survey_lumped)
  formuladepth <- catch_count ~ log_botdepth + log_botdepth2 + as.factor(survey_lumped)
}

# if (model == "hblldog_nojhook") {
#   d <- df |>
#     filter(!year %in% c(1986, 1989)) |> #<- dog comp is already rm from db
#     filter(survey_sep != "dog-jhook") # |>
#   years <- seq(min(d$year), 2023, 1)
#   grid <- purrr::map_dfr(years, ~ tibble(grid, year = .x))
#   formula <- catch_count ~ 1 + as.factor(survey_lumped)
#   # formula = catch_count ~ poly(log_botdepth, 2) + as.factor(survey_lumped)
# }

# index generation--------
# create the mesh
if (model %in% c("hblldog_no2004", "hbll-n-s", "hbll-n", "hbll-s")) {
  cutoff <- 3 #<- does this work for the int only model? try 5 if no
} else {
  if (model %in% c("dog")) {
    cutoff <- 10
  }
}

mesh <- make_mesh(d, c("UTM.lon", "UTM.lat"), cutoff = cutoff)
plot(mesh)
mesh$mesh$n

#<- should hbll-n be on?
if (model %in% c("hbll-s", "hbll-n", "dog")) {
  spatial <- "off"
} else {
  spatial <- "on"
}

if (model %in% c("hbll-n", "hbll-s")) {
  priorsint <- sdmTMBpriors(
    b = normal(c(NA), c(NA)),
    matern_st = pc_matern(range_gt = cutoff * 3, sigma_lt = 2),
    matern_s = pc_matern(range_gt = cutoff * 2, sigma_lt = 1))
  priors <- sdmTMBpriors(
    b = normal(c(NA, 0, 0), c(NA, 1, 1)),
    matern_st = pc_matern(range_gt = cutoff * 3, sigma_lt = 2),
    matern_s = pc_matern(range_gt = cutoff * 2, sigma_lt = 1)
  )
} else {
  if (model == "dog") { #<- two surveys
    priorsint <- sdmTMBpriors(b = normal(c(NA, NA), c(NA, NA)))
    priors <-  sdmTMBpriors(
      matern_st = pc_matern(range_gt = cutoff * 3, sigma_lt = 2),
      matern_s = pc_matern(range_gt = cutoff * 2, sigma_lt = 1),
      b = normal(c(NA, 0, 0, 0), c(NA, 1, 1, 1)))
  } else {
    if (model == "hblldog_no2004") { #<- three surveys
      priorsint <- sdmTMBpriors(b = normal(c(NA, NA, NA), c(NA, NA, NA)))
      priors <- sdmTMBpriors(b = normal(c(NA, 0, 0, NA, NA), c(NA, 1, 1, NA, NA)))
    }
  }
}

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
ggsave(paste0("Figures/sog-model-mesh-", model, ".pdf"), width = 6, height = 6)

# get the extra time paramater
year_range <- range(d$year)
all_years <- data.frame(year = year_range[1]:year_range[2])
missing_years <- anti_join(all_years, d, by = "year") %>%
  select(year) %>%
  unique()
extratime <- missing_years$year
if (length(extratime) == 0L) extratime <- NULL

# run model
fit <- sdmTMB(
  formula = formula,
  data = d,
  time = "year",
  offset = "offset",
  mesh = mesh,
  spatial = "on",
  spatiotemporal = "rw",
  #family = nbinom2(), #<- for dog has a lower aic value
  family = delta_gamma(),
  silent = TRUE,
  share_range = FALSE,
  extra_time = extratime,
  control = sdmTMBcontrol(newton_loops = 1L),
  priors = priorsint
  # do_index = TRUE,
  # predict_args = list(newdata = grid),
  # index_args = list(area = grid$area_km2)
)

fit$sd_report
sanity(fit)
AIC(fit)

# generate indices with and w/out depth
# fitdepth <- update(fit, formula = formuladepth) # why doesn't this work?
fitdepth <- sdmTMB(
  formula = formuladepth,
  data = d,
  time = "year",
  offset = "offset",
  mesh = mesh,
  spatial = spatial,
  spatiotemporal = "rw",
  family = delta_gamma(),
  #family = nbinom2(), #<- for dog
  silent = TRUE,
  priors = priors,
  share_range = FALSE,
  extra_time = extratime,
  control = sdmTMBcontrol(newton_loops = 1L)
)

fitdepth$sd_report
sanity(fitdepth)
AIC(fitdepth)

if (AIC(fit) < AIC(fitdepth)) {
  print("intercept model has lower AIC")
  print(paste0(AIC(fit), " ", AIC(fitdepth)))
} else {
  if (AIC(fit) > AIC(fitdepth)) {
    print("depth model has lower AIC")
  }
  print(paste0(AIC(fit), " ", AIC(fitdepth)))
}

ok <- all(unlist(sanity(fit)))
if (!ok) { # does this save it if it converged?
  # if (!exists("fit")) {
  fit <- NULL
} else {
  saveRDS(fit, file = paste0("output/fit-sog-intonly", model, ".rds"))
}

ok <- all(unlist(sanity(fitdepth)))
if (!ok) { # does this save it if it converged?
  # if (!exists("fitdepth")) {
  fitdepth <- NULL
} else {
  saveRDS(fitdepth, file = paste0("output/fit-sog-depth", model, ".rds"))
}

# check if NB mdoel fit is better
# ok <- all(unlist(sanity(fit)))
# if (!ok) {
#   try({
# fitnb <- sdmTMB(
#   formula = formula,
#   data = d,
#   time = "year",
#   offset = "offset",
#   mesh = mesh,
#   spatial = "off",
#   spatiotemporal = "rw",
#   family = nbinom2(),
#   silent = TRUE,
#   share_range = FALSE,
#   extra_time = extratime,
#   control = sdmTMBcontrol(newton_loops = 1L)
# )
#   })
# }
# AIC(fit)
# AIC(fitnb)
# AIC(fitdepth)

# get index
if (!is.null(fit)) { #<- #if fit is null ignore this
  pred <- predict(fit, grid, return_tmb_object = TRUE, response = TRUE)
  index <- get_index(pred, bias_correct = TRUE)
  index <- index |> mutate(model = ifelse(year %in% unique(d$year), "yrs_surved", "yrs_interp"))
  index$modelloc <- model
  index$type <- "int"
  saveRDS(index, file = paste0("output/ind-sog-intonly", model, ".rds"))
}

if (!is.null(fitdepth)) {
  grid$survey_lumped <- "dog"
  pred <- predict(fitdepth, grid, return_tmb_object = TRUE, response = TRUE)
  index <- get_index(pred, bias_correct = TRUE)
  index <- index |> mutate(model = ifelse(year %in% unique(d$year), "yrs_surved", "yrs_interp"))
  index$modelloc <- model
  index$type <- "depth"
  saveRDS(index, file = paste0("output/ind-sog-depth", model, ".rds"))
}

# high res grid, runs out of memory so chunked it up
# if (!is.null(fitdepth)){
#   pred <- predict(fitdepth, filter(grid, year %in% c(2003:2013)), return_tmb_object = TRUE, response = TRUE)
#   index <- get_index(pred, bias_correct = TRUE)
#   pred2 <- predict(fitdepth, filter(grid, year %in% c(2014:2023)), return_tmb_object = TRUE, response = TRUE)
#   index2 <- get_index(pred2, bias_correct = TRUE)
#   index <- bind_rows(index, index2)
#   index <- index |> mutate(model = ifelse(year %in% unique(d$year), "yrs_surved", "yrs_interp"))
#   index$modelloc <- model
#   index$type <- "depth-highres"
#   saveRDS(index, file = paste0("output/ind-sog-depth-", model, "-highres.rds"))
# }

