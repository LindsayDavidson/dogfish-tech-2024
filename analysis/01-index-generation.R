# notes
# 2004 is missing deployment times and therefore I can't calculate soak time
# soak times were between 1.5-3 hours in 2004 so I don't know how to account for that

#models are int only and include depth
#no model includes julian date
#hbll models don't include work done during dog survey
#dog models don't include work done during HBLL survey


library(ggplot2)
library(tidyverse)
remotes::install_github("pbs-assess/sdmTMB@dev", force = TRUE)
library(sdmTMB)


bccrs <- 32609

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
df$catch_prop <- df$catch_count/df$lglsp_hook_count
#weights <- df$lglsp_hook_count #j hooks needs the soak time included as it is variable, does that mess this up??
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

source("analysis/999-load-grid-data.R") #this is the hbll n, s, and dogfish grid


# run index ---------------------------------------------------------------

# params
# model = "hblldog_no2004"
# model = "hbll-n-s"
# model = "dog"
model = "dog-circle"

if (model == "hblldog_no2004") { #<- everything except for dogfish comp work in 2004
  d <- df #<- 2004 was removed above with the offset can't be na.
  years <- seq(min(d$year), 2023, 1)
  #take out comparative work?? that happened in the different season??
  remove <- filter(d, season == 4 & survey_lumped == "hbll")
  remove1 <- filter(d, season == 3 & survey_lumped == "hbll" & survey_abbrev == "OTHER")
  remove2 <- filter(d, season == 3 & survey_lumped == "dog")
  remove <- bind_rows(remove, remove1, remove2)
  d <- d |> filter(!fishing_event_id  %in% c(remove$fishing_event_id))
  ggplot(d, aes(year, catch_prop)) + geom_point() + facet_wrap(~survey_lumped)
  family = betabinomial(link = "cloglog")
  grid <- purrr::map_dfr(years, ~ tibble(grid, year = .x))
  weights <- (d$lglsp_hook_count * d$soak)
  formula <- catch_prop ~ 1 + as.factor(survey_lumped) #<- survey_lumped should just be dog, dogjhook, and hbll
  formuladepth <- catch_prop ~ log_botdepth + log_botdepth2 + as.factor(survey_lumped)
  grid$survey_lumped <- "hbll"
}

if (model == "hbll-n-s") {
  d <- df |>
    filter(survey_lumped %in% c("hbll")) |>
    filter(survey_abbrev != "OTHER") |> # should I removed survey_abbrev = other? yes? not fished ast hbll areas
    drop_na(offset) |>
    drop_na(depth_m) |>
    drop_na(julian) |>
    drop_na(catch_count)

  # remove <- filter(d, season == 4 & survey_lumped == "hbll")
  # remove1 <- filter(d, season == 3 & survey_lumped == "hbll" & survey_abbrev == "OTHER")
  # d <- d |> filter(!fishing_event_id  %in% c(remove$fishing_event_id))

  weights <- (d$lglsp_hook_count * d$soak)
  rm(grid)
  grid <- readRDS("output/prediction-grid-hbll-n-s.rds") |>
    filter(area %in% c("hbll_s", "hbll_n")) # from gfdata HBLL n and south merged
  # grid <- readRDS(paste0("output/prediction-grid-hbll-n-s-dog-", "0.5", "-km.rds"))
  # range(grid$bot_depth)  #<- does this grid (smaller grid size) reduce the error bars??
  years <- seq(min(d$year), 2023, 1)
  grid <- purrr::map_dfr(years, ~ tibble(grid, year = .x))
  grid$log_botdepth2 <- grid$log_botdepth * grid$log_botdepth
  formula <- catch_prop ~ 1
  formuladepth <- catch_prop ~ log_botdepth + log_botdepth2
  family = betabinomial(link = "cloglog")
}

if (model == "dog") {
  d <- df |>
    filter(survey_sep %in% c("dog comp", "dog", "dog-jhook")) |>
    filter(month > 9) |> # i did this to drop the comp work that happened in summer, keep in and include julian if wanted
    drop_na(offset) |>
    drop_na(depth_m) |>
    drop_na(julian) |>
    drop_na(catch_count)

  years <- seq(min(d$year), max(d$year), 1)
  grid <- purrr::map_dfr(years, ~ tibble(grid, year = .x))
  grid <- grid |> filter(latitude < max(d$latitude))
  grid$survey_lumped <- "dog"

  weights <- (d$lglsp_hook_count * d$soak)

  formula <- catch_prop ~ 1 + as.factor(survey_lumped)
  formuladepth <- catch_prop ~ log_botdepth + log_botdepth2 + as.factor(survey_lumped)
  family = betabinomial(link = "cloglog")
}





# index generation--------

# create the mesh
cutoff <- 10
mesh <- make_mesh(d, c("UTM.lon", "UTM.lat"), cutoff = cutoff)
plot(mesh)
mesh$mesh$n

if (model == "dog") { #<- two surveys
    priorsint <- sdmTMBpriors(b = normal(c(NA, NA), c(NA, NA)),
         matern_st = pc_matern(range_gt = cutoff * 3, sigma_lt = 2),
         matern_s = pc_matern(range_gt = cutoff * 2, sigma_lt = 1))
    priors <-  sdmTMBpriors(
      matern_st = pc_matern(range_gt = cutoff * 3, sigma_lt = 2),
      matern_s = pc_matern(range_gt = cutoff * 2, sigma_lt = 1),
      b = normal(c(NA, 0, 0, 0), c(NA, 1, 1, 1)))

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

# get the extra time parameter
year_range <- range(d$year)
all_years <- data.frame(year = year_range[1]:year_range[2])
missing_years <- anti_join(all_years, d, by = "year") %>%
  select(year) %>%
  unique()
extratime <- missing_years$year
if (length(extratime) == 0L) extratime <- NULL

ggplot() +
  geom_point(
    data = d |> arrange(year), aes(UTM.lon, UTM.lat, colour = year),
    size = 1,
    # ), size = 0.25, alpha = 0.25,
    pch = 16
  ) +
  scale_colour_viridis_c(direction = -1) +
  facet_grid(~survey_lumped)

ggplot() +
  geom_point(
    data = d |> arrange(year), aes(year, catch_prop),
    size = 1,
    # ), size = 0.25, alpha = 0.25,
    pch = 16
  ) +
  scale_colour_viridis_c(direction = -1) +
  facet_grid(~survey_lumped)

ggplot() +
  geom_point(
    data = d |> arrange(year), aes(offset, catch_prop),
    size = 1,
    # ), size = 0.25, alpha = 0.25,
    pch = 16
  ) +
  scale_colour_viridis_c(direction = -1) +
  facet_grid(~survey_lumped)

# run model
if (model %in% c("hbll-n-s", "hblldog_no2004")) {
  fit <- sdmTMB(
  formula = formula,
  data = d,
  time = "year",
  mesh = mesh,
  spatial = "on",
  spatiotemporal = "rw",
  family = family,
  weights = weights,
  silent = TRUE,
  share_range = FALSE,
  extra_time = extratime,
  control = sdmTMBcontrol(newton_loops = 1L)
)
} else{
  fit <- sdmTMB(
    formula = formula,
    data = d,
    time = "year",
    mesh = mesh,
    spatial = "on",
    spatiotemporal = "rw",
    family = family,
    weights = weights,
    silent = TRUE,
    share_range = FALSE,
    extra_time = extratime,
    control = sdmTMBcontrol(newton_loops = 1L),
    priors = priorsint
  )
}

fit$sd_report
sanity(fit)
AIC(fit)

# # generate indices with and w/out depth
# # fitdepth <- update(fit, formula = formuladepth) # why doesn't this work?
# fitdepth <- sdmTMB(
#   formula = formuladepth,
#   data = d,
#   time = "year",
#   weights = weights,
#   mesh = mesh,
#   spatial = "on", #<- try off for dog and hbll no 2004 to see if that fits
#   spatiotemporal = "rw",
#   family = family,
#   silent = TRUE,
#   #priors = priors,
#   share_range = FALSE,
#   extra_time = extratime,
#   control = sdmTMBcontrol(newton_loops = 1L)
# )
#
# fitdepth$sd_report
# sanity(fitdepth)
# AIC(fitdepth)
#
#
# #fit <- readRDS(paste0("output/fit-sog-intonly", "hblldog_no2004", ".rds"))
# #fit$model
#
#
# if (AIC(fit) < AIC(fitdepth)) {
#   print("intercept model has lower AIC")
#   print(paste0(AIC(fit), " ", AIC(fitdepth)))
# } else {
#   if (AIC(fit) > AIC(fitdepth)) {
#     print("depth model has lower AIC")
#   }
#   print(paste0(AIC(fit), " ", AIC(fitdepth)))
# }

ok <- all(unlist(sanity(fit)))
if (!ok) {
  # if (!exists("fit")) {
  fit <- NULL
} else {
  saveRDS(fit, file = paste0("output/fit-sog-intonly", model, ".rds"))
}

# ok <- all(unlist(sanity(fitdepth)))
# if (!ok) {
#   # if (!exists("fitdepth")) {
#   fitdepth <- NULL
# } else {
#   saveRDS(fitdepth, file = paste0("output/fit-sog-depth", model, ".rds"))
# }

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
  index <- get_index(pred, area = 100, bias_correct = TRUE)
  index <- index |> mutate(model = ifelse(year %in% unique(d$year), "yrs_surved", "yrs_interp"))
  index$modelloc <- model
  index$type <- "int"
  saveRDS(index, file = paste0("output/ind-sog-intonly", model, ".rds"))
}

ggplot(index, aes(as.factor(year), (est), ymin = (lwr), ymax = (upr))) +
  geom_pointrange(position = position_dodge(width = 0.25))

# if (!is.null(fitdepth)) {
#   grid$survey_lumped <- survey
#   pred <- predict(fitdepth, grid, return_tmb_object = TRUE, response = TRUE)
#   index <- get_index(pred, bias_correct = TRUE)
#   index <- index |> mutate(model = ifelse(year %in% unique(d$year), "yrs_surved", "yrs_interp"))
#   index$modelloc <- model
#   index$type <- "depth"
#   saveRDS(index, file = paste0("output/ind-sog-depth", model, ".rds"))
# }

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

