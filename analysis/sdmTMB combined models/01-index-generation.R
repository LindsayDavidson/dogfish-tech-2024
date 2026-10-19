# notes
# soak times were between 1.5-3 hours in 2004

library(ggplot2)
library(tidyverse)
#remotes::install_github("pbs-assess/sdmTMB@dev", force = TRUE)
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
df$catch_prop <- df$catch_count / df$lglsp_hook_count
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

# run index ---------------------------------------------------------------

cutoff <- 15

# model = "hblldog" #hbll and circle and j hook
# model = "hbll-n-s"
 model <- "dog-j-circle" #circle and j hook sog surveys

if (model == "hblldog") {
  d <- df #<- check if 2004 is here now as I added soak times
  sort(unique(d$year))
  years <- seq(min(d$year), max(d$year), 1)

  ggplot(d, aes(year, catch_prop)) +
    geom_point() +
    facet_wrap(~survey_lumped)

  family <- betabinomial(link = "cloglog")

  #source("analysis/999-load-grid-data.R") # this is the hbll n, s, and dogfish grid
  source("analysis/999-load-hbll-n-s-grid.R") #this is the hbll n, s grid from gfdata
  grid <- purrr::map_dfr(years, ~ tibble(grid, year = .x))
  grid$julian_c <- 0
  grid$survey_lumped <- "hbll"

  weights <- (d$lglsp_hook_count * d$soak)
  formula <- catch_prop ~ 1 + as.factor(survey_lumped) + poly(julian_c, 2)


}

if (model == "hbll-n-s") {
  d <- df |>
    filter(survey_lumped %in% c("hbll")) |>
    filter(survey_abbrev != "OTHER") |> #no comparative work, therefore no need for julian but could try
    drop_na(offset) |>
    drop_na(depth_m) |>
    drop_na(julian) |>
    drop_na(catch_count)

  weights <- (d$lglsp_hook_count * d$soak)

  # source("analysis/999-load-grid-data.R") #this is the hbll n, s, and dogfish grid
  source("analysis/999-load-hbll-n-s-grid.R") # this is the hbll n, s grid from gfdata
  years <- seq(min(d$year), max(d$year), 1)
  grid <- purrr::map_dfr(years, ~ tibble(grid, year = .x))
  grid$survey_lumped <- "hbll"
  formula <- catch_prop ~ 1
  #formuladepth <- catch_prop ~ 1 + s(julian_c)
  family <- betabinomial(link = "cloglog")
}

if (model == "dog-j-circle") {
  d <- df |>
    filter(survey_sep %in% c("dog", "dog comp")) |>
    filter(year %in% c(2005, 2008, 2011, 2014, 2019, 2023)) |> #want all years
    mutate(HBLLcomp = ifelse(month == 9 & day < 27, "erase", "keep")) |>
    filter(HBLLcomp == "keep") |> #dont want hbll in this one
    # filter(month > 9) |> # i did this to drop the comp work that happened in summer, keep in and include julian if wanted
    drop_na(offset) |>
    drop_na(depth_m) |>
    drop_na(julian) |>
    drop_na(catch_count)

  grid <- readRDS("data-raw/dogfish_sets.rds") |>
    filter(YEAR %in% 1986) |>
    dplyr::select(LATITUDE, LONGITUDE, FE_FISHING_GROUND_COMMENT, GROUPING_DESC) |>
    filter(GROUPING_DESC != "SoG Dogfish 0 - 55 m") |>
    distinct(.keep_all = TRUE) |>
    add_utm_columns(
      ll_names = c("LONGITUDE", "LATITUDE"),
      utm_names = c("UTM.lon", "UTM.lat"),
      utm_crs = 32609
    ) |>
    mutate(UTM.lon.m = UTM.lon * 1000, UTM.lat.m = UTM.lat * 1000)

  weights <- (d$lglsp_hook_count * d$soak)
  years <- seq(min(d$year), max(d$year), 1)
  spatial <- "off"

  years <- seq(min(d$year), max(d$year), 1)
  grid <- purrr::map_dfr(years, ~ tibble(grid, year = .x))
  grid$survey_lumped <- "dog"

  formula <- catch_prop ~ 1
  formuladepth <- catch_prop ~ as.factor(depth_bin)
  family <- betabinomial(link = "cloglog")

  cutoff <- 10
}


# index generation--------

# create the
library(sdmTMB)
mesh <- make_mesh(d, c("UTM.lon", "UTM.lat"), cutoff = cutoff)
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
#ggsave(paste0("Figures/sog-model-mesh-", model, ".pdf"), width = 6, height = 6)


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

ggplot() +
  geom_point(
    data = d |> arrange(year), aes(month, catch_prop),
    size = 1,
    # ), size = 0.25, alpha = 0.25,
    pch = 16
  ) +
  scale_colour_viridis_c(direction = -1) +
  facet_grid(~survey_lumped)


# run model
if (model %in% c("hbll-n-s", "hblldog")) {
  fit <- sdmTMB(
    formula = formula,
    data = d,
    time = "year",
    mesh = mesh,
    spatial = "on",
    spatiotemporal = "rw",
    family = betabinomial(),
    weights = weights,
    silent = TRUE,
    share_range = FALSE,
    extra_time = extratime,
    control = sdmTMBcontrol(newton_loops = 1L)
  )

  } else {
  fit <- sdmTMB(
    formula = formula,
    data = d,
    time = "year",
    mesh = mesh,
    spatial = spatial,
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

ok <- all(unlist(sanity(fit)))

if (!ok) {
  # if (!exists("fit")) {
  fit <- NULL
} else {
  saveRDS(fit, file = paste0("output/fit-intonly", model, ".rds"))
}

# get index
if (!is.null(fit)) {
  pred <- predict(fit, grid, return_tmb_object = TRUE, response = TRUE)
  index <- get_index(pred, area = 1, bias_correct = TRUE)
  index <- index |> mutate(model = ifelse(year %in% unique(d$year), "yrs_surved", "yrs_interp"))
  index$modelloc <- model
  index$type <- "int"
  saveRDS(index, file = paste0("output/ind-sog-intonly", model, ".rds"))
}

ggplot(index, aes(as.factor(year), (est), ymin = (lwr), ymax = (upr))) +
  geom_pointrange(position = position_dodge(width = 0.25)) +
  theme_classic()
