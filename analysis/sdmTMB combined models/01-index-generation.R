library(ggplot2)
library(tidyverse)
library(sdmTMB)

#choose model
# model = "hblldog" #hbll and circle and j hook
# model = "hbll-n-s" #hbll
 model <- "dog-j-circle" # circle and j hook sog surveys

df <- readRDS("data-raw/wrangled-hbll-dog-sets.rds") |>
  drop_na(catch_count) |>
  drop_na(offset) |>
  drop_na(julian)

df$log_botdepth2 <- df$log_botdepth * df$log_botdepth
df$catch_prop <- df$catch_count / (df$lglsp_hook_count * df$soak)
df$catch_prop_hbll <- df$catch_count / df$lglsp_hook_count

cutoff <- 5

if (model == "hblldog") {
  d <- df
  sort(unique(d$year))
  years <- seq(min(d$year), max(d$year), 1)

  id_remove <- d %>%
    filter(grepl("COMPARISON", activity_desc) & hooksize_desc == "12/0" & year %in% c(2023, 2024)) |>
    pull(fishing_event_id)

  d <- d |>
    filter(!fishing_event_id %in% id_remove ) |>
    drop_na(offset) |>
    drop_na(julian) |>
    drop_na(catch_count)

  meanj <- mean(d$julian)
  d$julian_c <- d$julian - meanj

  ggplot(d, aes(year, catch_prop)) +
    geom_point() +
    facet_wrap(~survey_lumped)

  family <- betabinomial(link = "logit")

  source("analysis/999-load-hbll-n-s-grid.R") # this is the hbll n, s grid from gfdata
  grid <- purrr::map_dfr(years, ~ tibble(grid, year = .x))
  grid$julian_c <- 0
  grid$survey_lumped <- "hbll"

  weight <- (d$lglsp_hook_count * d$soak)
  formulanb <- catch_count ~ 1 + as.factor(survey_lumped) + s(julian_c)
  formulabb <- catch_prop ~ 1 + as.factor(survey_lumped) + s(julian_c)

  spatial <- "off"
}

if (model == "hbll-n-s") {
  d <- df |>
    filter(survey_abbrev %in% c("HBLL INS S", "HBLL INS N")) |>
    filter(survey_abbrev != "OTHER") |> # no comparative work, therefore no need for julian but could try
    drop_na(offset) |>
    drop_na(julian) |>
    drop_na(catch_count)

  meanj <- mean(d$julian)
  d$julian_c <- d$julian - meanj

  weight <- (d$lglsp_hook_count)

  source("analysis/999-load-hbll-n-s-grid.R") # this is the hbll n, s grid from gfdata
  years <- seq(min(d$year), max(d$year), 1)
  grid <- purrr::map_dfr(years, ~ tibble(grid, year = .x))

  grid$julian_c <- 0
  formulabb <- catch_prop_hbll ~ 1
  # formula <- catch_count ~ 1
  family <- betabinomial()

  spatial <- "on"
}

if (model == "dog-j-circle") {

  d <- df |>
    filter(survey_sep %in% c("dog", "dog comp", "dog-jhook"))

  id_remove <- d %>%
    filter(grepl("COMPARISON", activity_desc) & hooksize_desc == "12/0" & year %in% c(2023, 2024)) |>
    pull(fishing_event_id)

  d <- d |>
    filter(!fishing_event_id %in% id_remove ) |>
    drop_na(offset) |>
    drop_na(julian) |>
    drop_na(catch_count)

  meanj <- mean(d$julian)
  d$julian_c <- d$julian - meanj

  grid_dog <- gfplot::dogfish_grid$grid %>%
    mutate(UTM.lon = X / 1e3, UTM.lat = Y / 1e3) |>
    distinct()

  years <- seq(min(d$year), max(d$year), 1)
  grid_dog <- replicate_df(grid_dog, "year", unique(years))
  grid_dog$julian_c <- 0
  grid_dog$survey_lumped <- "dog"
  grid <- grid_dog

  weight <- (d$lglsp_hook_count * d$soak)
  years <- seq(min(d$year), max(d$year), 1)

  spatial <- "off"

  # formula <- catch_count ~ 1 + s(julian_c)
  formulabb <- catch_prop ~ 1 + as.factor(survey_lumped)
  family <- betabinomial()
}


# index generation--------


mesh <- make_mesh(d, c("UTM.lon", "UTM.lat"), cutoff = cutoff)
plot(mesh)
mesh$mesh$n


g <-
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
  theme(legend.position = "right")
  #theme(legend.position = "inside", legend.position.inside = c(0.2, 0.25))
g
saveRDS(g, paste0("Figures/sog-model-mesh-", model, ".rds"))

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


# fit <- sdmTMB(
#     formula = formulanb,
#     data = d,
#     time = "year",
#     mesh = mesh,
#     spatial = "on", #off for hbll
#     spatiotemporal = "rw",
#     family = nbinom2(),
#     offset = d$offset_hksoak,
#     silent = TRUE,
#     share_range = FALSE,
#     extra_time = extratime,
#     control = sdmTMBcontrol(newton_loops = 1L)
#   )
#
# fit$sd_report
# sanity(fit)

fitbb <- sdmTMB(
  formula = formulabb,
  data = d,
  time = "year",
  mesh = mesh,
  spatial = spatial,
  spatiotemporal = "rw",
  weights = weight,
  family = betabinomial(),
  silent = TRUE,
  share_range = FALSE,
  extra_time = extratime,
  control = sdmTMBcontrol(newton_loops = 1L)
)

AIC(fitbb)
sanity(fitbb)
# AIC(fit)

ok <- all(unlist(sanity(fitbb)))

if (!ok) {
  # if (!exists("fit")) {
  fit <- NULL
} else {
  saveRDS(fitbb, file = paste0("output/fit-intonly", model, ".rds"))
}

# get index
if (!is.null(fitbb)) {
  pred <- predict(fitbb, grid, return_tmb_object = TRUE, response = TRUE)
  index <- get_index(pred, bias_correct = TRUE)
  index <- index |> mutate(model = ifelse(year %in% unique(d$year), "yrs_surved", "yrs_interp"))
  index$modelloc <- model
  index$type <- "int"
  saveRDS(index, file = paste0("output/ind-sog-intonly", model, ".rds"))
}

ggplot(index, aes(as.factor(year), (est), ymin = (lwr), ymax = (upr))) +
  geom_pointrange(position = position_dodge(width = 0.25)) +
  theme_classic()

ggplot(filter(index, model == "yrs_surved"), aes(as.factor(year), (est), ymin = (lwr), ymax = (upr))) +
  geom_pointrange(position = position_dodge(width = 0.25)) +
  theme_classic()
