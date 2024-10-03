# notes
# 2004 is missing deployment times and therefore I can't calculate soak time
# soak times were between 1.5-3 hours
#needs cleaning

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
    depth_m > 165 & depth_m <= 220  ~ 4,
    depth_m > 220 ~ 5 ))

grid <- readRDS("output/prediction-grid-hbll-n-s-dog-2-km.rds") # ("output/prediction-grid-hbll-n-s-dog-1-km.rds")

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
    depth_m > 165 & depth_m <= 220  ~ 4,
    depth_m > 220 ~ 5 ))


#params
model <- "hblldog_no2004"

if (model == "hblldog_no2004") { #<- everything except for dogfish comp work in 2004
  d <- df #<- 2004 was removed above with the offset can't be na.
  years <- seq(min(d$year), 2023, 1)
  grid <- purrr::map_dfr(years, ~ tibble(grid, year = .x))
  #formula = catch_count ~ 1 + as.factor(survey_lumped)
  formula = catch_count ~ poly(log_botdepth, 2) + as.factor(survey_lumped)
  formula_julian_int = catch_count ~ poly(log_botdepth, 2) * poly(julian_c, 2) + as.factor(survey_lumped)
  formula_julian = catch_count ~ poly(log_botdepth, 2) + poly(julian_c, 2) + as.factor(survey_lumped)
  formula_month = catch_count ~ poly(log_botdepth, 2) * month + as.factor(survey_lumped)
  formula_depthbin = catch_count ~ depth_bin * poly(julian,2) + as.factor(survey_lumped)
  #spatial = "off" #doesnt coverge otherwise
}


# # index generation function--------

indexfunc <- function(d) {
  mesh <- make_mesh(d, c("UTM.lon", "UTM.lat"), cutoff = 5)
  # mesh <- make_mesh(d, c("UTM.lon", "UTM.lat"), cutoff = 2) #hbll s only
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
  ggsave(paste0("Figures/sog-model-mesh-", model, ".pdf"), width = 6, height = 6)

  # ggplot() +
  #   inlabru::gg(mesh$mesh) +
  #   geom_point(data = d, aes(UTM.lon, UTM.lat), size = 0.5, alpha = 0.7, pch = 21) +
  #   xlab("UTM (km)") +
  #   ylab("UTM (km)") +
  #   coord_fixed()
  # # ggsave("Figures/mesh.pdf", width = 6, height = 6)

  unique(sort(d$year))
  unique(sort(d$survey_lumped))

  fit <- sdmTMB(
    formula = formula,
    #formula = catch_count ~ 1,
    data = d,
    time = "year",
    offset = "offset",
    mesh = mesh,
    spatial = spatial,
    spatiotemporal = "rw",
    #family = nbinom2(),
    family = delta_gamma(),
    silent = FALSE,
    share_range = FALSE,
    extra_time = extratime
    # do_index = TRUE,
    # predict_args = list(newdata = grid),
    # index_args = list(area = grid$area_km2)
  )

  fit$sd_report
  sanity(fit)
  ok <- all(unlist(sanity(fit)))
  saveRDS(fit, file = paste0("output/fit-sog-", model, ".rds"))
  AIC(fit)
  pred <- predict(fit, grid, return_tmb_object = TRUE, response = TRUE)
  index <- get_index(pred, bias_correct = TRUE)
  index <- index |> mutate(model = ifelse(year %in% unique(d$year), "yrs_surved", "yrs_interp"))
  saveRDS(index, file = paste0("output/ind-sog-", model, ".rds"))

  fitjulian2 <- update(fit, formula = formula_julian_int)
  sanity(fitjulian2)
  fitjulian2$sd_report
  saveRDS(fitjulian2, file = paste0("output/fit-sog-", model, "-julian-interaction.rds"))

  pred <- predict(fitjulian2, grid, return_tmb_object = TRUE, response = TRUE)
  index <- get_index(pred, bias_correct = TRUE)
  index <- index |> mutate(model = ifelse(year %in% unique(d$year), "yrs_surved", "yrs_interp"))
  saveRDS(index, file = paste0("output/ind-sog-", model, "-julian-interaction.rds"))

  fitjulian <- update(fit, formula = formula_julian)
  sanity(fitjulian)
  fitjulian$sd_report
  saveRDS(fitjulian, file = paste0("output/fit-sog-", model, "-julian.rds"))

  pred <- predict(fitjulian, grid, return_tmb_object = TRUE, response = TRUE)
  index <- get_index(pred, bias_correct = TRUE)
  index <- index |> mutate(model = ifelse(year %in% unique(d$year), "yrs_surved", "yrs_interp"))
  saveRDS(index, file = paste0("output/ind-sog-", model, "-month.rds"))

  fitmonth <- update(fit, formula = formula_month)
  sanity(fitmonth)
  fitmonth$sd_report
  saveRDS(fitmonth, file = paste0("output/fit-sog-", model, "-month-int.rds"))

  pred <- predict(fitmonth, grid, return_tmb_object = TRUE, response = TRUE)
  index <- get_index(pred, bias_correct = TRUE)
  index <- index |> mutate(model = ifelse(year %in% unique(d$year), "yrs_surved", "yrs_interp"))
  saveRDS(index, file = paste0("output/ind-sog-", model, "-month.rds"))

  fitdepthbin<- update(fit, formula = formula_depthbin)
  sanity(fitdepthbin)
  fitdepthbin$sd_report
  saveRDS(fitdepthbin, file = paste0("output/fit-sog-", model, "-depthbin-julian-int.rds"))

  pred <- predict(fitdepthbin, grid, return_tmb_object = TRUE, response = TRUE)
  index <- get_index(pred, bias_correct = TRUE)
  index <- index |> mutate(model = ifelse(year %in% unique(d$year), "yrs_surved", "yrs_interp"))
  saveRDS(index, file = paste0("output/ind-sog-", model, "-depthbin-julian-int.rds"))

  yearlabs <- as.list(index |> filter(model == "yrs_surved") |> reframe(year = year))
  yearlabs <- yearlabs$year

  ggplot(index, aes(year, (est))) +
    geom_line(col = "#8D9999") +
    geom_point(col = "#8D9999") +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.4, fill = "#8D9999") +
    theme_classic() +
    scale_x_continuous(breaks = c(years))

  ggplot(index, aes(year, (est), ymin = (lwr), ymax = (upr))) +
    # geom_pointrange(data = filter(index, model == "yrs_interp"), mapping = aes(x = year - 0.25), size = 0.2, pch = 5, colour = "grey40", alpha = 0.6) +
    geom_pointrange(data = filter(index, model == "yrs_surved"), mapping = aes(x = year - 0.25), size = 0.2, pch = 5, colour = "red", alpha = 0.6) +
    theme_classic()

  ggplot(index, aes(year, log(est), ymin = log(lwr), ymax = log(upr))) +
    # geom_pointrange(data = filter(index, model == "yrs_interp"), mapping = aes(x = year - 0.25), size = 0.2, pch = 5, colour = "grey40", alpha = 0.6) +
    geom_pointrange(data = filter(index, model == "yrs_surved"), mapping = aes(x = year - 0.25), size = 0.2, pch = 5, colour = "red", alpha = 0.6) +
    theme_classic() +
    scale_x_continuous(breaks = c(yearlabs)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}


# run functions -----------------------------------------------------------

indexfunc(d)
