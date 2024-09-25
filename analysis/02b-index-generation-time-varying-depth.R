library(sdmTMB)
library(ggplot2)
library(tidyverse)
library(sdmTMB)

latitude_cutoff <- 50.34056
bccrs <- 32609
# loc = "HBLL INS N"
# loc = "HBLL INS S"

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

# hbll n and s only -------------------------------------------------------------
d <- df |>
  #filter(survey_abbrev %in% c("HBLL INS S", "HBLL INS N")) |>
  filter(survey_lumped == "hbll") |>
  drop_na(offset) |>
  drop_na(depth_m) |>
  drop_na(julian) |>
  drop_na(catch_count)
grid <- readRDS("output/prediction-grid-hbll-n-s.rds") |>
  filter(area %in% c("hbll_S", "hbll_n")) # from gfdata HBLL n and south merged
years <- seq(min(d$year), 2023, 1)
sort(unique(d$year))
# grid <- purrr::map_dfr(unique(d$year), ~ tibble(grid, year = .x))
grid <- purrr::map_dfr(years, ~ tibble(grid, year = .x))
grid$survey_type <- "hbll"
grid$julian <- mean(d$julian)
model = "hbll-n-s"
formula = catch_count ~ poly(log_botdepth, 2)
formula_depthbin= catch_count ~ depth_bin * poly(julian,2)
sort(unique(d$year))
extratime <- c(2006, 2017, 2020)
spatial = "on"



# time varying model ------------------------------------------------------

mesh <- make_mesh(d, c("UTM.lon", "UTM.lat"), cutoff = 5)
plot(mesh)

fittv <- sdmTMB(
  formula = catch_count ~ 0 + as.factor(year),
  offset = "offset",
  time = "year",
  time_varying = ~ 0 + log_botdepth + log_botdepth2,
  #time_varying_type = "rw0",
  spatiotemporal = "AR1", #off or AR1 left over variation from the time varying process is ar1
  silent = FALSE,
  spatial = "on", #or on
  family = nbinom2(),
  mesh = mesh,
  data = d,
  do_index = FALSE,
  extra_time = c(2006, 2017, 2020)
)

sanity(fittv)
fittv$sd_report
saveRDS(fittv, paste0("output/fittv-", model, ".rds"))

# time varying depth plot
nd <- expand.grid(
  log_botdepth =
    seq(min(d$log_botdepth), max(d$log_botdepth), length.out = 500)
)
nd$log_botdepth2 <- nd$log_botdepth^2
nd$offset <- 0
# nd$survey_name <- "GOA"
# nd$year <- 2021L # L: integer to match original data

year <- unique(d$year)
nd <- purrr::map_dfr(year, function(.x) {
  dplyr::mutate(nd, year = .x)
})

p <- predict(fittv, newdata = nd, se_fit = TRUE, re_form = NA)
#p$depth_m <- exp(p$log_botdepth_cent + mean) * -1
p$depth_m <- exp(p$log_botdepth)
p$mean_logbot_depth <- mean
saveRDS(p, paste0("output/indtv-", model, ".rds"))

p |>
  ggplot() +
  geom_line(aes((depth_m ), exp(est), group = as.factor(year), colour = year)) +
  scale_colour_viridis_c() +
  theme_classic()
ggsave("Figures/fit-tv-predicteddepth.jpg", width = 4, height = 3)

p %>%
  group_by(year) %>%
  summarize(max_est = max(est), xint = (depth_m[est == max_est])) |>
  # filter(year > 2003) |>
  ggplot() +
  theme_classic() +
  geom_line(aes(year, xint, colour = year, group = year)) +
  geom_point(aes(year, xint, colour = year, group = year))

ggplot(p, aes(depth_m, exp(est),
              ymin = exp(est - 1.96 * est_se),
              ymax = exp(est + 1.96 * est_se),
              group = as.factor(year)
)) +
  geom_line(aes(colour = year), lwd = 1) +
  geom_ribbon(aes(fill = year), alpha = 0.1) +
  scale_colour_viridis_c() +
  scale_fill_viridis_c() +
  scale_x_continuous(labels = function(x) round(exp(x * pcod$depth_sd[1] + pcod$depth_mean[1]))) +
  coord_cartesian(expand = F) +
  labs(x = "Depth (m)", y = "Biomass density (kg/km2)")
