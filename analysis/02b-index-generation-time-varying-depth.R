library(sdmTMB)
library(ggplot2)
library(tidyverse)
library(sdmTMB)

latitude_cutoff <- 50.34056
bccrs <- 32609

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
years <- seq(min(d$year), max(d$year), 1)
sort(unique(d$year))
# grid <- purrr::map_dfr(unique(d$year), ~ tibble(grid, year = .x))
grid <- purrr::map_dfr(years, ~ tibble(grid, year = .x))
grid$julian <- mean(d$julian)
model = "hbll-n-s"
formula = catch_count ~ poly(log_botdepth, 2)
#formula_depthbin= catch_count ~ depth_bin * poly(julian,2)
sort(unique(d$year))
extratime <- c(2006, 2017, 2020)
spatial = "on"



# time varying model ------------------------------------------------------

mesh <- make_mesh(d, c("UTM.lon", "UTM.lat"), cutoff = 15)
plot(mesh)

d$year_factor <- as.factor(d$year)
mean <- mean(d$log_botdepth)
d$log_botdepth_c <- d$log_botdepth - mean
d$log_botdepth_c2 <- d$log_botdepth_c * d$log_botdepth_c

fittv <- sdmTMB(
  formula = catch_count ~ 0 + year_factor, #year here to soak up the year to year variation otherwise it all goes into the depth relationship
  offset = "offset",
  time = "year",
  time_varying = ~ 1 + log_botdepth_c + log_botdepth_c2, #should this be one to have a time varying intercept for each year not much difference when 0 or 1
  time_varying_type = "rw0",
  spatiotemporal = "AR1", #off or AR1 left over variation from the time varying process is ar1
  silent = FALSE,
  spatial = "on",
  family = nbinom2(), #<- couldnt get delta_gamma to converge for now
  mesh = mesh,
  data = d,
  do_index = FALSE,
  extra_time = c(2006, 2017, 2020),
  priors = sdmTMBpriors(
  b = normal(c(0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
    c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)),
  matern_st = pc_matern(range_gt = cutoff * 3, sigma_lt = 2),
  matern_s = pc_matern(range_gt = cutoff * 2, sigma_lt = 1))
  # control = sdmTMBcontrol(
  #   start = list(
  #   ln_tau_V = matrix(log(0.1), 2, 2)
  #   ),
  #   map = list(
  #     ln_tau_V = factor(as.vector(matrix(c(1, NA, NA, 2, NA, NA), 2, 2)))
  #   ),
  #   newton_loops = 1L
  # )
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


nd$year_factor <- as.factor(nd$year)
nd$log_botdepth_c <- nd$log_botdepth - mean
nd$log_botdepth_c2 <- nd$log_botdepth_c * nd$log_botdepth_c

p <- predict(fittv, newdata = nd, se_fit = TRUE, re_form = NA)
#p$depth_m <- exp(p$log_botdepth_cent + mean) * -1
p$depth_m <- exp(p$log_botdepth)
#p$mean_logbot_depth <- mean
saveRDS(p, paste0("output/indtv-", model, ".rds"))

p |>
  #group_by(year) |>
  #mutate(est2 = est - est[depth_m == min(depth_m)]) |>
  mutate(est2 = est - mean(est)) |>
  ggplot() +
  geom_line(aes((depth_m ), (est2), group = as.factor(year), colour = year)) +
  scale_colour_viridis_c() +
  theme_classic()
ggsave("Figures/fit-tv-predicteddepth_centre.jpg", width = 4, height = 3)

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
  coord_cartesian(expand = F) +
  labs(x = "Depth (m)", y = "Density (number/km2)") +
  theme_classic()
ggsave("Figures/fit-tv-predicteddepth_CIs.jpg", width = 4, height = 3)
