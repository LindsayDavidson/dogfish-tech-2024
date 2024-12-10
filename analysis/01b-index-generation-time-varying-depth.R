library(sdmTMB)
library(ggplot2)
library(tidyverse)
library(sdmTMB)

latitude_cutoff <- 50.34056
bccrs <- 32609
source("999-load-hbll-n-s-grid.R") #need a different grid, see below

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

# hbll n and s only -------------------------------------------------------------
d <- df |>
  #filter(survey_abbrev %in% c("HBLL INS S", "HBLL INS N")) |>
  filter(survey_lumped == "hbll") |>
  drop_na(offset) |>
  drop_na(depth_m) |>
  drop_na(julian) |>
  drop_na(catch_count)
range(d$depth_m)
df <- df |> #4 points are deeper than 110, instead of dropping ill change the categorizations
  mutate(depth_bin = case_when(
    depth_m <= 70 ~ 1,
    depth_m > 70 & depth_m <= 150 ~ 2))




# shallow deep hbll block model ------------------------------------------------------
d$year_factor <- as.factor(d$year)
mean <- mean(d$log_botdepth)
d$log_botdepth_c <- d$log_botdepth - mean
d$log_botdepth_c2 <- d$log_botdepth_c * d$log_botdepth_c
unique(d$depth_bin)

dsh <- filter(d, depth_bin == 1)
gridshallow <- filter(grid, depth_bin == 1)
meshsh <- make_mesh(dsh, c("UTM.lon", "UTM.lat"), cutoff = 15)
plot(meshsh)

ddp <- filter(d, depth_bin == 2)
griddeep <- filter(grid, depth_bin == 2)
meshdp <- make_mesh(ddp, c("UTM.lon", "UTM.lat"), cutoff = 15)
plot(meshdp)

ms <- sdmTMB(
  formula = catch_count ~ 1,
  offset = "offset",
  time = "year",
  spatiotemporal = "rw",
  silent = FALSE,
  spatial = "on",
  family = delta_lognormal(),
  mesh = meshsh,
  data = dsh,
  do_index = FALSE,
  extra_time = c(2006, 2017, 2020)
)
saveRDS(ms, "output/fit-depth-bins-shallow.rds")
ms <- readRDS("output/fit-depth-bins-shallow.rds")

md <- sdmTMB(
  formula = catch_count ~ 1,
  offset = "offset",
  time = "year",
  spatiotemporal = "rw",
  silent = FALSE,
  spatial = "off",
  family = delta_gamma(),
  mesh = meshdp,
  data = ddp,
  do_index = FALSE,
  extra_time = c(2006, 2017, 2020)
)
saveRDS(md, "output/fit-depth-bins-deep.rds")
md <- readRDS("output/fit-depth-bins-deep.rds")

sanity(ms)
sanity(md)
ms$sd_report
md$sd_report


psh <- predict(ms, newdata = gridshallow, se_fit = TRUE, re_form = NA, return_tmb_object = TRUE)
index <- get_index(psh, bias_correct = TRUE)
index$loc <- "shallow"
saveRDS(psh, paste0("output/ind-depth-bin-shallow", model, ".rds"))

pdeep <- predict(md, newdata = griddeep, response = TRUE, return_tmb_object = TRUE)
index_d <- get_index(pdeep, bias_correct = TRUE)
index_d$loc <- "deep"
saveRDS(index_d, paste0("output/ind-depth-bin-deep", model, ".rds"))

index <- bind_rows(index, index_d)
index |>
  #group_by(year) |>
  #mutate(est2 = est - est[depth_m == min(depth_m)]) |>
  mutate(est2 = est - mean(est)) |>
  ggplot() +
  geom_line(aes(year, (est), group = loc, colour = loc)) +
  #geom_pointrange(mapping = aes(x = year - 0.25), size = 0.2, pch = 5, alpha = 0.6, position = position_dodge(width = 0.5)) +
  scale_colour_viridis_d() +
  theme_classic()

ggplot(index, aes(year, (est), ymin = (lwr), ymax = (upr), group = loc, colour = loc)) +
  geom_pointrange(mapping = aes(x = year - 0.25), size = 0.2, pch = 5, alpha = 0.6, position = position_dodge(width = 0.5)) +
  scale_colour_manual(values = c("#d8b365", "#5ab4ac")) +
  theme_classic() +
  ylab("Abundance estimate") +
  xlab("Year")  +
  coord_cartesian(ylim = c(0,50))
ggsave("Figures/depth-bin-figure.jpg", width = 4, height = 3)

ggplot(index, aes(year, (est), ymin = (lwr), ymax = (upr), group = loc, colour = loc)) +
  geom_pointrange(data = index, mapping = aes(x = year - 0.25), size = 0.2, pch = 5, alpha = 0.6, position = position_dodge(width = 0.5)) +
  theme_classic() +
  coord_cartesian(ylim = c(0, 30)) +
  scale_colour_manual(values = c("grey", "black"))
ggsave("Figures/ind_deep_shallow.jpg", width = 4, height = 3)

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
