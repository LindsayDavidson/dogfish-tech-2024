#plot depth through time for HBLL and DOg survey

library(ggplot2)
library(tidyverse)
library(sdmTMB)

# data ---------------------------------------------------------------
# all
samps <- readRDS("output/samps_joined.rds")
sets <- readRDS("data-raw/wrangled-hbll-dog-sets.rds")
samps <- samps |> filter(survey3 %in% c("HBLL INS S", "dog"))
samps <- left_join(samps, sets[,c("fishing_event_id", "year", "depth_m")])
#needs depth

# plot length comps through time for each survey --------------------------

samps |>
  filter(sex %in% c(1, 2)) |>
  ggplot() +
  geom_density(aes(depth_m, group = year, colour = year, fill = year),
               alpha = 0.2) +
  facet_wrap(~survey_abbrev + sex) + theme_classic() +
  scale_x_continuous(limits = c(45, 115))

samps |>
  filter(sex %in% c(1, 2)) |>
  ggplot() +
  geom_point(aes(depth_m, length, group = year, colour = year, fill = year),
               alpha = 0.2) +
  facet_wrap(~survey_abbrev + sex) + theme_classic() +
  scale_x_continuous(limits = c(45, 115))

samps$depth_group <- ifelse(samps$depth_m %in% c(0:20), 1,
                 ifelse(samps$depth_m %in% c(21:40), 2,
                        ifelse(samps$depth_m %in% c(41:60), 3,
                               ifelse(samps$depth_m %in% c(61:80), 4,
                                      ifelse(samps$depth_m %in% c(81:100), 5,
                                             ifelse(samps$depth_m %in% (100:400), 6,
                                                                        NA))))))

samps |>
  filter(sex %in% c(1, 2)) |>
  #filter(depth_m %in% (0:50)) |>
  group_by(survey_abbrev, sex, year, depth_group) |>
  reframe(n = n(), depth_m = depth_m) |>
  ggplot() +
  geom_line(aes(depth_m, n, group = year, colour = year, fill = year),
               alpha = 0.9) +
  facet_wrap(~survey_abbrev + sex, scales = "free") + theme_classic()


samps |>
  filter(sex %in% c(1, 2)) |>
  filter(depth_m %in% (0:50)) |>
  ggplot() +
  geom_density(aes(length, group = year, colour = year, fill = year),
               alpha = 0.2) +
  facet_wrap(~survey_abbrev + sex, scales = "free") + theme_classic()

samps |>
  filter(sex %in% c(1, 2)) |>
  filter(depth_m %in% (50:100)) |>
  ggplot() +
  geom_density(aes(length, group = year, colour = year, fill = year),
               alpha = 0.2) +
  facet_wrap(~survey_abbrev + sex, scales = "free") + theme_classic()

samps |>
  filter(sex %in% c(1, 2)) |>
  filter(depth_m %in% (150:400)) |>
  ggplot() +
  geom_density(aes(length, group = year, colour = year, fill = year),
               alpha = 0.2) +
  facet_wrap(~survey_abbrev + sex, scales = "free") + theme_classic()


samps |>
  filter(sex %in% c(2)) |>
  filter(survey_abbrev == "HBLL INS S") |>
  ggplot() +
  geom_vline(xintercept = c(70)) +
  geom_density(aes(length, group = year), fill = "red",
               alpha = 0.2) +
  facet_wrap(~year, ncol = 1) + theme_classic() +
  scale_x_continuous(limits = c(40, 120)) +
  guides(fill = "none", colour = "none")
ggsave("Figures/lenghtcomps_time.jpg", height = 12, width = 3)

samps |>
  filter(sex %in% c(2)) |>
  filter(survey_abbrev == "dog") |>
  ggplot() +
  geom_vline(xintercept = c(65, 85)) +
  geom_density(aes(length, group = year), fill = "red",
               alpha = 0.2) +
  facet_wrap(~year, ncol = 1) + theme_classic() +
  scale_x_continuous(limits = c(40, 120)) +
  guides(fill = "none", colour = "none")
ggsave("Figures/lenghtcomps_dog_time.jpg", height = 12, width = 3)

samps |>
  filter(sex %in% c(1)) |>
  filter(survey_abbrev == "dog") |>
  ggplot() +
  geom_vline(xintercept = c(75)) +
  geom_density(aes(length, group = year), fill = "grey50",
               alpha = 0.2) +
  facet_wrap(~year, ncol = 1) + theme_classic() +
  scale_x_continuous(limits = c(40, 100)) +
  guides(fill = "none", colour = "none")
ggsave("Figures/lenghtcomps_dog_males_time.jpg", height = 12, width = 3)

samps |>
  filter(sex %in% c(1)) |>
  filter(survey_abbrev == "HBLL INS S") |>
  ggplot() +
  geom_vline(xintercept = c(75)) +
  geom_density(aes(length, group = year), fill =  "grey50",
               alpha = 0.2) +
  facet_wrap(~year, ncol = 1) + theme_classic() +
  scale_x_continuous(limits = c(40, 100)) +
  guides(fill = "none", colour = "none")
ggsave("Figures/lenghtcomps_hbll_males_time.jpg", height = 12, width = 3)



# generate time vary depth index ------------------------------------------

  mesh <- make_mesh(d, c("UTM.lon", "UTM.lat"), cutoff = 5)
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

  fit <- sdmTMB(
    formula =
    catch_count ~ poly(log_botdepth, 2) + as.factor(survey_type),
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
  )

  fit$sd_report
  sanity(fit)
  # fitjul <- update(fit, formula = catch_count ~ poly(log_botdepth, 2) + as.factor(survey_type) + poly(julian, 2))
  # fitmonth <- update(fit, formula = catch_count ~ poly(log_botdepth, 2) + as.factor(survey_type) + poly(month, 2))
  # sanity(fitmonth)
  # fitmonth$sd_report
  # saveRDS(fitmonth, file = "output/fit-sog-hblldog-month.rds")
  #fitmonth <- readRDS(file = "output/fit-sog-hblldog-month.rds")

  #pred <- predict(fitmonth, grid, return_tmb_object = TRUE, response = TRUE)
  #index <- get_index(pred, bias_correct = TRUE)
  #saveRDS(index, file = "output/ind-sog-hblldog-month.rds")

  saveRDS(fit, file = path)
  # saveRDS(fitjul, file = "output/fit-sog-hblldog-julian.rds")

  pred <- predict(fit, grid, return_tmb_object = TRUE, response = TRUE)
  index <- get_index(pred, bias_correct = TRUE)


