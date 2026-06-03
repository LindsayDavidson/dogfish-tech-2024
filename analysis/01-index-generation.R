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

#load them below
#source("analysis/999-load-grid-data.R") #this is the hbll n, s, and dogfish grid
#source("analysis/999-load-hbll-n-s-grid.R") #this is the hbll n, s grid from gfdata
#ggplot(grid, aes(UTM.lon, UTM.lat)) + geom_point()


# run index ---------------------------------------------------------------

# params
cutoff <- 10

 # model = "hblldog_no2004"
 #model = "hbll-n-s"
 model = "dog"
# model = "dog-predict"

if (model == "hblldog_no2004") { #<- everything except for dogfish comp work in 2004
  d <- df #<- 2004 was removed above with the offset can't be na.
  years <- seq(min(d$year), max(d$year), 1)
  #take out comparative work?? that happened in the different season??
  remove <- filter(d, season == 4 & survey_lumped == "hbll")
  remove1 <- filter(d, season == 3 & survey_lumped == "hbll" & survey_abbrev == "OTHER")
  remove2 <- filter(d, season == 3 & survey_lumped == "dog")
  remove <- bind_rows(remove, remove1, remove2)
  d <- d |> filter(!fishing_event_id  %in% c(remove$fishing_event_id))
  ggplot(d, aes(year, catch_prop)) + geom_point() + facet_wrap(~survey_lumped)
  family = betabinomial(link = "cloglog")

  source("analysis/999-load-grid-data.R") #this is the hbll n, s, and dogfish grid
  #source("analysis/999-load-hbll-n-s-grid.R") #this is the hbll n, s grid from gfdata
  grid <- purrr::map_dfr(years, ~ tibble(grid, year = .x))

  weights <- (d$lglsp_hook_count * d$soak)
  formula <- catch_prop ~ 1 + as.factor(survey_lumped) #<- survey_lumped should just be dog, dogjhook, and hbll
  formuladepth <- catch_prop ~ as.factor(depth_bin) + as.factor(survey_lumped)
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

  weights <- (d$lglsp_hook_count * d$soak)

  #source("analysis/999-load-grid-data.R") #this is the hbll n, s, and dogfish grid
  source("analysis/999-load-hbll-n-s-grid.R") #this is the hbll n, s grid from gfdata
  years <- seq(min(d$year), max(d$year), 1)
  grid <- purrr::map_dfr(years, ~ tibble(grid, year = .x))
  grid$survey_lumped <- "hbll"
  formula <- catch_prop ~ 1
  formuladepth <- catch_prop ~ as.factor(depth_bin)
  family = betabinomial(link = "cloglog")
}

if (model == "dog") {
  d <- df |>
    filter(survey_sep %in% c("dog comp", "dog", "dog-jhook")) |>
    mutate(HBLLcomp = ifelse(activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" & month == 9 & day < 27, "erase", "keep")) |>
    filter(HBLLcomp == "keep") |>
    filter(year != 2022) |>
    drop_na(offset) |>
    drop_na(depth_m) |>
    drop_na(julian) |>
    drop_na(catch_count)

  grid <- readRDS("data-raw/dogfish_sets.rds") |>
    filter(YEAR %in% 1986) |>
    dplyr::select(LATITUDE, LONGITUDE, FE_FISHING_GROUND_COMMENT, GROUPING_DESC) |>
    filter(GROUPING_DESC != 	"SoG Dogfish 0 - 55 m") |>
    distinct(.keep_all = TRUE) |>
    add_utm_columns(ll_names = c("LONGITUDE", "LATITUDE"),
                    utm_names = c("UTM.lon", "UTM.lat"),
                    utm_crs = 32609
    ) |>
    mutate(UTM.lon.m = UTM.lon * 1000, UTM.lat.m = UTM.lat * 1000)

  cutoff <- 2
  weights <- (d$lglsp_hook_count * d$soak)
  years <- seq(min(d$year), max(d$year), 1)
  spatial = "off"
  #source("analysis/999-load-grid-data.R") #this is the hbll n, s, and dogfish grid
  #source("analysis/999-load-hbll-n-s-grid.R") #this is the hbll n, s grid from gfdata
  grid <- purrr::map_dfr(years, ~ tibble(grid, year = .x))
  grid$survey_lumped <- "dog"
  formula <- catch_prop ~ 1 + as.factor(survey_lumped)
  formuladepth <- catch_prop ~ as.factor(depth_bin) + as.factor(survey_lumped)
  family = betabinomial(link = "cloglog")

  priors = sdmTMBpriors(b = normal(c(NA, NA), c(NA, NA)),
                        #matern_st = pc_matern(range_gt = cutoff * 3, sigma_lt = 2),
                        matern_s = pc_matern(range_gt = cutoff * 4, sigma_lt = 2))


}

if (model == "dog-predict") {
  d <- df |>
    #filter(survey_abbrev %in% c("DOG", "OTHER")) |>
    filter(survey_sep %in% c("dog", "dog comp")) |>
    filter(year %in% c(2005, 2008, 2011, 2019, 2023)) |>
    mutate(HBLLcomp = ifelse(month == 9 & day < 27, "erase", "keep")) |>
    filter(HBLLcomp == "keep") |>
   # filter(month > 9) |> # i did this to drop the comp work that happened in summer, keep in and include julian if wanted
    drop_na(offset) |>
    drop_na(depth_m) |>
    drop_na(julian) |>
    drop_na(catch_count)

  grid <- readRDS("data-raw/dogfish_sets.rds") |>
    filter(YEAR %in% 1986) |>
    dplyr::select(LATITUDE, LONGITUDE, FE_FISHING_GROUND_COMMENT, GROUPING_DESC) |>
    filter(GROUPING_DESC != 	"SoG Dogfish 0 - 55 m") |>
    distinct(.keep_all = TRUE) |>
    add_utm_columns(ll_names = c("LONGITUDE", "LATITUDE"),
                         utm_names = c("UTM.lon", "UTM.lat"),
                         utm_crs = 32609
    ) |>
    mutate(UTM.lon.m = UTM.lon * 1000, UTM.lat.m = UTM.lat * 1000)

  weights <- (d$lglsp_hook_count * d$soak)
  years <- seq(min(d$year), max(d$year), 1)
  spatial = "off"

  years <- seq(min(d$year), max(d$year), 1)
  grid <- purrr::map_dfr(years, ~ tibble(grid, year = .x))
  grid$survey_lumped <- "dog"

  formula <- catch_prop ~ 1
  formuladepth <- catch_prop ~ as.factor(depth_bin)
  family = betabinomial(link = "cloglog")

  cutoff = 2
  priorsint <- sdmTMBpriors(b = normal(c(NA), c(NA)),
                            matern_st = pc_matern(range_gt = cutoff * 3, sigma_lt = 2))

}




# index generation--------

# create the mesh
mesh <- make_mesh(d, c("UTM.lon", "UTM.lat"), cutoff = cutoff)
plot(mesh)
mesh$mesh$n



if (model %in% c("dog")) { #<- two surveys
    priorsint <- sdmTMBpriors(b = normal(c(NA, NA), c(NA, NA)),
         matern_st = pc_matern(range_gt = cutoff * 3, sigma_lt = 2),
         matern_s = pc_matern(range_gt = cutoff * 2, sigma_lt = 1))
    priors <-  sdmTMBpriors(
      matern_st = pc_matern(range_gt = cutoff * 3, sigma_lt = 2),
      matern_s = pc_matern(range_gt = cutoff * 2, sigma_lt = 1),
      b = normal(c(NA, 0, 0, 0), c(NA, 1, 1, 1)))
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
  saveRDS(fit, file = paste0("output/fit-sog-intonly", model, ".rds"))
}

# get index
if (!is.null(fit)) { #<- #if fit is null ignore this
  pred <- predict(fit, grid, return_tmb_object = TRUE, response = TRUE)
  index <- get_index(pred, area = 1, bias_correct = TRUE)
  index <- index |> mutate(model = ifelse(year %in% unique(d$year), "yrs_surved", "yrs_interp"))
  index$modelloc <- model
  index$type <- "int"
  saveRDS(index, file = paste0("output/ind-sog-intonly", model, ".rds"))
}

ggplot(index, aes(as.factor(year), (est), ymin = (lwr), ymax = (upr))) +
  geom_pointrange(position = position_dodge(width = 0.25))

