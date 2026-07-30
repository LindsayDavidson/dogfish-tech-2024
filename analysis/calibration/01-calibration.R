library(tidyverse)
library(sdmTMB)

dat <- readRDS("data-raw/wrangled-hbll-dog-sets.rds")

#dat$survey_desc %>% table()
#dat$survey_abbrev %>% unique()

dat %>%
  filter(grepl("COMPARISON", activity_desc)) %>%
  summarise(nsets = n(), .by = c(year, survey_abbrev, activity_desc)) %>%
  arrange(year)

#### Comparative sets for HBLL vs dogfish, exclude J hooks for now
comp <- filter(dat, grepl("COMPARISON", activity_desc))

# Remove sets that compared J hook vs circle hook
id_remove <- comp %>% filter(hook_desc == "J-HOOK") %>% pull(fishing_event_id)
comp <- filter(comp, !fishing_event_id %in% id_remove) %>%
  arrange(year, fishing_event_id, survey_abbrev)

#ld added
comp <- comp |> mutate(survey_abbrev = ifelse(hooksize_desc == "13/0", "hbll", "dog"))

unique(comp$offset)
#end

# Compare sets with common fishing_event_id
comp_df <- lapply(unique(comp$fishing_event_id), function(i) { #look at difference in offsets between the hbll and dog sets, puts same fishign events on the same line
  set_i <- filter(comp, fishing_event_id == i)
  df_dog <- filter(comp, fishing_event_id == i, survey_abbrev == "dog")
  df_hbll <- filter(comp, fishing_event_id == i, survey_abbrev == "hbll")

  if (nrow(set_i) != 2) return(data.frame())

  set_i[1, ] %>%
    select(year, fishing_event_id, latitude, longitude, depth_m, UTM.lon, UTM.lat) %>%
    mutate(
      catch_dog = df_dog$catch_count,
      catch_hbll = df_hbll$catch_count,
      offset_dog = df_dog$offset,
      offset_hbll = df_hbll$offset
    )
}) %>%
  bind_rows() %>%
  arrange(year, fishing_event_id) %>%
  rename(id = fishing_event_id) %>%
  mutate(offset = offset_hbll - offset_dog)

# What's happening in 2019 and 2022?
#comp %>% filter(!fishing_event_id %in% comp_df$id)

# Plot calibration data
comp %>% #more hbll sets with higher cpue
  filter(fishing_event_id %in% comp_df$id) %>%
  ggplot(aes(x = catch_count/offset, fill = survey_abbrev)) +
  geom_density(alpha = 0.75) +
  facet_wrap(vars(year))

comp %>%
  filter(fishing_event_id %in% comp_df$id) %>%
  ggplot(aes(x = depth_m, y = catch_count/offset, fill = survey_abbrev)) +
  geom_point(shape = 21) +
  facet_wrap(vars(year))

#coast <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")
comp %>%
  filter(fishing_event_id %in% comp_df$id) %>%
  ggplot(aes(x = longitude, y = latitude)) +
  #geom_sf(data = coast, inherit.aes = FALSE) +
  #coord_sf(xlim = c(-125.5, -123), ylim = c(48.5, 50.5)) +
  #geom_tile(data = gfplot::hbll_inside_n_grid$grid, aes(X, Y), fill = "red", width = 0.02, height = 0.02) +
  #geom_tile(data = gfplot::hbll_inside_s_grid$grid, aes(X, Y), fill = "lightblue", width = 0.02, height = 0.02) +
  geom_point(aes(colour = catch_count/offset)) +
  facet_grid(vars(year), vars(survey_abbrev)) +
  scale_colour_viridis_c(trans = "sqrt")

#### Estimate calibration factors from comparative sets ----


# Fixed effect only
dummy_mesh <- sdmTMB::make_mesh(comp_df, c("UTM.lon", "UTM.lat"), n_knots = 10)

m <- sdmTMB(
  cbind(catch_hbll, catch_dog) ~ 1,
  mesh = dummy_mesh,
  spatial = "off",
  spatiotemporal = "off",
  data = comp_df,
  offset = comp_df$offset,
  family = binomial(),
  control = sdmTMBcontrol(multiphase = FALSE)
)

# Random effect by set
m2 <- sdmTMB(
  cbind(catch_hbll, catch_dog) ~ 1 + (1 | id), #random effect of fishing event
  mesh = dummy_mesh,
  spatial = "off",
  spatiotemporal = "off",
  data = comp_df,
  offset = comp_df$offset, #why does he use this offset? because the binomial model is calculating the ratio and so this is the offset difference
  family = binomial(),
  control = sdmTMBcontrol(multiphase = FALSE)
)




# Plot random effect (station effect)
station_re <- as.list(m2$sd_report, what = "Estimate")$re_b_pars[, 1]
comp_df %>%
  mutate(station_re = station_re) %>%
  ggplot(aes(x = station_re)) +
  geom_histogram() +
  facet_wrap(vars(year))


#### Directly calibrated SoG dogfish + HBLL index, spatiotemporal model without comparative sets ----
# Prediction grid from the HBLL stations
log_rho_CF <- coef(m2)["(Intercept)"]
#calibration exp(log_rho_CF) = 1.19 HBLL catches about 1.19 more than dogfish 19% more dogfish

dogfish <- filter(dat, !grepl("COMPARISON", activity_desc), survey_abbrev != "dog-jhook") %>%
  mutate(offset_rho = offset - ifelse(survey_abbrev == "dog", log_rho_CF, 0),
         cpue = catch_count/exp(offset),
         cpue_rho = catch_count/exp(offset_rho)) %>%
  arrange(year) %>%
  mutate(survey = ifelse(survey_abbrev == "dog", "dog", "hbll")) %>%
  select(!UTM.lon & !UTM.lat) %>%
  sdmTMB::add_utm_columns(ll_names = c("longitude", "latitude"), utm_crs = 32609, utm_names = c("UTM.lon", "UTM.lat"))

ggplot(dogfish, aes(longitude, latitude, fill = cpue_rho)) +
  geom_point(shape = 21) +
  facet_grid(vars(year), vars(survey)) +
  scale_fill_viridis_c(trans = "sqrt")

ggplot(dogfish, aes(depth_m, cpue_rho)) +
  geom_point(shape = 21) +
  facet_grid(vars(survey)) +
  scale_fill_viridis_c()


dogfish <- dogfish |> drop_na(offset_rho)

mesh <- sdmTMB::make_mesh(
  dogfish,
  c("UTM.lon", "UTM.lat"),
  n_knots = 100
)

fit <- sdmTMB( #composite index
  catch_count ~ 0 + factor(year), # + log_botdepth,
  mesh = mesh,
  data = dogfish,
  offset = dogfish$offset_rho,
  time = "year",
  family = nbinom2(),
  anisotropy = TRUE
)

grid_hbll <- rbind(
  gfplot::hbll_inside_n_grid$grid,
  gfplot::hbll_inside_s_grid$grid
) %>%
  sdmTMB::add_utm_columns(ll_names = c("X", "Y"), utm_crs = 32609, utm_names = c("UTM.lon", "UTM.lat"))

index <- local({
  newdata <- replicate_df(grid_hbll, "year", unique(dogfish$year))
  pred <- predict(fit, newdata, return_tmb_object = TRUE)
  get_index(pred, TRUE)
})
write_csv(index, file = "analysis/index-calibration/index/index_dogfish_hbll_calibrate.csv")

# HBLL only
hbll <- filter(dogfish, survey == "hbll")
mesh <- sdmTMB::make_mesh(
  hbll,
  c("UTM.lon", "UTM.lat"),
  n_knots = 100
)

fit_hbll <- sdmTMB(
  catch_count ~ 0 + factor(year), # + log_botdepth,
  mesh = mesh,
  data = hbll,
  offset = hbll$offset_rho,
  time = "year",
  family = nbinom2(),
  anisotropy = TRUE
)

index_hbll <- local({
  newdata <- replicate_df(grid_hbll, "year", unique(hbll$year))
  pred <- predict(fit_hbll, newdata, return_tmb_object = TRUE)
  get_index(pred, TRUE)
})
write_csv(index_hbll, file = "analysis/index-calibration/index/index_hbll.csv")

# Dogfish only
dog <- filter(dogfish, survey == "dog")
mesh <- sdmTMB::make_mesh(
  dog,
  c("UTM.lon", "UTM.lat"),
  n_knots = 20
)

fit_dog <- sdmTMB(
  catch_count ~ 0 + factor(year), # + log_botdepth,
  mesh = mesh,
  data = dog,
  offset = dog$offset_rho,
  time = "year",
  spatiotemporal = "iid",
  family = nbinom2(),
  anisotropy = TRUE
)

grid_dog <- gfplot::dogfish_grid$grid %>%
  mutate(UTM.lon = X/1e3, UTM.lat = Y/1e3)

index_dog <- local({
  newdata <- replicate_df(grid_dog, "year", unique(dog$year))
  pred <- predict(fit_dog, newdata, return_tmb_object = TRUE)
  get_index(pred, TRUE)
})
write_csv(index_dog, file = "analysis/index-calibration/index/index_dog.csv")

# Compare index
index_compare <- rbind(
  read.csv(file = "analysis/index-calibration/index/index_dogfish_hbll_calibrate.csv") %>%
    mutate(Survey = "Calibrated HBLL + SoG dogfish"),
  read.csv(file = "analysis/index-calibration/index/index_dog.csv") %>%
    mutate(Survey = "SoG dogfish"),
  read.csv(file = "analysis/index-calibration/index/index_hbll.csv") %>%
    mutate(Survey = "HBLL")
)

year_dogfish <- index_compare %>%
  filter(Survey == "SoG dogfish") %>%
  pull(year)

g <- index_compare %>%
  mutate(dyear = year %in% year_dogfish) %>%
  ggplot(aes(year, est, ymin = lwr, ymax = upr, colour = dyear)) +
  geom_point() +
  geom_line(aes(group = Survey), linewidth = 0.1) +
  geom_linerange() +
  facet_wrap(vars(Survey), ncol = 1, scales = "free_y") +
  expand_limits(y = 0) +
  labs(x = "Year", y = "Index", colour = "Year with \nSoG dogfish survey?")
ggsave("analysis/index-calibration/index/compare_index.png", g, height = 6, width = 5)