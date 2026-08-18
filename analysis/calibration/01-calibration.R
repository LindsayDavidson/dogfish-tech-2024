#paired calibration no lengths

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

comp_df <- comp_df %>% mutate(cpue_dog = catch_dog/exp(offset), cpue_hbll = catch_hbll/exp(offset))
weight = exp(comp_df$offset)
dummy_mesh <- sdmTMB:offsetdummy_mesh <- sdmTMB::make_mesh(comp_df, c("UTM.lon", "UTM.lat"), n_knots = 10)

m <- sdmTMB(

  #cbind(catch_hbll, catch_dog) ~ 1,
  cbind(cpue_hbll, cpue_dog) ~ 1,

  mesh = dummy_mesh,
  spatial = "off",
  spatiotemporal = "off",
  data = comp_df,

  #offset = comp_df$offset,
  #family = binomial(),

  weights = weight,
  family = betabinomial(),

  control = sdmTMBcontrol(multiphase = FALSE)
)
sanity(m)
tidy(m)

exp(0.177)
exp(0.131)
exp(0.222)


# Random effect by set
m2 <- sdmTMB(

  #cbind(catch_hbll, catch_dog) ~ 1 + (1 | id), #random effect of fishing event
  cbind(catch_hbll, catch_dog) ~ 1 + poly(depth_m, 2) + (1 | id),

  mesh = dummy_mesh,
  spatial = "off",
  spatiotemporal = "off",
  data = comp_df,
  offset = comp_df$offset,
  family = binomial(),
  control = sdmTMBcontrol(multiphase = FALSE)
)
sanity(m2)

# Plot random effect (station effect)
station_re <- as.list(m2$sd_report, what = "Estimate")$re_b_pars[, 1]
comp_df %>%
  mutate(station_re = station_re) %>%
  ggplot(aes(x = station_re)) +
  geom_histogram() +
  facet_wrap(vars(year))







