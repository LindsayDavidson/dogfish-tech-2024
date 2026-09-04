#calibration coeff for paired comparative sets, tested calibration coeff by length, depth, and intercept only model

library(dplyr)
library(tidyr)
library(sdmTMB)
library(ggplot2)

samps <- readRDS("output/samps_joined.rds")

samps <- samps %>%
  filter(grepl("COMPARISON", activity_desc))

id_remove <- samps %>% filter(hook_desc == "J-HOOK") %>% pull(fishing_event_id)

site_id <- samps |>
  distinct(fishing_event_id) |>
  mutate(fold_id = factor(1:n()))

samps <- filter(samps, !fishing_event_id %in% id_remove) %>%
  arrange(year, fishing_event_id, survey_abbrev)

samps <- samps |>
  filter(year %in% c(2022, 2023)) |>
  mutate(hook_name = ifelse(hooksize_desc == "13/0", "HBLL_13/0", "Dog_14/0")) |>
  dplyr::select(hook_name, sex, fishing_event_id, year, length, time_deployed, lglsp_hook_count, latitude, longitude, depth_m, grouping_depth_id)

samps <- samps |> mutate(survey_abbrev = ifelse(time_deployed > "2023-09-06 09:15:21", "Dog_14/0", hook_name))
unique(samps$grouping_depth_id)

samps <- left_join(samps, site_id)

min <- min(samps$length, na.rm = TRUE)
max <- max(samps$length, na.rm = TRUE)

breaks <- c(seq(min, max, by = 10),112)
breaks <- c(min-1, 64, 84, max+1) #change to a 80 plus length bin

test <- samps |>  group_by(hook_name) |> mutate(length_bin = cut(length, breaks)) |> drop_na(length) |> ungroup()

test2 <- test |> group_by(length_bin, hook_name, fishing_event_id, year) |>
  reframe(catch_count_length = n(), offset = mean(lglsp_hook_count)) |>
  drop_na(length_bin) |>
  ungroup()

test3 <- test |> left_join(test2) |> dplyr::select(-length, -sex) |> distinct()

test4 <- test3 |>
  #filter(hook_name == "Dog_14/0") |>
  select(year, length_bin, fishing_event_id, latitude, longitude, depth_m, hook_name, catch_count_length) %>%
  distinct() |>
  group_by(fishing_event_id) |>
  pivot_wider(
    names_from = hook_name    ,
    values_from = catch_count_length
  ) |>
  rename(catch_count_hbll_length = "HBLL_13/0", catch_count_dog_length = "Dog_14/0") |>
  ungroup()


depth <- samps |> group_by(hook_name, fishing_event_id, year, grouping_depth_id, fold_id) |>
  reframe(catch_count = n(), offset = mean(lglsp_hook_count)) |>
  ungroup() |>
  pivot_wider(
    names_from = hook_name    ,
    values_from = c(catch_count, offset)
  ) |>
  ungroup() |>
  arrange(year, fishing_event_id) |>
  drop_na("offset_HBLL_13/0", "offset_Dog_14/0") |>
  rename(offset_HBLL = "offset_HBLL_13/0" , offset_Dog = "offset_Dog_14/0") |>
  rename(catch_count_HBLL = "catch_count_HBLL_13/0" , catch_count_Dog = "catch_count_Dog_14/0") |>
  mutate(offset = log(offset_HBLL) - log(offset_Dog))

test5 <- test3 |>
  select(year, fishing_event_id, lglsp_hook_count, hook_name, grouping_depth_id, fold_id) %>%
  distinct() |>
  mutate(offset_name = ifelse(hook_name == "HBLL_13/0", "offset_hbll", "offset_dog")) |>
  dplyr::select(-hook_name) |>
  group_by(fishing_event_id) |>
  pivot_wider(
    names_from = offset_name    ,
    values_from = lglsp_hook_count
  ) |>
  ungroup() |>
  arrange(year, fishing_event_id) |>
  mutate(offset = log(offset_hbll) - log(offset_dog))

final <- left_join(test4, test5)
final$grouping_depth_id


#change to UTMs
final <- add_utm_columns(
  final,
  ll_names = c("longitude", "latitude"),
  ll_crs = 4326,
  utm_names = c("UTM.lat", "UTM.lon"))


# length based model ------------------------------------------------------

dummy_mesh <- sdmTMB::make_mesh(final, c("UTM.lon", "UTM.lat"), n_knots = 10)
final <- final |> drop_na()
weight = exp(final$offset)
dummy_mesh <- sdmTMB::make_mesh(final, c("UTM.lon", "UTM.lat"), n_knots = 10)

mlength <- sdmTMB(
  cbind(catch_count_hbll_length , catch_count_dog_length) ~ 0 + factor(length_bin), #length calibration coeff, then each catch should be for a length bin
  #cbind(cpue_hbll_length , cpue_dog_length ) ~ 0 + factor(length_bin),
  mesh = dummy_mesh,
  spatial = "off",
  spatiotemporal = "off",
  data = final,
  #offset = final$offset,
  #family = binomial(),
  family = betabinomial(),
  weights = weight,
  control = sdmTMBcontrol(multiphase = FALSE)
)

AIC(mlength)
coef(mlength)
exp(tidy(mlength)$estimate)
exp(tidy(mlength)$conf.low)
exp(tidy(mlength)$conf.high)



# intercept only model ----------------------------------------------------

weight = exp(depth$offset)

mint <- sdmTMB(
  cbind(catch_count_HBLL , catch_count_Dog ) ~ 1,
  mesh = dummy_mesh,
  spatial = "off",
  spatiotemporal = "off",
  data = depth,
  family = betabinomial(),
  weights = weight,
  control = sdmTMBcontrol(multiphase = FALSE)
)

coef(mint)
mint2 <- tidy(mint, ran.pars = TRUE)
exp(mint2$estimate)
exp(mint2$conf.low)
exp(mint2$conf.high)



# depth model -------------------------------------------------------------

weight = exp(depth$offset)

depthm <- sdmTMB(
  cbind(catch_count_HBLL , catch_count_Dog ) ~ 1 + as.factor(grouping_depth_id),
  mesh = dummy_mesh,
  spatial = "off",
  spatiotemporal = "off",
  data = depth,
  #offset = final$offset,
  #family = binomial(),
  family = betabinomial(),
  weights = weight,
  control = sdmTMBcontrol(multiphase = FALSE)
)
coef(depthm)
depthc <- tidy(depthm, ran.pars = TRUE)
exp(depthc$estimate)
exp(depthc$conf.low)
exp(depthc$conf.high)



# random effect of site model ---------------------------------------------

site <- sdmTMB(
  cbind(catch_count_HBLL , catch_count_Dog ) ~ 1 + (1|fishing_event_id),
  #cbind(cpue_hbll , cpue_dog ) ~ 1 + (1|fishing_event_id),
  mesh = dummy_mesh,
  spatial = "off",
  spatiotemporal = "off",
  data = depth,
  #offset = final$offset,
  #family = binomial(),
  family = betabinomial(),
  weights = weight,
  control = sdmTMBcontrol(multiphase = FALSE)
)

coef(site)
site2 <- tidy(site, ran.pars = TRUE)
exp(site2$estimate)
exp(site2$conf.low)
exp(site2$conf.high)



# compete aics ------------------------------------------------------------

AIC(site) #cant compare but the coefs are the same as the mint so chose that
AIC(mint)
AIC(mlength) #how to compare length?
AIC(depthm)

dummy_mesh <- sdmTMB::make_mesh(final, c("UTM.lon", "UTM.lat"), n_knots = 10)
final <- final |> drop_na(offset, catch_count_hbll_length, catch_count_dog_length)
weight = exp(final$offset)

k_folds <- 5
clust_folds <- data.frame(fishing_event_id = unique(final$fishing_event_id)) %>%
  mutate(clust = sample(rep(1:k_folds, length.out = n())))

final <- left_join(final, clust_folds)
depth <- left_join(depth, clust_folds)

table(final$clust, final$fishing_event_id)
table(depth$clust, depth$fishing_event_id)


m_cv_length <- sdmTMB_cv(
    cbind(catch_count_hbll_length , catch_count_dog_length) ~ 0 + factor(length_bin),
    mesh = dummy_mesh,
    spatial = "off",
    spatiotemporal = "off",
    data = final,
    #offset = final$offset,
    #family = binomial(),
    family = betabinomial(),
    weights = weight,
    control = sdmTMBcontrol(multiphase = FALSE),
    k_folds = 5,
    fold_ids = final$clust
)

m_cv_length$fold_loglik
m_cv_length$sum_loglik

weight = exp(depth$offset)

m_cv_depth <- sdmTMB_cv(
    cbind(catch_count_HBLL , catch_count_Dog ) ~ 1,
    mesh = dummy_mesh,
    spatial = "off",
    spatiotemporal = "off",
    data = depth,
    #offset = final$offset,
    #family = binomial(),
    family = betabinomial(),
    weights = weight,
    control = sdmTMBcontrol(multiphase = FALSE),
    k_folds = 5,
    fold_ids = depth$clust
)

m_cv_depth$fold_loglik
m_cv_depth$sum_loglik

#depth model has the higher loglik and therefore better model
table = (tidy(mint))

exp(table$estimate)
exp(table$conf.low)
exp(table$conf.high)


