# calibration coeff for paired comparative sets

library(dplyr)
library(tidyr)
library(sdmTMB)
library(ggplot2)

samps <- readRDS("data-generated/samps_joined.rds")

samps <- samps %>%
  filter(grepl("COMPARISON", activity_desc))

id_remove <- samps %>%
  filter(hook_desc == "J-HOOK") %>%
  pull(fishing_event_id)

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

depth <- samps |>
  group_by(hook_name, fishing_event_id, year, grouping_depth_id, fold_id) |>
  reframe(catch_count = n(), offset = mean(lglsp_hook_count)) |>
  ungroup() |>
  pivot_wider(
    names_from = hook_name,
    values_from = c(catch_count, offset)
  ) |>
  ungroup() |>
  arrange(year, fishing_event_id) |>
  drop_na("offset_HBLL_13/0", "offset_Dog_14/0") |>
  rename(offset_HBLL = "offset_HBLL_13/0", offset_Dog = "offset_Dog_14/0") |>
  rename(catch_count_HBLL = "catch_count_HBLL_13/0", catch_count_Dog = "catch_count_Dog_14/0") |>
  mutate(
    offset_log = log(offset_HBLL) - log(offset_Dog),
    weight = (offset_HBLL) / (offset_Dog)
  )


# intercept only model ----------------------------------------------------
weight <- depth$weight

mintbb <- sdmTMB(
  cbind(catch_count_HBLL, catch_count_Dog) ~ 1,
  spatial = "off",
  spatiotemporal = "off",
  data = depth,
  family = betabinomial(),
  weights = weight,
  control = sdmTMBcontrol(multiphase = FALSE)
)

mintb <- update(mintbb, family = binomial(), offset = depth$offset_log)

mintqb <- glm(
  formula = cbind(catch_count_HBLL, catch_count_Dog) ~ 1 + offset(offset_log),
  data = depth,
  family = quasibinomial()
)

sanity(mintb)
sanity(mintbb)

AIC(mintb)
AIC(mintbb) # lower
AIC(mintqb) # no aic

coef(mintqb)
coef(mintbb) # very similar

mintbb_params <- tidy(mintbb, ran.pars = TRUE)
exp(mintbb_params$estimate)
exp(mintbb_params$conf.low)
exp(mintbb_params$conf.high)


# depth factor model -------------------------------------------------------------

depthmbb <- sdmTMB(
  cbind(catch_count_HBLL, catch_count_Dog) ~ 0 + as.factor(grouping_depth_id),
  spatial = "off",
  spatiotemporal = "off",
  data = depth,
  family = betabinomial(),
  weights = weight,
  control = sdmTMBcontrol(multiphase = FALSE)
)

depthmb <- update(depthmbb, family = binomial(), offset = depth$offset_log)

# check disperson
pearson_chisq <- sum(residuals(depthmb, type = "pearson")^2)

pchisq(
  pearson_chisq,
  df = df.residual(depthmb),
  lower.tail = FALSE
)

sanity(depthmb)
sanity(depthmbb)

AIC(depthmb)
AIC(depthmbb)

coef(depthmbb)
depthmbb_params <- tidy(depthmbb, ran.pars = TRUE)
exp(depthmbb_params$estimate)
exp(depthmbb_params$conf.low)
exp(depthmbb_params$conf.high)


# compete aics ------------------------------------------------------------

AIC(mintbb)
AIC(depthmbb)
