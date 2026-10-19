# calibration coeff for seasonally paired sets.

library(dplyr)
library(tidyr)
library(sdmTMB)
library(ggplot2)
library(stringr)


# Pair by season ----------------------------------------------------------

samps <- readRDS("output/samps_joined.rds") |>
  dplyr::select(year, fishing_event_id, hooksize_desc, catch_count, time_deployed, total_length, activity_desc, grouping_desc, grouping_depth_id, survey_abbrev, hooksize_desc, sex, lglsp_hook_count, latitude, longitude, depth_m) |>
  filter(grepl("COMPARISON", activity_desc)) |>
  mutate(grouping_depth_id = ifelse(depth_m %in% seq(56, 110, 1), "D2", grouping_depth_id))

unique(samps$grouping_depth_id)

id_remove <- samps %>%
  filter(hooksize_desc == "12/0") %>%
  pull(fishing_event_id)

samps <- filter(samps, !fishing_event_id %in% id_remove) %>%
  arrange(year, fishing_event_id, survey_abbrev) |>
  filter(!year %in% c(2004, 2019, 2024)) |>
  mutate(hook_name = ifelse(hooksize_desc == "13/0", "HBLL_13/0", "Dog_14/0")) |>
  mutate(survey_abbrev = ifelse(year == 2023 & time_deployed > as.POSIXct("2023-09-06 09:15:21"), "Dog_14/0",
    ifelse(year == 2023 & time_deployed <= as.POSIXct("2023-09-06 09:15:21"), "HBLL_13/0",
      "HBLL_13/0"
    )
  )) |>
  dplyr::select(-grouping_desc)

dat <- readRDS("data-raw/wrangled-hbll-dog-sets.rds") # need the depth fished to create pairs

dat <- dat |>
  dplyr::select(year, fishing_event_id, grouping_desc, survey_abbrev, catch_count, hooksize_desc) |>
  filter(year %in% c(2022, 2023)) |> # add in m and f catches to this to preserve zeros
  filter(survey_abbrev == "OTHER") |>
  dplyr::select(-survey_abbrev) |>
  rename(catch_count_sets = catch_count)

final <- left_join(samps, dat)
glimpse(final)
unique(final$grouping_desc)
# separate by depht

hbll <- filter(final, survey_abbrev == "HBLL_13/0", hook_name == "HBLL_13/0") |>
  rename(catch_count_hbll = "catch_count", offset_hbll = "lglsp_hook_count", hbll_survey = "survey_abbrev") |>
  mutate(offset_hbll = log(offset_hbll)) |>
  select(-hook_name)

dog <- filter(final, survey_abbrev == "Dog_14/0", hook_name == "Dog_14/0") |>
  rename(catch_count_dog = "catch_count", offset_dog = "lglsp_hook_count", dog_survey = "survey_abbrev") |>
  mutate(offset_dog = log(offset_dog)) |>
  select(-hook_name)

# change to depths here???
# create a file with all length bins and all areas sampled and catch counts  to left join onto??
combinations <- expand.grid(grouping_desc = unique(final$grouping_desc))
comb <- left_join(combinations, unique(hbll[, c("grouping_desc", "catch_count_hbll", "offset_hbll")]))
comb <- left_join(comb, unique(dog[, c("grouping_desc", "catch_count_dog", "offset_dog")])) # these are real NAs, some sites have duplicates in the HBLL as they were sampled twice, maybe take the average? do it after as it wont join if we do it now

# first join by grouping depth and get the full catch numbers then join by length bins
hbll3 <- hbll |>
  dplyr::select(grouping_desc, catch_count_hbll, offset_hbll) |>
  distinct()
dog3 <- dog |>
  dplyr::select(grouping_desc, catch_count_dog, offset_dog) |>
  distinct()
join <- full_join(comb, hbll3[, c("offset_hbll", "catch_count_hbll", "grouping_desc")]) # first join by depth
join <- full_join(join, dog3[, c("offset_dog", "catch_count_dog", "grouping_desc")]) |>
  distinct() # make NAs zero or keep NA if no fishing happened
join <- full_join(join, dog3)
join <- full_join(join, hbll3)
join2 <- join |> drop_na(catch_count_hbll, catch_count_dog) # drop nas in the toal catch count column as that means those sites weren't sampled and therefore there is no pairwise comp
join2[is.na(join2)] <- 0

join2 <- join2 |>
  group_by(grouping_desc) |>
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop") # take the average of duplicates

latlon <- readRDS("data-raw/wrangled-hbll-dog-sets.rds") |>
  dplyr::select(latitude, longitude, grouping_desc) |>
  distinct() |>
  group_by(grouping_desc) |>
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop") # take the average of duplicates

join2 <- left_join(join2, latlon) |> mutate(offset = offset_hbll - offset_dog)
head(join2)

# change to UTMs
final3 <- add_utm_columns(
  join2,
  ll_names = c("longitude", "latitude"),
  ll_crs = 4326,
  utm_names = c("UTM.lat", "UTM.lon")
)

final3 <- final3 |> mutate(depth_bin = str_split(grouping_desc, ":", simplify = TRUE)[, 2])
unique(final3$depth_bin)


# Depth model ------------------------------------------------------

final3 <- final3 |> drop_na()

glimpse(final3)

ggplot(final3, aes(catch_count_dog, catch_count_hbll, group = depth_bin, colour = depth_bin)) +
  geom_point() +
  geom_abline()

weight <- exp(final3$offset)
dummy_mesh <- sdmTMB::make_mesh(final3, c("UTM.lon", "UTM.lat"), n_knots = 10)

final3$grouping_desc <- as.factor(final3$grouping_desc)

mdepthb <- sdmTMB(
  cbind(catch_count_hbll, catch_count_dog) ~ 0 + factor(depth_bin),
  mesh = dummy_mesh,
  spatial = "off",
  spatiotemporal = "off",
  data = final3,
  offset = final3$offset,
  family = binomial(),
  control = sdmTMBcontrol(multiphase = FALSE)
)

mdepthbb <- update(mdepthb, family = betabinomial(), weights = weight)

AIC(mdepthbb)
AIC(mdepthb)

tidy(mdepthbb)
exp(tidy(mdepthbb, ran.pars = TRUE)$estimate)
exp(tidy(mdepthbb, ran.pars = TRUE)$conf.low)
exp(tidy(mdepthbb, ran.pars = TRUE)$conf.high)

q4 <- coef(mdepthbb)["factor(depth_bin) > 220 m"]
q2 <- coef(mdepthbb)["factor(depth_bin) 111 - 165 m"]
q3 <- coef(mdepthbb)["factor(depth_bin) 166 - 220 m"]
q1 <- coef(mdepthbb)["factor(depth_bin) 56 - 110 m"]


# Intercept only model ----------------------------------------------------

intbb <- sdmTMB(
  cbind(catch_count_hbll, catch_count_dog) ~ 1,
  mesh = dummy_mesh,
  spatial = "off",
  spatiotemporal = "off",
  data = final3,
  weights = weight,
  family = betabinomial(),
  control = sdmTMBcontrol(multiphase = FALSE)
)

intb <- update(intbb, family = binomial(), offset = final3$offset)

AIC(intbb)
AIC(intb)

tidy(intbb)
exp(tidy(intbb, ran.pars = TRUE)$estimate)
exp(tidy(intbb, ran.pars = TRUE)$conf.low)
exp(tidy(intbb, ran.pars = TRUE)$conf.high)

saveRDS(tidy(intbb), "output/calibration_coeffs_intercept.rds")


# table of coeff values for overleaf --------------------------------------

#<- not reproducible, check order of params
tidy(mdepth, ran.pars = TRUE)

table <- data.frame("Depth (m)" = c(">220 m", "111-165 m", "166-220 m", "56-110 m"))
table <- table |> mutate(rho = round(exp(tidy(mdepth, ran.pars = TRUE)$estimate), 2))

table <- table |> mutate(conf.low = round(exp(tidy(mdepth, ran.pars = TRUE)$conf.low), 2))
table <- table |> mutate(conf.high = round(exp(tidy(mdepth, ran.pars = TRUE)$conf.high), 2))
table <- table |> mutate(Data = c("seasonally paired", "seasonally paired", "seasonally paired", "seasonally paired"))
table <- table |>
  group_by(rho) |>
  mutate(CI = paste0((conf.low), "-", (conf.high)))
table <- table |> mutate(final = paste0(rho, " (", conf.low, "-", conf.high, ")"))
table <- table |> dplyr::select("Data", "Depth..m.", final)
rownames(table) <- NULL
table$"Depth..m." <- factor(table$"Depth..m.", levels = c("56-110 m", "111-165 m", "166-220 m", ">220 m"))
table <- table |>
  ungroup() |>
  dplyr::select(-rho)

table |>
  knitr::kable(
    format = "latex",
    col.names = c(
      "Data",
      "Length bin (cm)", "rho (CI)"
    ),
    booktabs = TRUE,
    align = "c",
    caption = "rho for seasonally paired comparative sets",
    label = "length_calibration_season"
  ) |>
  # kableExtra::row_spec(0, bold = TRUE, color = "white", background = "grey")  |>  # header styling
  kableExtra::kable_styling(full_width = FALSE)
