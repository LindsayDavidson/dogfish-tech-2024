library(dplyr)
library(tidyr)
library(sdmTMB)
library(ggplot2)

# pair by season - can I match sites and depths across HBLL and Dogfish surveys
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

# hbll2 <- hbll |>
#   group_by(fishing_event_id, year, grouping_desc, catch_count_hbll, latitude, longitude) |>
#   reframe(catch_count_hbll_length = n(), survey_abbrev_hbll = "HBLL_13/0", length_bin = length_bin, offset_hbll = mean(offset_hbll)) |>
#   drop_na(length_bin) |>
#   dplyr::select(-fishing_event_id, -year) |>
#   ungroup()
#
# dog <- dog |>
#   mutate(length_bin = cut(total_length, breaks)) |>
#   drop_na(total_length) |>
#   ungroup()
# dog2 <- dog |>
#   group_by(length_bin, fishing_event_id, year, grouping_desc, catch_count_dog) |>
#   reframe(catch_count_dog_length = n(), survey_abbrev_dog = "Dog_14/0", length_bin = length_bin, offset_dog = mean(offset_dog)) |>
#   drop_na(length_bin) |>
#   dplyr::select(-fishing_event_id, -year) |>
#   ungroup()

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

final3 <- final3 |> mutate(depth_bin = str_split(grouping_desc, ':', simplify = TRUE)[,2])
unique(final3$depth_bin)


# model run for length based calibration coefficient
final3 <- final3 |> drop_na()
weight <- exp(final3$offset)
dummy_mesh <- sdmTMB::make_mesh(final3, c("UTM.lon", "UTM.lat"), n_knots = 10)

m2 <- sdmTMB(
  cbind(catch_count_hbll, catch_count_dog) ~ 0 + factor(depth_bin), # + (1 | grouping_desc),
  mesh = dummy_mesh,
  spatial = "off",
  spatiotemporal = "off",
  data = final3,

  # offset = final3$offset,
  # family = binomial(),

  weights = weight,
  family = betabinomial(),
  control = sdmTMBcontrol(multiphase = FALSE)
)


#without depth to compare AICs
m3 <- sdmTMB(
  cbind(catch_count_hbll, catch_count_dog) ~ 1 ,
  mesh = dummy_mesh,
  spatial = "off",
  spatiotemporal = "off",
  data = final3,

  # offset = final3$offset,
  # family = binomial(),

  weights = weight,
  family = betabinomial(),
  control = sdmTMBcontrol(multiphase = FALSE)
)
AIC(m3)

sanity(m2)
AIC(m2)
tidy(m2, ran.pars = TRUE)

saveRDS(tidy(m2), "output/calibration_coeffs_depth.rds")

table <- data.frame(est = exp(tidy(m2, ran.pars = TRUE)$estimate))
table$conf.low <- data.frame(conf.low = exp(tidy(m2, ran.pars = TRUE)$conf.low))
table$conf.high <- data.frame(conf.high = exp(tidy(m2, ran.pars = TRUE)$conf.high))


#modify this..

depth <- data.frame("Depth (m)" = c("56-110 m", "111-165 m", "166-220 m", ">220 m"))
q <- data.frame("rho" = round(c(table$est[1], table$est[2], table$est[3], table$est[4]), 2))
data <- data.frame(Data = "seasonally paired")

table <- cbind(data, depth, q)
table$conf.low <- c(round(exp(tidy(m2)$conf.low), 2))
table$conf.high <- c(round(exp(tidy(m2)$conf.high), 2))
table$CI <- paste0((table$conf.low), "-", (table$conf.high))
table <- table |> mutate(final = paste0(rho, " (", conf.low, "-", conf.high, ")"))
table <- table |> dplyr::select("Data", "Depth..m.", final)
rownames(table) <- NULL
#table$"Depth..m." <- rbind(table$"Depth..m.", "all lengths")


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
