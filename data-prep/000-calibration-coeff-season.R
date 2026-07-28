# change the database , add length groups and then have the catch count per group
# need the samps database
# ignore sex for now

# pair by season - can I match sites and depths across HBLL and Dogfish surveys

samps <- readRDS("output/samps_joined.rds") |>
  dplyr::select(year, fishing_event_id, hooksize_desc, catch_count, time_deployed, total_length, activity_desc, grouping_desc, survey_abbrev, hooksize_desc, sex, lglsp_hook_count, latitude, longitude, depth_m) |>
  filter(grepl("COMPARISON", activity_desc))

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

# seperate by length
hbll <- filter(final, survey_abbrev == "HBLL_13/0", hook_name == "HBLL_13/0") |>
  rename(catch_count_hbll = "catch_count", offset_hbll = "lglsp_hook_count", hbll_survey = "survey_abbrev") |>
  mutate(offset_hbll = log(offset_hbll)) |>
  select(-hook_name)

dog <- filter(final, survey_abbrev == "Dog_14/0", hook_name == "Dog_14/0") |>
  rename(catch_count_dog = "catch_count", offset_dog = "lglsp_hook_count", dog_survey = "survey_abbrev") |>
  mutate(offset_dog = log(offset_dog)) |>
  select(-hook_name)

# #pair by sites and depths
min <- min(samps$total_length, na.rm = TRUE)
max <- max(samps$total_length, na.rm = TRUE)
breaks <- c(min - 1, 54, 64, 74, 84, max + 1)

# create a file with all length bins and all areas sampled and catch counts  to left join onto??
combinations <- expand.grid(grouping_desc = unique(final$grouping_desc), length_bin = unique(hbll$length_bin))
comb <- left_join(combinations, unique(hbll[, c("grouping_desc", "catch_count_hbll", "offset_hbll")]))
comb <- left_join(comb, unique(dog[, c("grouping_desc", "catch_count_dog", "offset_dog")])) # these are real NAs, some sites have duplicates in the HBLL as they were sampled twice, maybe take the average? do it after as it wont join if we do it now

hbll <- hbll |>
  mutate(length_bin = cut(total_length, breaks)) |>
  drop_na(total_length) |>
  ungroup()

hbll2 <- hbll |>
  group_by(length_bin, fishing_event_id, year, grouping_desc, catch_count_hbll, latitude, longitude) |>
  reframe(catch_count_hbll_length = n(), survey_abbrev_hbll = "HBLL_13/0", length_bin = length_bin, offset_hbll = mean(offset_hbll)) |>
  drop_na(length_bin) |>
  dplyr::select(-fishing_event_id, -year) |>
  ungroup()

dog <- dog |>
  mutate(length_bin = cut(total_length, breaks)) |>
  drop_na(total_length) |>
  ungroup()
dog2 <- dog |>
  group_by(length_bin, fishing_event_id, year, grouping_desc, catch_count_dog) |>
  reframe(catch_count_dog_length = n(), survey_abbrev_dog = "Dog_14/0", length_bin = length_bin, offset_dog = mean(offset_dog)) |>
  drop_na(length_bin) |>
  dplyr::select(-fishing_event_id, -year) |>
  ungroup()

# first join by grouping depth and get the full catch numbers then join by length bins
hbll3 <- hbll2 |>
  dplyr::select(grouping_desc, catch_count_hbll, catch_count_hbll_length, length_bin, offset_hbll) |>
  distinct()
dog3 <- dog2 |>
  dplyr::select(grouping_desc, catch_count_dog, catch_count_dog_length, length_bin, offset_dog) |>
  distinct()
join <- full_join(comb, hbll3[, c("offset_hbll", "catch_count_hbll", "grouping_desc")]) # first join by depth
join <- full_join(join, dog3[, c("offset_dog", "catch_count_dog", "grouping_desc")]) |>
  distinct() # make NAs zero or keep NA if no fishing happened
join <- full_join(join, dog3)
join <- full_join(join, hbll3)
join2 <- join |> drop_na(catch_count_hbll, catch_count_dog) # drop nas in the toal catch count column as that means those sites weren't sampled and therefore there is no pairwise comp
join2[is.na(join2)] <- 0

join2 <- join2  |>
  group_by(grouping_desc, length_bin) |>
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop") #take the average of duplicates

latlon <- readRDS("data-raw/wrangled-hbll-dog-sets.rds") |>
  dplyr::select(latitude, longitude, grouping_desc) |> distinct() |>
  group_by(grouping_desc) |>
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop") #take the average of duplicates

join2 <- left_join(join2, latlon) |> mutate(offset = offset_hbll - offset_dog)
head(join2)

# change to UTMs
final3 <- add_utm_columns(
  join2,
  ll_names = c("longitude", "latitude"),
  ll_crs = 4326,
  utm_names = c("UTM.lat", "UTM.lon")
)


# model run for length based calibration coefficient
dummy_mesh <- sdmTMB::make_mesh(final3, c("UTM.lon", "UTM.lat"), n_knots = 10)

m2 <- sdmTMB(
  #cbind(catch_count_hbll_length, catch_count_dog_length) ~ 1 + (1 | grouping_desc), # length calibration coeff, then each catch should be for a length bin
  cbind(catch_count_hbll_length , catch_count_dog_length ) ~ 0 + factor(length_bin) + (1 | grouping_desc), #could have a season and calc q for paired comp and q for across season, here data is just season
  mesh = dummy_mesh,
  spatial = "off",
  spatiotemporal = "off",
  data = final3,
  offset = final3$offset,
  family = binomial(),
  control = sdmTMBcontrol(multiphase = FALSE)
)

coef(m2)
exp(coef(m2))
q1 <- coef(m2)["factor(length_bin)(43,54]"]
exp(q1) # 1.74 for smallest length bin
q2 <- coef(m2)["factor(length_bin)(54,64]"]
exp(q2)
q3 <- coef(m2)["factor(length_bin)(64,74]"]
exp(q3)
q4 <- coef(m2)["factor(length_bin)(74,84]"]
exp(q4)
q5 <- coef(m2)["factor(length_bin)(84,113]"]
exp(q5)

# make a table of legnth bin and q between hbll and dog by length

length <- data.frame("Length (cm)" = c("43-54", "54-64", "64-74", "74-84", "84-113"))
q <- data.frame("q ratio (HBLL/Dog" = round(c(exp(q1), exp(q2), exp(q3), exp(q4), exp(q5)),2))

table <- cbind(length, q)
table
