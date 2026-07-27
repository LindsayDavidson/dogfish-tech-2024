#change the databse , add length groups and then have the catch count per group
#need the samps database
#ignore sex for now

#pair by season - can I match sites and depths across HBLL and Dogfish surveys
samps <- readRDS("output/samps_joined.rds")
dat <- readRDS("data-raw/wrangled-hbll-dog-sets.rds")
dat <- dat |> dplyr::select(year, fishing_event_id, grouping_desc, time_deployed, survey_abbrev, catch_count, hooksize_desc, latitude, longitude) |>
  filter(year %in% c(2022, 2023)) |>  #add in m and f catches to this to preserve zeros
 filter(survey_abbrev == "OTHER") |>
  dplyr::select(-survey_abbrev)

samps <- samps %>%
  filter(grepl("COMPARISON", activity_desc))

id_remove <- samps %>% filter(hook_desc == "J-HOOK") %>% pull(fishing_event_id)

samps <- filter(samps, !fishing_event_id %in% id_remove) %>%
  arrange(year, fishing_event_id, survey_abbrev)

samps <- samps |>
  filter(!year %in% c(2004, 2019, 2024)) |>
  mutate(hook_name = ifelse(hooksize_desc == "13/0", "HBLL_13/0", "Dog_14/0")) |>
  dplyr::select(hook_name, sex, fishing_event_id, year, length, time_deployed, lglsp_hook_count, latitude, longitude, depth_m, catch_count)

samps <- samps |> mutate(survey_abbrev = ifelse(year == 2023 & time_deployed > as.POSIXct("2023-09-06 09:15:21"), "Dog_14/0",
                                                ifelse(year == 2023 & time_deployed <= as.POSIXct("2023-09-06 09:15:21"), "HBLL_13/0",
                                                       "HBLL_13/0")))

test <- left_join(samps, dat)
unique(test$grouping_desc)

#pull out HBLL survey comp sets
hbll <- filter(test, survey_abbrev == "HBLL_13/0", hook_name == "HBLL_13/0") |>
  rename(catch_count_hbll = "catch_count", offset_hbll = "lglsp_hook_count", hbll_survey = "survey_abbrev") |>
  mutate(offset_hbll = log(offset_hbll)) |>
  select(-hook_name)
  #filter(sex == 2) |>  #erase this just want to do it for now
  #dplyr::select(-length, -sex) |>
  #distinct()
dath <- dat |> filter(hooksize_desc == "13/0")
hbll2 <- left_join(hbll, dath)

#pull out dog survey comp sets
dog <- filter(test, survey_abbrev == "Dog_14/0", hook_name == "Dog_14/0") |>
  rename(catch_count_dog = "catch_count", offset_dog = "lglsp_hook_count", dog_survey = "survey_abbrev") |>
  mutate(offset_dog = log(offset_dog)) |>
  select(-hook_name)
  #filter(sex == 2) |>  #erase this just want to do it for now
  #dplyr::select(-length, -sex) |>
  #distinct()
datd <- dat |> filter(hooksize_desc == "14/0")
dog2 <- left_join(dog, datd)

#pair by sites and depths
final <- full_join(hbll, dog) |> drop_na(catch_count_hbll) |>
  mutate(offset = offset_hbll - offset_dog)
head(final)

#by length, get by season first and come back to
min <- min(samps$length, na.rm = TRUE)
max <- max(samps$length, na.rm = TRUE)
breaks <- c(43, 54,64,74,84, 113) #change to a 80 plus length bin

test <- hbll |>  mutate(length_bin = cut(length, breaks)) |> drop_na(length) |> ungroup()
test2 <- test |> group_by(length_bin, fishing_event_id, year, grouping_desc, catch_count_hbll, latitude, longitude ) |>
  reframe(catch_count_hbll_length = n(), offset_hbll = mean(offset_hbll), survey_abbrev_hbll = "HBLL_13/0") |>
  drop_na(length_bin) |>
  dplyr::select(-fishing_event_id, -year) |>
  ungroup()

testd <- dog |>  mutate(length_bin = cut(length, breaks)) |> drop_na(length) |> ungroup()
testd2 <- testd |> group_by(length_bin, fishing_event_id, year, grouping_desc, catch_count_dog) |>
  reframe(catch_count_dog_length = n(), offset_dog = mean(offset_dog), survey_abbrev_dog = "Dog_14/0") |>
  drop_na(length_bin) |>
  dplyr::select(-fishing_event_id, -year) |>
  ungroup()

#first join by grouping depth and get the full catch numbers then join by length bins
glimpse(testd2)
glimpse(test2)

hbll2 <- test2 |> dplyr::select(grouping_desc, catch_count_hbll, offset_hbll) |> distinct()
dog2 <- testd2 |> dplyr::select(grouping_desc, catch_count_dog, offset_dog) |> distinct()
final <- full_join(hbll2, dog2) #get rid of the NAs as that means there is no comparative sites fished for hbll and dog
final <- final |> drop_na()


final1 <- full_join(final , test2)
final2 <- full_join(final1, testd2) |>
  mutate(offset = offset_hbll - offset_dog) |>
  drop_na(latitude, longitude, catch_count_hbll, catch_count_dog)
final2$catch_count_dog_length[is.na(final2$catch_count_dog_length)] <- 0

#test3 <- test |> left_join(test2) |> dplyr::select(-length, -sex) |> distinct()
#test4 <- test3 |>
#  select(year, length_bin, fishing_event_id, latitude, longitude, depth_m, hook_name, catch_count) %>%
#  distinct() |>
#  group_by(fishing_event_id) |>
#  pivot_wider(
#    names_from = hook_name    ,
#    values_from = catch_count
#  ) |>
#  rename(catch_count_hbll = "HBLL_13/0", catch_count_dog = "Dog_14/0") |>
#  ungroup()
#test5 <- test3 |>
#  select(year, fishing_event_id, lglsp_hook_count, hook_name) %>%
#  distinct() |>
#  mutate(offset_name = ifelse(hook_name == "HBLL_13/0", "offset_hbll", "offset_dog")) |>
#  dplyr::select(-hook_name) |>
#  group_by(fishing_event_id) |>
#  pivot_wider(
#    names_from = offset_name    ,
#    values_from = lglsp_hook_count
#  ) |>
#  ungroup() |>
#  arrange(year, fishing_event_id) |>
#  mutate(offset = log(offset_hbll) - log(offset_dog))
#final <- left_join(test4, test5)

#change to UTMs
final3 <- add_utm_columns(
  final2,
  ll_names = c("longitude", "latitude"),
  ll_crs = 4326,
  utm_names = c("UTM.lat", "UTM.lon"))


#model run for length based calibration coefficient
dummy_mesh <- sdmTMB::make_mesh(final3, c("UTM.lon", "UTM.lat"), n_knots = 10)

m2 <- sdmTMB(
  cbind(catch_count_hbll , catch_count_dog ) ~ 1 + (1 | grouping_desc), #length calibration coeff, then each catch should be for a length bin
  #cbind(catch_count_hbll_length , catch_count_dog_length ) ~ 0 + factor(length_bin) + (1 | grouping_desc), #could have a season and calc q for paired comp and q for across season, here data is just season
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
exp(q1) #1.74 for smallest length bin
q2 <- coef(m2)["factor(length_bin)(54,64]"]
exp(q2)
q3 <- coef(m2)["factor(length_bin)(64,74]"]
exp(q3)
q4 <- coef(m2)["factor(length_bin)(74,84]"]
exp(q4)
q5 <- coef(m2)["factor(length_bin)(84,113]"]
exp(q5)

#make a table of legnth bin and q between hbll and dog by length

length <- data.frame("Length (cm)" = c("43-54", "54-64", "64-74", "74-84", "84-113"))
q <- data.frame('q ratio (HBLL/Dog' = c(1.74, 1.40, 1.21, 1.13, 1.13 ))

table <- cbind(length, q)
