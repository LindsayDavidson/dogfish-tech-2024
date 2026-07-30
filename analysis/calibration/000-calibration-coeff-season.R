library(dplyr)
library(tidyr)
library(sdmTMB)
library(ggplot2)

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

# separate by length
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
breaks <- c(min - 1, 64, 84, max + 1)

hbll <- hbll |>
  mutate(length_bin = cut(total_length, breaks)) |>
  drop_na(total_length) |>
  ungroup()

# create a file with all length bins and all areas sampled and catch counts  to left join onto??
combinations <- expand.grid(grouping_desc = unique(final$grouping_desc), length_bin = unique(hbll$length_bin))
comb <- left_join(combinations, unique(hbll[, c("grouping_desc", "catch_count_hbll", "offset_hbll")]))
comb <- left_join(comb, unique(dog[, c("grouping_desc", "catch_count_dog", "offset_dog")])) # these are real NAs, some sites have duplicates in the HBLL as they were sampled twice, maybe take the average? do it after as it wont join if we do it now

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

join2 <- join2 |>
  group_by(grouping_desc, length_bin) |>
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

# model run for length based calibration coefficient
dummy_mesh <- sdmTMB::make_mesh(final3, c("UTM.lon", "UTM.lat"), n_knots = 10)

m2 <- sdmTMB(
  # cbind(catch_count_hbll_length, catch_count_dog_length) ~ 1 + (1 | grouping_desc), # length calibration coeff, then each catch should be for a length bin
  cbind(catch_count_hbll_length, catch_count_dog_length) ~ 0 + factor(length_bin) + (1 | grouping_desc), # could have a season and calc q for paired comp and q for across season, here data is just season
  mesh = dummy_mesh,
  spatial = "off",
  spatiotemporal = "off",
  data = final3,
  offset = final3$offset,
  family = binomial(),
  control = sdmTMBcontrol(multiphase = FALSE)
)
sanity(m2)
AIC(m2)
coef(m2)
exp(coef(m2))
q1 <- coef(m2)["factor(length_bin)(43,64]"]
q1
exp(q1) # 1.74 for smallest length bin
q2 <- coef(m2)["factor(length_bin)(64,84]"]
q2
exp(q2)
q3 <- coef(m2)["factor(length_bin)(84,113]"]
q3
exp(q3)

# make a table of legnth bin and q between hbll and dog by length

length <- data.frame("Length (cm)" = c("43-64", "64-84", "84-113"))
q <- data.frame("q ratio (HBLL/Dog" = round(c(exp(q1), exp(q2), exp(q3)), 2))

table <- cbind(length, q)
table

log_offset = log(mean(c(exp(q1), exp(q2), exp(q3))))
log_offset
exp(log_offset) #68 percent more fish caught on HBLL

# calibrated composite index ----------------------------------------------

dat <- readRDS("data-raw/wrangled-hbll-dog-sets.rds") # need the depth fished to create pairs
exp(dat$offset)
dat$offset_hksoak
unique(dat$survey_abbrev)
x <- filter(dat, survey_abbrev == "OTHER")

dogfish <-
  #filter(dat, !grepl("COMPARISON", activity_desc)) %>%

  mutate(survey_abbrev = ifelse(survey_abbrev == "DOG" & year %in% c(1986, 1989), "j-hook", survey_abbrev)) |>
  #mutate(survey_abbrev = ifelse(survey_abbrev == "OTHER" & year %in% c(1986, 1989), "j-hook", survey_abbrev)) |> #put in the 2023 and 2024 j hook comp work

  #filter(dat, !grepl("COMPARISON", activity_desc), survey_abbrev != "dog-jhook") %>%
  #filter(!year %in% c(1986, 1989)) |> #come back to add in Jackies calibration for this can I multiply the offsets?


  mutate(

    #offset_rho = offset - ifelse(survey_abbrev == "DOG", log_offset, 0), # define offset differences, minus because you are saying those catches were on fewer hooks

    offset_rho_quang = offset - ifelse(survey_abbrev == "DOG", log(1.19), 0), # quangs value of 1.19

    offset_rho = offset - ifelse(survey_abbrev == "DOG", log_offset,
            ifelse(survey_abbrev %in% c("HBLL INS N", "HBLL INS S"), 0, log(exp(log_offset)/1.45))), # 1.45 from Jackie's 2004 report

    offset_test = offset - ifelse(survey_abbrev == "DOG", 0,
                                 ifelse(survey_abbrev %in% c("HBLL INS N", "HBLL INS S"), -0.597837, log(1.45))), # scale to dog gear, add and subtract

    offset_dog_gears = offset - ifelse(survey_abbrev == "DOG", 0,
                                 ifelse(survey_abbrev %in% c("HBLL INS N", "HBLL INS S"), NA, log(1.45))), # 1.45 from Jackie's 2004 report

    cpue = catch_count / exp(log_offset),
    cpue_rho = catch_count / exp(offset_rho)
  ) %>%
  arrange(year) %>%
  mutate(survey = ifelse(survey_abbrev == "dog", "dog", "hbll")) %>%
  select(!UTM.lon & !UTM.lat) %>%
  sdmTMB::add_utm_columns(ll_names = c("longitude", "latitude"), utm_crs = 32609, utm_names = c("UTM.lon", "UTM.lat"))

ggplot(dogfish, aes(longitude, latitude, fill = cpue_rho)) +
  geom_point(shape = 21) +
  facet_grid(vars(year), vars(survey_abbrev)) +
  scale_fill_viridis_c(trans = "sqrt")

ggplot(dogfish, aes(depth_m, cpue_rho)) +
  geom_point(shape = 21) +
  facet_grid(vars(survey_abbrev)) +
  scale_fill_viridis_c()

ggplot(dogfish, aes(depth_m, offset_rho)) +
  geom_point(shape = 21) +
  facet_grid(vars(survey_abbrev)) +
  scale_fill_viridis_c()

ggplot(dogfish, aes(depth_m, offset)) +
  geom_point(shape = 21) +
  facet_grid(vars(survey_abbrev)) +
  scale_fill_viridis_c()

ggplot(dogfish, aes(year, catch_count, colour = survey_abbrev)) +
  geom_jitter(shape = 21) +
  scale_fill_viridis_c()

# calibrated with j-hook --------------------------------------------------


dogfishj <- dogfish |> filter(survey_abbrev %in% c("DOG", "j-hook"))
#dogfishj <- dogfish
dogfishj <- dogfishj |> drop_na(offset_rho)
unique(dogfishj$year)
unique(dogfishj$survey_abbrev)
unique(dogfishj$offset_test)

mesh <- sdmTMB::make_mesh(
  dogfishj,
  c("UTM.lon", "UTM.lat"),
  n_knots = 10
)


fit <- sdmTMB( # composite index
  catch_count ~ 1, # + log_botdepth, #could include gear and hopefully the coef is zero
  #catch_count ~ 1, # + log_botdepth, #could include gear and hopefully the coef is zero
  mesh = mesh,
  data = dogfishj,
  spatial = "on",
  spatiotemporal = "rw",

  offset = dogfishj$offset_test,

  extra_time = c(1987, 1988, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2006, 2007, 2009, 2010, 2012, 2013, 2015, 2016,
                 2017, 2018),

  #extra_time = c(1987, 1988, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2006, 2007, 2009, 2010, 2012, 2013, 2015, 2016,
  #              2017, 2018, 2020),

  #offset = dogfish$offset_rho_quang, # different offsets see above

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

  newdata <- replicate_df(grid_hbll, "year", c(1986, 1989, 2005 ,2008, 2011, 2014, 2019))
  #newdata <- replicate_df(grid_hbll, "year", c(c(1986, 1989), seq(2003, 2025, 1)))

  pred <- predict(fit, newdata, return_tmb_object = TRUE)
  get_index(pred, TRUE)
})


index <- filter(index, year %in% c(1986, 1989, 2005 ,2008, 2011, 2014, 2019))
#index <- filter(index, year %in% c(c(1986, 1989), seq(2003, 2025, 1)))

ggplot(index, aes(year, est, ymin = lwr, ymax = upr)) +
  geom_point() +
  geom_line( linewidth = 0.1) +
  geom_linerange()

ggplot(index, aes(year, est, ymin = lwr, ymax = upr)) +
  geom_point() +
  geom_line( linewidth = 0.1) +
  geom_linerange()

# calibrated --------------------------------------------------------------

#dogfish <- filter(dogfish, survey_abbrev %in% c("DOG", "j-hook"))
dogfish <- dogfish |> drop_na(offset_rho)

mesh <- sdmTMB::make_mesh(
  dogfish,
  c("UTM.lon", "UTM.lat"),
  n_knots = 10
)

dogfish <- dogfish |>  filter()
fit <- sdmTMB( # composite index
  catch_count ~ 0 + factor(year), # + log_botdepth, #could include gear and hopefully the coef is zero
  #catch_count ~ 1, # + log_botdepth, #could include gear and hopefully the coef is zero
  mesh = mesh,
  data = dogfish,
  spatial = "on",
  spatiotemporal = "rw",

  #offset = dogfish$offset_rho, # different offsets see above


  offset = dogfish$offset_rho_quang, # different offsets see above

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

ggplot(index, aes(year, est, ymin = lwr, ymax = upr)) +
  geom_point() +
  geom_line( linewidth = 0.1) +
  geom_linerange()

#write_csv(index, file = "analysis/index-calibration/index/index_dogfish_hbll_calibrate.csv")


# HBLL model --------------------------------------------------------------

hbll <- filter(dogfish, survey == "hbll")
mesh <- sdmTMB::make_mesh(
  hbll,
  c("UTM.lon", "UTM.lat"),
  n_knots = 100
)

fit_hbll <- sdmTMB(
  catch_count ~ 0 + factor(year), # + log_botdepth,
  #catch_count ~ 1, # + log_botdepth,
  mesh = mesh,
  data = hbll,
  spatial = "on",
  spatiotemporal = "rw",

  #offset = hbll$offset_rho,
  offset = hbll$offset_rho_quang, # different offsets see above

  time = "year",
  family = nbinom2(),
  anisotropy = TRUE
)

index_hbll <- local({
  newdata <- replicate_df(grid_hbll, "year", unique(hbll$year))
  pred <- predict(fit_hbll, newdata, return_tmb_object = TRUE)
  get_index(pred, TRUE)
})

ggplot(index_hbll, aes(year, est, ymin = lwr, ymax = upr)) +
  geom_point() +
  geom_line( linewidth = 0.1) +
  geom_linerange()

#write_csv(index_hbll, file = "analysis/index-calibration/index/index_hbll.csv")


# dog circle only ---------------------------------------------------------


# Dogfish only
dog <- filter(dogfish, survey_abbrev == "DOG")
mesh <- sdmTMB::make_mesh(
  dog,
  c("UTM.lon", "UTM.lat"),
  n_knots = 20
)

fit_dog <- sdmTMB(
  catch_count ~ 0 + factor(year), # + log_botdepth,
  mesh = mesh,
  data = dog,

  #offset = dog$offset_rho,
  offset = dog$offset_rho_quang, # different offsets see above

  time = "year",
  spatiotemporal = "iid",
  family = nbinom2(),
  anisotropy = TRUE
)

grid_dog <- gfplot::dogfish_grid$grid %>%
  mutate(UTM.lon = X / 1e3, UTM.lat = Y / 1e3)

index_dog <- local({
  newdata <- replicate_df(grid_dog, "year", unique(dog$year))
  pred <- predict(fit_dog, newdata, return_tmb_object = TRUE)
  get_index(pred, TRUE)
})

ggplot(index_dog, aes(year, est, ymin = lwr, ymax = upr)) +
  geom_point() +
  geom_line( linewidth = 0.1) +
  geom_linerange()

#write_csv(index_dog, file = "analysis/index-calibration/index/index_dog.csv")

# Compare index

index_compare1 <- rbind(
  index |> mutate(Survey = "Calibrated HBLL + SoG dogfish", value = "1.68"),
  index_hbll |> mutate(Survey = "HBLL", value = "1.68"),
  index_dog |> mutate(Survey = "dog", value = "1.68")
)

index_compare2 <- index_compare <- rbind(
  index |> mutate(Survey = "Calibrated HBLL + SoG dogfish", value = "1.19"),
  index_hbll |> mutate(Survey = "HBLL", value = "1.19"),
  index_dog |> mutate(Survey = "dog", value = "1.19")
)

index_compare <- rbind(index_compare1, index_compare2)

g <- index_compare %>%
  #mutate(dyear = year %in% year_dogfish) %>%
  ggplot(aes(year, est, ymin = lwr, ymax = upr, group = value, colour = value)) +
  geom_point() +
  geom_line(aes(group = value, colour = value), linewidth = 0.1) +
  geom_linerange() +
  facet_wrap(vars(Survey), ncol = 1, scales = "free_y") +
  expand_limits(y = 0) +
  labs(x = "Year", y = "Index")

g

ggsave ("figures/calibrated_index.jpg", g, width = 5, height = 8)

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
