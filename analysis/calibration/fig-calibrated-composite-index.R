# calibrated composite index

m <- readRDS("data-generated/model_nolength_season_bb.rds")
all <- tidy(m, ran.pars = TRUE)
log_offset <- (all$estimate)
exp(log_offset)

dat <- readRDS("data-raw/wrangled-hbll-dog-sets.rds") # need the depth fished to create pairs
years <- dat |> group_by(survey_lumped) |> reframe(year = sort(unique(year)))

exp(dat$offset)
exp(dat$offset_hksoak)
unique(dat$survey_abbrev)
x <- filter(dat, survey_abbrev == "OTHER")

# define offsets for composite indexes for all surveys

id_remove <- dat %>%
  filter(grepl("COMPARISON", activity_desc) & !year %in% c(2004, 2023)) |>
  pull(fishing_event_id)

id_remove2 <- dat %>%
  filter(grepl("COMPARISON", activity_desc) & hooksize_desc == "12/0" & year %in% c(2022, 2023, 2024)) |>
  pull(fishing_event_id)

dogfish <-
  dat |>
  filter(!fishing_event_id %in% id_remove) %>%
  filter(!fishing_event_id %in% id_remove2) %>%

  #  filter(dat, !grepl("COMPARISON", activity_desc)) %>% #I want to keep the 2004 and the 2023 comp work

  mutate(survey_abbrev = ifelse(year == 2023 & time_deployed > as.POSIXct("2023-09-06 09:15:21") & hooksize_desc == "14/0" & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS", "DOG",
    ifelse(year == 2023 & time_deployed > as.POSIXct("2023-09-06 09:15:21") & hooksize_desc == "13/0" & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS", "erase", # don't want this one
      ifelse(year == 2023 & time_deployed <= as.POSIXct("2023-09-06 09:15:21") & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS", "erase",
        ifelse(year == 2004 & hooksize_desc == "14/0" & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS", "DOG",
          ifelse(year == 2004 & hooksize_desc == "12/0" & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS", "j-hook",
            survey_abbrev
          )
        )
      )
    )
  )) |>

  filter(survey_abbrev != "erase") %>%

  mutate(survey_abbrev = ifelse(survey_abbrev == "DOG" & year %in% c(1986, 1989), "j-hook", survey_abbrev)) |>
  mutate(
    offset_jhook = offset_hksoak - ifelse(survey_abbrev == "DOG", 0,
      ifelse(survey_abbrev %in% c("HBLL INS N", "HBLL INS S"), 0, log(1.45))
    ), # 1.45 from Jackies report
    offset_rho = offset_hksoak - ifelse(survey_abbrev == "DOG", log_offset,
      ifelse(survey_abbrev %in% c("HBLL INS N", "HBLL INS S"), 0, log(exp(log_offset) * 1.45))
    ),
    offset_dogcircle = offset_hksoak + ifelse(survey_abbrev == "DOG", 0,
      ifelse(survey_abbrev %in% c("HBLL INS N", "HBLL INS S"), log_offset, -(log(1.45)))
    ), # scale to dog gear

    # cpue = catch_count / exp(log_offset),
    cpue_rho = catch_count / exp(offset_rho),
    cpue_dogcircle = catch_count / exp(offset_dogcircle)
  ) %>%
  arrange(year) %>%
  #mutate(survey = ifelse(survey_abbrev == "DOG", "dog", "hbll")) %>%
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

ggplot(dogfish, aes(year, cpue_dogcircle, colour = survey_abbrev)) +
  geom_jitter(shape = 21) +
  scale_fill_viridis_c()

# calibrated with j-hook --------------------------------------------------

dogfishj <- dogfish |> filter(survey_abbrev %in% c("DOG", "j-hook"))
# dogfishj <- dogfish
dogfishj <- dogfishj |> drop_na(offset_jhook) # offset_jhook
unique(dogfishj$year)
unique(dogfishj$survey_abbrev)
unique(dogfishj$offset_jhook)

ggplot(dogfishj, aes(year, catch_count, colour = survey_abbrev)) +
  geom_jitter(shape = 21) +
  scale_fill_viridis_c()

dogfishj <- dogfishj |> mutate(catch_cpue = catch_count / exp(offset_jhook))
range(dogfishj$catch_cpue)
weight <- exp(dogfishj$offset_jhook)
range(weight)

ggplot(dogfishj, aes(year, catch_cpue, colour = survey_abbrev)) +
  geom_jitter(shape = 21) +
  scale_fill_viridis_c()

ggplot(dogfishj, aes(year, offset_jhook, colour = survey_abbrev)) +
  geom_jitter(shape = 21) +
  scale_fill_viridis_c()

ggplot(dogfishj, aes(year, offset_hksoak, colour = survey_abbrev)) +
  geom_jitter(shape = 21) +
  scale_fill_viridis_c()

mesh <- sdmTMB::make_mesh(
  dogfishj,
  c("UTM.lon", "UTM.lat"),
  n_knots = 5
)

# dogfish gears just use the dogfish calibration
fit <- sdmTMB( # composite index
  #catch_count ~ 1, # + log_botdepth, #could include gear and hopefully the coef is zero
  catch_cpue ~ 1 ,
  mesh = mesh,
  data = dogfishj,
  spatial = "on",
  spatiotemporal = "rw",
  extra_time = c(1987, 1988, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2006, 2007, 2009, 2010, 2012, 2013, 2015, 2016, 2017, 2018),

  # offset = dogfish$offset_rho_quang, # different offsets see above

  time = "year",

  family = betabinomial(),
  weights = weight,

  #family = nbinom2(),
  #offset = dogfishj$offset_jhook,

  anisotropy = TRUE
)

sanity(fit)
tidy(fit)
exp(tidy(fit)$estimate)

grid_hbll <- rbind(
  gfplot::hbll_inside_n_grid$grid,
  gfplot::hbll_inside_s_grid$grid
) %>%
  sdmTMB::add_utm_columns(ll_names = c("X", "Y"), utm_crs = 32609, utm_names = c("UTM.lon", "UTM.lat"))


index <- local({
  newdata <- replicate_df(grid_hbll, "year", c(1986, 1989, 2004, 2005, 2008, 2011, 2014, 2019, 2023))
  pred <- predict(fit, newdata, return_tmb_object = TRUE)
  get_index(pred, TRUE)
})

index <- filter(index, year %in% c(1986, 1989, 2004, 2005, 2008, 2011, 2014, 2019, 2023))

ggplot(index, aes(year, est, ymin = lwr, ymax = upr)) +
  geom_point() +
  geom_line(linewidth = 0.1) +
  geom_linerange()


# calibrated hbll and dog j hook and circle --------------------------------------------------------------

dogfish <- dogfish |> drop_na(offset_rho)
dogfish$julian
dogfish$cpue_rho <- dogfish$catch_count / (exp(dogfish$offset_rho))
weight <- exp(dogfish$offset_rho)

dogfish <- dogfish |> drop_na(julian)

dogfish$julian_c <- dogfish$julian - mean(dogfish$julian)

mesh <- sdmTMB::make_mesh(
  dogfish,
  c("UTM.lon", "UTM.lat"),
  n_knots = 5
)

ggplot(dogfish, aes(year, cpue_rho)) +
  geom_point() +
  facet_wrap(~survey_lumped)

ggplot(dogfish, aes(year, offset_rho)) +
  geom_point() +
  facet_wrap(~survey_lumped)

compfit <- sdmTMB( # composite index

  #catch_count ~ 1 + poly(julian_c,2), # + log_botdepth,
  catch_count ~ 1, # + log_botdepth,
  #cpue_rho ~ 1 + poly(julian_c, 2), # + as.factor(survey_abbrev), #include gear and hopefully the coef is zero

  mesh = mesh,
  data = dogfish,
  spatial = "off",
  spatiotemporal = "rw",
  extra_time = c(1987, 1988, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2006, 2017, 2020),
  time = "year",

  offset = dogfish$offset_rho, # different offsets see above
  family = nbinom2(),

 #weights = weight,
 #family = betabinomial(), #couldnt' get this to converge

  anisotropy = TRUE
)

sanity(compfit)
tidy(compfit, ran.pars = TRUE)

grid_hbll <- rbind(
  gfplot::hbll_inside_n_grid$grid,
  gfplot::hbll_inside_s_grid$grid
) %>%
  sdmTMB::add_utm_columns(ll_names = c("X", "Y"), utm_crs = 32609, utm_names = c("UTM.lon", "UTM.lat"))

index <- local({
  newdata <- replicate_df(grid_hbll, "year", unique(dogfish$year))
  newdata$julian_c <- 0
  pred <- predict(compfit, newdata, return_tmb_object = TRUE)
  get_index(pred, TRUE)
})

ggplot(index, aes(year, est, ymin = lwr, ymax = upr)) +
  geom_point() +
  geom_line(linewidth = 0.1) +
  geom_linerange()

write_csv(index, file = "analysis/calibration/index/index_dogfish_calibrate.csv")
#write_csv(index, file = "analysis/calibration/index/index_dogfish_calibrate_julian.csv")


# calibrated dog j hook and circle --------------------------------------------------------------

dogfishd <- dogfish |> drop_na(offset_jhook) |> filter(survey_lumped != c("hbll"))
unique(dogfishd$year)
dogfishd$cpue_jhook <- dogfishd$catch_count / (exp(dogfishd$offset_jhook))
weight <- exp(dogfishd$offset_jhook)
dogfishd <- dogfishd |> drop_na(julian)

dogfishd$julian_c <- dogfishd$julian - mean(dogfishd$julian)

mesh <- sdmTMB::make_mesh(
  dogfishd,
  c("UTM.lon", "UTM.lat"),
  n_knots = 5
)

ggplot(dogfishd, aes(year, cpue_jhook)) +
  geom_point() +
  facet_wrap(~survey_lumped)

ggplot(dogfishd, aes(year, offset_jhook)) +
  geom_point() +
  facet_wrap(~survey_lumped)

compfit <- sdmTMB( # composite index

  #catch_count ~ 1 + poly(julian_c,2), # + log_botdepth,
  #catch_count ~ 1, # + log_botdepth,
  cpue_jhook ~ 1, # + poly(julian_c, 2), # + as.factor(survey_abbrev), #include gear and hopefully the coef is zero

  mesh = mesh,
  data = dogfishd,
  spatial = "off",
  spatiotemporal = "rw",
  extra_time = c(1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2006, 2007, 2009, 2010, 2012, 2013, 2015, 2016, 2017, 2018, 2020, 2021, 2022),
  time = "year",

  #offset = dogfish$offset_jhook, # different offsets see above
  #family = nbinom2(),

  weights = weight,
  family = betabinomial(), #couldnt' get this to converge

  anisotropy = TRUE
)

sanity(compfit)
tidy(compfit, ran.pars = TRUE)

grid_hbll <- rbind(
  gfplot::hbll_inside_n_grid$grid,
  gfplot::hbll_inside_s_grid$grid
) %>%
  sdmTMB::add_utm_columns(ll_names = c("X", "Y"), utm_crs = 32609, utm_names = c("UTM.lon", "UTM.lat"))

index_dc<- local({
  newdata <- replicate_df(grid_hbll, "year", unique(dogfishd$year))
  newdata$julian_c <- 0
  pred <- predict(compfit, newdata, return_tmb_object = TRUE)
  get_index(pred, TRUE)
})

ggplot(index_dc, aes(year, est, ymin = lwr, ymax = upr)) +
  geom_point() +
  geom_line(linewidth = 0.1) +
  geom_linerange()

write_csv(index_dc, file = "analysis/calibration/index/index_dogfish_jcirclehook_calibrate.csv")
#write_csv(index, file = "analysis/calibration/index/index_dogfish_calibrate_julian.csv")



# HBLL model --------------------------------------------------------------

hbll <- filter(dogfish, survey_abbrev %in% c("HBLL INS S", "HBLL INS N"))
range(hbll$year)
hbll <- hbll |> mutate(cpue = catch_count/exp(offset))
weight = exp(hbll$offset)


mesh <- sdmTMB::make_mesh(
  hbll,
  c("UTM.lon", "UTM.lat"),
  n_knots = 15
)

fit_hbll <- sdmTMB(
  # catch_count ~ 1 + factor(year), # + log_botdepth,
  cpue ~ 1, # + log_botdepth,
  mesh = mesh,
  data = hbll,
  spatial = "on",
  spatiotemporal = "rw",

  #offset = hbll$offset,
  #family = nbinom2(),

  family = betabinomial(),
  weights = weight,

  time = "year",
  anisotropy = TRUE
)

sanity(fit_hbll)

index_hbll <- local({
  newdata <- replicate_df(grid_hbll, "year", unique(hbll$year))
  pred <- predict(fit_hbll, newdata, return_tmb_object = TRUE)
  get_index(pred, TRUE)
})

ggplot(index_hbll, aes(year, est, ymin = lwr, ymax = upr)) +
  geom_point() +
  geom_line(linewidth = 0.1) +
  geom_linerange()

write_csv(index_hbll, file = "analysis/calibration/index/index_hbll.csv")


# dog circle only ---------------------------------------------------------


# Dogfish only
dog <- filter(dogfish, survey_abbrev == "DOG")
range(dog$year)
dog <- dog |> mutate(cpue = catch_count/exp(offset))
weight = exp(dog$offset)

mesh <- sdmTMB::make_mesh(
  dog,
  c("UTM.lon", "UTM.lat"),
  n_knots = 5
)

fit_dog <- sdmTMB(
  #catch_count ~ 1, # + log_botdepth,
  cpue ~ 1, # + log_botdepth,
  mesh = mesh,
  data = dog,

  #offset = dog$offset_rho,
  #family = nbinom2(),

  weights = weight,
  family = betabinomial(),

  time = "year",
  spatial = "on",
  spatiotemporal = "rw",
  anisotropy = TRUE
)

sanity(fit_dog)

grid_dog <- gfplot::dogfish_grid$grid %>%
  mutate(UTM.lon = X / 1e3, UTM.lat = Y / 1e3)

index_dog <- local({
  newdata <- replicate_df(grid_dog, "year", unique(dog$year))
  pred <- predict(fit_dog, newdata, return_tmb_object = TRUE)
  get_index(pred, TRUE)
})

ggplot(index_dog, aes(year, est, ymin = lwr, ymax = upr)) +
  geom_point() +
  geom_line(linewidth = 0.1) +
  geom_linerange()

write_csv(index_dog, file = "analysis/calibration/index/index_dog.csv")
#write_csv(index_dog, file = "analysis/calibration/index/index_dog_julian.csv")



# compare indices ---------------------------------------------------------


index_compare <- rbind(   #seasonally paired value and center data
  index |> mutate(Survey = "Calibrated HBLL + SoG dogfish") |>
    mutate(est_c = scale(est, center = TRUE, scale = TRUE), lwr_c = (lwr - mean(est))/sd(est), upr_c = (upr - mean(est))/sd(est)),
  index_hbll |> mutate(Survey = "HBLL") |>
    mutate(est_c = scale(est, center = TRUE, scale = TRUE), lwr_c = (lwr - mean(est))/sd(est), upr_c = (upr - mean(est))/sd(est)),
  #index_dog |> mutate(Survey = "SoG dogfish (circle)") |>
  #  mutate(est_c = scale(est, center = TRUE, scale = TRUE), lwr_c = (lwr - mean(est))/sd(est), upr_c = (upr - mean(est))/sd(est)),
  index_dc |> mutate(Survey = "Calibrated SoG dogfish") |>
    mutate(est_c = scale(est, center = TRUE, scale = TRUE), lwr_c = (lwr - mean(est))/sd(est), upr_c = (upr - mean(est))/sd(est))

)

# index_compare2 <- index_compare <- rbind(
#   index |> mutate(Survey = "Calibrated HBLL + SoG dogfish (circle and j-hook)", value = "1.19"), #paired value
#   index_hbll |> mutate(Survey = "HBLL", value = "1.19"),
#   index_dog |> mutate(Survey = "SoG dogfish", value = "1.19")
# )

#index_compare <- rbind(index_compare, index_compare2)

x <- palette.colors(palette = "Okabe-Ito")

gg <- index_compare %>%
  # mutate(dyear = year %in% year_dogfish) %>%
  #ggplot(aes(year, est, ymin = lwr, ymax = upr, group = Survey, colour = Survey)) +
  ggplot(aes(year, est_c, ymin = lwr_c, ymax = upr_c, group = Survey, colour = Survey)) +
  geom_line(aes(group = Survey, colour = Survey), linewidth = 1) +
  geom_point(aes(group = Survey, colour = Survey), size = 2) +
  geom_ribbon(aes(year, est_c, ymin = lwr_c, ymax = upr_c, fill = Survey), alpha = 0.5, guides = NULL) +
  #geom_linerange() +
  #facet_wrap(vars(Survey), ncol = 1, scales = "free_y") +
  expand_limits(y = 0) +
  scale_colour_manual(values = x[c(2,4,7)]) +
  scale_fill_manual(values = x[c(2,4,7)]) +
  labs(x = "Year", y = "Index") +
  theme_classic()

ggsave("figures/calibrated_index.jpg", gg, width = 6, height =3)

# index_compare <- rbind(
#   read.csv(file = "analysis/calibration/index/index_dogfish_hbll_calibrate.csv") %>%
#     mutate(Survey = "Calibrated HBLL + SoG dogfish"),
#   read.csv(file = "analysis/calibration/index/index_dog.csv") %>%
#     mutate(Survey = "SoG dogfish"),
#   read.csv(file = "analysis/calibration/index/index_hbll.csv") %>%
#     mutate(Survey = "HBLL")
# )
#
# year_dogfish <- index_compare %>%
#   filter(Survey == "SoG dogfish") %>%
#   pull(year)
#
# gg <- index_compare %>%
#   mutate(dyear = year %in% year_dogfish) %>%
#   ggplot(aes(year, est, ymin = lwr, ymax = upr, colour = dyear)) +
#   geom_point() +
#   geom_line(aes(group = Survey), linewidth = 0.1) +
#   geom_linerange() +
#   facet_wrap(vars(Survey), ncol = 1, scales = "free_y") +
#   expand_limits(y = 0) +
#   labs(x = "Year", y = "Index", colour = "Year with \nSoG dogfish survey?")
# ggsave("analysis/index-calibration/index/compare_index.png", g, height = 6, width = 5)
