# generate calibrated composite indices


#can I iterated this 50 times to get 50 different calibration coefficients and curves

index_all <- data.frame()

for (i in c(1:50)){
set.seed(575+ i*7)
source("analysis/index/01-index-generation-data-prep.R") #each time I call this different values are pulled from the calibration coeff distribution
print(range(d$estc, na.rm = TRUE))

dogfish <- d

# calibrated hbll and dog j hook and circle --------------------------------------------------------------

dogfish <- dogfish |> drop_na(offset_rho)
dogfish$julian
dogfish$cpue_rho <- dogfish$catch_count / (exp(dogfish$offset_rho))
weight <- exp(dogfish$offset_rho)

dogfish <- dogfish |> drop_na(julian)
dogfish <- dogfish |> drop_na(cpue_rho_mean)


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

#compfitd <- update(compfit, formula = catch_count ~ 1 + s(depth_m))
#AIC(compfitd)

AIC(compfit)

grid_hbll <- rbind(
  gfplot::hbll_inside_n_grid$grid,
  gfplot::hbll_inside_s_grid$grid
) %>%
  sdmTMB::add_utm_columns(ll_names = c("X", "Y"), utm_crs = 32609, utm_names = c("UTM.lon", "UTM.lat"))

s <- sanity(compfit)

if (!s$gradients_ok) {
  index <- local({
    newdata <- replicate_df(grid_hbll, "year", unique(dogfish$year))
    newdata$julian_c <- 0
    pred <- predict(compfit, newdata, return_tmb_object = TRUE)
    get_index(pred, TRUE)
    index$iter <- paste0("iter_", i)
    index_all <- bind_rows(index_all, index)
    write_csv(index_all, file = "data-generated/index_dogfish_calibrate.csv")
      })
}

# ggplot(index, aes(year, est, ymin = lwr, ymax = upr)) +
#   geom_point() +
#   geom_line(linewidth = 0.1) +
#   geom_linerange()

rm(d, dogfish, compfit, mesh, index, weight)
gc()
}






# Models outside of the loop (jhook/circle hook, hbll) --------------------

source("analysis/index/01-index-generation-data-prep.R")

dogfish <- d
test <- dogfish |> dplyr::select(catch_count, offset_rho_mean, survey_abbrev, year, cpue_rho_mean)
range(d$cpue_rho)
dogfish <- dogfish |> drop_na(cpue_rho_mean)
dogfish <- dogfish |> drop_na(offset_rho_mean)

# calibrated dogfish circle, jhook, and hbll
ggplot(dogfish, aes(year, cpue_rho_mean, colour = survey_abbrev)) +
  geom_jitter(shape = 21) +
  scale_fill_viridis_c()


cpue_rho_mean <- dogfish$catch_count/exp(dogfish$offset_rho_mean)
range(dogfish$cpue_rho_mean)
range(cpue_rho_mean)
weight <- exp(dogfish$offset_rho_mean)
range(weight)

ggplot(dogfish, aes(year, catch_count, colour = survey_abbrev)) +
  geom_jitter(shape = 21) +
  scale_fill_viridis_c()

ggplot(dogfish, aes(year, cpue_rho_mean, colour = survey_abbrev)) +
  geom_jitter(shape = 21) +
  scale_fill_viridis_c()

ggplot(dogfish, aes(year, offset_rho_mean, colour = survey_abbrev)) +
  geom_jitter(shape = 21) +
  scale_fill_viridis_c()

mesh <- sdmTMB::make_mesh(
  dogfish,
  c("UTM.lon", "UTM.lat"),
  n_knots = 5
)



# dogfish gears just use the dogfish calibration
fit <- sdmTMB(
  #catch_count ~ 1, # + log_botdepth, #could include gear and hopefully the coef is zero
  catch_count ~ 1 ,
  mesh = mesh,
  data = dogfish,
  spatial = "on",
  spatiotemporal = "rw",
  extra_time = c(1987, 1988, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2006, 2017, 2020),
  time = "year",

  family = nbinom2(),
  offset = dogfish$offset_rho_mean,

  #family = betabinomial(), #couldnt get this to converge
  #weights = weight,

  anisotropy = FALSE
)

sanity(fit)
tidy(fit)

#fitd <- update(fit, formula = catch_cpue ~ 1 + s(depth_m))
#AIC(fitd)
#AIC(fit)

grid_hbll <- rbind(
  gfplot::hbll_inside_n_grid$grid,
  gfplot::hbll_inside_s_grid$grid
) %>%
  sdmTMB::add_utm_columns(ll_names = c("X", "Y"), utm_crs = 32609, utm_names = c("UTM.lon", "UTM.lat"))

s <- sanity(fit)

if (!s$gradients_ok) {
  index <- local({
  newdata <- replicate_df(grid_hbll, "year", c(1986 ,1989, 2003, 2004, 2005, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2018, 2019, 2021, 2022, 2023, 2024, 2025))
    pred <- predict(fit, newdata, return_tmb_object = TRUE)
    get_index(pred, TRUE)
  })
}

#index <- filter(index, year %in% c(1986, 1989, 2004, 2005, 2008, 2011, 2014, 2019, 2023))
index$iter <- paste0("iter_", 1)
write_csv(index, file = "data-generated/index_dogfish_jcirclehookhbll_mean.csv")


# ggplot(index, aes(year, est, ymin = lwr, ymax = upr)) +
#   geom_point() +
#   geom_line(linewidth = 0.1) +
#   geom_linerange()



# calibrated dogfish circle with j-hook --------------------------------------------------

dogfishj <- dogfish |> filter(survey_abbrev %in% c("DOG", "j-hook", "OTHER"))
dogfishj <- dogfishj |> drop_na(offset_jhook) # offset_jhook
# unique(dogfishj$year)
# unique(dogfishj$survey_abbrev)
# unique(dogfishj$offset_jhook)

# ggplot(dogfishj, aes(year, catch_count, colour = survey_abbrev)) +
#   geom_jitter(shape = 21) +
#   scale_fill_viridis_c()

dogfishj <- dogfishj |> mutate(catch_cpue = catch_count / exp(offset_jhook))
range(dogfishj$catch_cpue)
weight <- exp(dogfishj$offset_jhook)
range(weight)

# ggplot(dogfishj, aes(year, catch_cpue, colour = survey_abbrev)) +
#   geom_jitter(shape = 21) +
#   scale_fill_viridis_c()
#
# ggplot(dogfishj, aes(year, offset_jhook, colour = survey_abbrev)) +
#   geom_jitter(shape = 21) +
#   scale_fill_viridis_c()
#
# ggplot(dogfishj, aes(year, offset_hksoak, colour = survey_abbrev)) +
#   geom_jitter(shape = 21) +
#   scale_fill_viridis_c()

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

#fitd <- update(fit, formula = catch_cpue ~ 1 + s(depth_m))
#AIC(fitd)
#AIC(fit)

grid_hbll <- rbind(
  gfplot::hbll_inside_n_grid$grid,
  gfplot::hbll_inside_s_grid$grid
) %>%
  sdmTMB::add_utm_columns(ll_names = c("X", "Y"), utm_crs = 32609, utm_names = c("UTM.lon", "UTM.lat"))

if (!s$gradients_ok) {
index <- local({
  newdata <- replicate_df(grid_hbll, "year", c(1986, 1989, 2004, 2005, 2008, 2011, 2014, 2019, 2023))
  pred <- predict(fit, newdata, return_tmb_object = TRUE)
  index <- get_index(pred, TRUE)
  index <- filter(index, year %in% c(1986, 1989, 2004, 2005, 2008, 2011, 2014, 2019, 2023))
  write_csv(index, file = "data-generated/index_dogfish_jcirclehook_calibrate.csv")
  })
}


# ggplot(index, aes(year, est, ymin = lwr, ymax = upr)) +
#   geom_point() +
#   geom_line(linewidth = 0.1) +
#   geom_linerange()

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


index_hbll$iter <- paste0("iter_", 1)
write_csv(index_hbll, file = "data-generated/index_hbll.csv")


