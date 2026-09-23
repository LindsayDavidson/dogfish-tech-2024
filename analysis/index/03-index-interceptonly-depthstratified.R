# divide datasets by depth and length to apply calibration coefficients

dat <- readRDS("data-raw/wrangled-hbll-dog-sets.rds") # need the depth fished to create pairs
unique(dat$grouping_desc)

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
  mutate(survey_abbrev = ifelse(year == 2023 & time_deployed > as.POSIXct("2023-09-06 09:15:21") & hooksize_desc == "14/0" & activity_desc == "DOGFISH GEAR/TIMING   COMPARISON SURVEYS", "DOG",
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
  mutate(survey_abbrev = ifelse(survey_abbrev == "DOG" & year %in% c(1986, 1989), "j-hook", survey_abbrev))

sort(unique(dogfish$year))
sort(unique(dogfish$grouping_desc))
dogfish <- dogfish |> mutate(depth_bin = ifelse(depth_m < 56, "D1",
  ifelse(depth_m >= 56 & depth_m <= 110, "D2",
    ifelse(depth_m >= 111 & depth_m <= 165, "D3",
      ifelse(depth_m >= 166 & depth_m <= 220, "D4",
        ifelse(depth_m > 220, "D5", NA)
      )
    )
  )
))
unique(dogfish$depth_bin)
unique(dogfish$survey_abbrev)
test <- filter(dogfish, survey_abbrev == "OTHER")
test <- filter(dogfish, year == 2004)

# DOG circle add calibration coeff to length data -------------------------
# depth stratified coeffs
coeffs <- readRDS("output/calibration_coeffs_depth.rds") #
coeffs <- coeffs |> mutate(depth_bin = c("D5", "D3", "D4", "D2"))

# intercept only coeffs
coeffs_int <- readRDS("output/calibration_coeffs_intercept.rds")
coeffs_int <- cbind(coeffs_int, depth_bin = NA)
coeffs$est_int <- coeffs_int$estimate


dogc <- dogfish |>
  filter(survey_abbrev %in% c("DOG", "OTHER") & hooksize_desc == "14/0" & year %in% c(2004, 2005, 2008, 2011, 2014, 2019, 2023)) |>
  filter(depth_bin != "D1") |>
  ungroup()

sort(unique(dogc$year))
unique(dogc$depth_bin)

# nsim <- 1 # 20
# pull from a distribtuion of coeffs

# dogc <- left_join(dogc, coeffs)
# unique(dogc$depth_bin)
# unique(dogc$estc)


# DOG j hook - add calibration by depth -----------------------------------

dogj <- dogfish |>
  filter(survey_abbrev %in% c("j-hook")) |>
  ungroup()

sort(unique(dogj$year))
unique(dogj$depth_bin)

depths <- data.frame(depth_bin = c("D1", "D2", "D3", "D4", "D5"))
depths$estj <- c(log(1.2), log(1.2), log(1.65), log(1.65), log(1.65))
depths$estj_int <- c(log(1.45))

# dogj <- left_join(dogj, depths)
# unique(dogj$depth_bin)
# str(dogj$estj)


# put data  together ------------------------------------------------------

hbll <- dogfish |> filter(survey_abbrev %in% c("HBLL INS N", "HBLL INS S"))

hbll <- hbll |> mutate(depth_bin = ifelse(depth_m < 56, "D1",
  ifelse(depth_m >= 56 & depth_m <= 110, "D2",
    ifelse(depth_m >= 111 & depth_m <= 165, "D3",
      ifelse(depth_m >= 166 & depth_m <= 220, "D4",
        ifelse(depth_m > 220, "D5", NA)
      )
    )
  )
))

# put data together and then modify the offsets
d <- bind_rows(dogc, dogj)
d <- bind_rows(d, hbll)

d <- left_join(d, depths, by = "depth_bin")
d <- left_join(d, coeffs, by = "depth_bin")
range(d$estimate, na.rm = TRUE)
range(d$estj_int, na.rm = TRUE)
range(d$estj, na.rm = TRUE)
range(d$est_int, na.rm = TRUE)

d <- d %>%
  mutate(
    offset_jhook = offset_hksoak - ifelse(survey_abbrev %in% c("DOG", "OTHER"), 0,
      ifelse(survey_abbrev %in% c("HBLL INS N", "HBLL INS S"), 0, (estj)) # log(1.45))
    ), # 1.45 from Jackies report
   offset_rho_depth = offset_hksoak - ifelse(survey_abbrev %in% c("DOG", "OTHER"), (estimate),
      ifelse(survey_abbrev %in% c("HBLL INS N", "HBLL INS S"), 0, log(exp(estimate) * exp(estj)))
    ),
    offset_rho_int = offset_hksoak - ifelse(survey_abbrev %in% c("DOG", "OTHER"), (est_int),
                                             ifelse(survey_abbrev %in% c("HBLL INS N", "HBLL INS S"), 0, log(exp(est_int) * exp(estj)))
    ),

    cpue_rho_depth = catch_count / exp(offset_rho_depth),
    cpue_rho_int = catch_count / exp(offset_rho_int),


  ) %>%
  arrange(year) %>%
  # mutate(survey = ifelse(survey_abbrev == "DOG", "dog", "hbll")) %>%
  select(!UTM.lon & !UTM.lat) %>%
  sdmTMB::add_utm_columns(ll_names = c("longitude", "latitude"), utm_crs = 32609, utm_names = c("UTM.lon", "UTM.lat"))

saveRDS(d, "output/data-index-generation-depth-int-coefficients.rds")

# index generation with int and depth coefficients ------------------------

d <- readRDS("output/data-index-generation-depth-int-coefficients.rds")
test <- d |> dplyr::select(catch_count, offset_rho_depth, offset_rho_int, survey_abbrev, year, cpue_rho_depth, cpue_rho_int)
range(d$cpue_rho)
d <- d |> drop_na(cpue_rho_depth)
d <- d |> drop_na(offset_rho_depth)

# calibrated dogfish circle, jhook, and hbll
ggplot(d, aes(year, cpue_rho_depth, colour = survey_abbrev)) +
  geom_jitter(shape = 21) +
  scale_fill_viridis_c()

weight <- exp(d$offset_rho_depth)
range(weight)


mesh <- sdmTMB::make_mesh(
  d,
  c("UTM.lon", "UTM.lat"),
  n_knots = 5
)

# depth
fit <- sdmTMB(
  #catch_count ~ 1, # + log_botdepth, #could include gear and hopefully the coef is zero
  catch_count ~ 1 ,
  mesh = mesh,
  data = d,
  spatial = "on",
  spatiotemporal = "rw",
  extra_time = c(1987, 1988, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2006, 2017, 2020),
  time = "year",

  family = nbinom2(),
  offset = d$offset_rho_depth,

  #family = betabinomial(), #couldnt get this to converge
  #weights = weight,

  anisotropy = FALSE
)

sanity(fit)
tidy(fit)

# int
fit_int <- sdmTMB(
  #catch_count ~ 1, # + log_botdepth, #could include gear and hopefully the coef is zero
  catch_count ~ 1 ,
  mesh = mesh,
  data = d,
  spatial = "on",
  spatiotemporal = "rw",
  extra_time = c(1987, 1988, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2006, 2017, 2020),
  time = "year",

  family = nbinom2(),
  offset = d$offset_rho_int,

  #family = betabinomial(), #couldnt get this to converge
  #weights = weight,

  anisotropy = FALSE
)

sanity(fit_int)
tidy(fit_int)

grid_hbll <- rbind(
  gfplot::hbll_inside_n_grid$grid,
  gfplot::hbll_inside_s_grid$grid
) %>%
  sdmTMB::add_utm_columns(ll_names = c("X", "Y"), utm_crs = 32609, utm_names = c("UTM.lon", "UTM.lat"))

AIC(fit)
AIC(fit_int)

s <-fits <- sanity(fit)
s2 <- sanity(fit_int)

if (s$gradients_ok) {
  index <- local({
    newdata <- replicate_df(grid_hbll, "year", c(1986 ,1989, 2003, 2004, 2005, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2018, 2019, 2021, 2022, 2023, 2024, 2025))
    pred <- predict(fit, newdata, return_tmb_object = TRUE)
    get_index(pred, TRUE)
  })
}

if (s2$gradients_ok) {
  index_int <- local({
    newdata <- replicate_df(grid_hbll, "year", c(1986 ,1989, 2003, 2004, 2005, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2018, 2019, 2021, 2022, 2023, 2024, 2025))
    pred <- predict(fit_int, newdata, return_tmb_object = TRUE)
    get_index(pred, TRUE)
  })
}

index <- #filter(index, year %in% c(1986, 1989, 2004, 2005, 2008, 2011, 2014, 2019, 2023))
  index|> mutate(id = "depth")
index_int <- #filter(index_int, year %in% c(1986, 1989, 2004, 2005, 2008, 2011, 2014, 2019, 2023))
  index_int|> mutate(id = "int")
indc <- rbind(index, index_int)

saveRDS(indc, file = "data-generated/index_dogfish_depth_int.rds")


# ggplot(index, aes(year, est, ymin = lwr, ymax = upr)) +
#   geom_point() +
#   geom_line(linewidth = 0.1) +
#   geom_linerange()

