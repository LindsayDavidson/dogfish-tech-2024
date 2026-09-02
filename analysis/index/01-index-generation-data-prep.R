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
coeffs <- readRDS("output/calibration_coeffs_depth.rds") #
log_est_221 <- rnorm(n = 1, mean = coeffs$estimate[coeffs$term == "factor(depth_bin) > 220 m"], sd = coeffs$std.error[coeffs$term == "factor(depth_bin) > 220 m"])
log_est_165 <- rnorm(n = 1, mean = coeffs$estimate[coeffs$term == "factor(depth_bin) 111 - 165 m"], sd = coeffs$std.error[coeffs$term == "factor(depth_bin) 111 - 165 m"])
log_est_220 <- rnorm(n = 1, mean = coeffs$estimate[coeffs$term == "factor(depth_bin) 166 - 220 m"], sd = coeffs$std.error[coeffs$term == "factor(depth_bin) 166 - 220 m"])
log_est_110 <- rnorm(n = 1, mean = coeffs$estimate[coeffs$term == "factor(depth_bin) 56 - 110 m"], sd = coeffs$std.error[coeffs$term == "factor(depth_bin) 56 - 110 m"])

coeffs$estc <- c(log_est_221, log_est_220, log_est_165, log_est_110)

coeffs <- coeffs |> mutate(depth_bin = c("D5", "D3", "D4", "D2"))

dogc <- dogfish |>
  filter(survey_abbrev %in% c("DOG", "OTHER") & hooksize_desc == "14/0" & year %in% c(2004, 2005, 2008, 2011, 2014, 2019, 2023)) |>
  filter(depth_bin != "D1") |>
  ungroup()

sort(unique(dogc$year))
unique(dogc$depth_bin)

#nsim <- 1 # 20
#pull from a distribtuion of coeffs

#dogc <- left_join(dogc, coeffs)
#unique(dogc$depth_bin)
#unique(dogc$estc)


# DOG j hook - add calibration by depth -----------------------------------

dogj <- dogfish |>
  filter(survey_abbrev %in% c("j-hook")) |>
  ungroup()

sort(unique(dogj$year))
unique(dogj$depth_bin)

depths <- data.frame(depth_bin = c("D1", "D2", "D3", "D4", "D5"))
depths$estj <- c(log(1.2), log(1.2), log(1.65), log(1.65), log(1.65))

#dogj <- left_join(dogj, depths)
#unique(dogj$depth_bin)
#str(dogj$estj)


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

#put data together and then modify the offsets
d <- bind_rows(dogc, dogj)
d <- bind_rows(d, hbll)

d <- left_join(d, depths, by = "depth_bin")
d <- left_join(d, coeffs, by = "depth_bin")

glimpse(d)

d <- d %>%
  mutate(
    offset_jhook = offset_hksoak - ifelse(survey_abbrev %in% c("DOG", "OTHER"), 0,
                                          ifelse(survey_abbrev %in% c("HBLL INS N", "HBLL INS S"), 0, (estj)) #log(1.45))
    ), # 1.45 from Jackies report
    offset_rho = offset_hksoak - ifelse(survey_abbrev %in% c("DOG", "OTHER"), (estc),
                                        ifelse(survey_abbrev %in% c("HBLL INS N", "HBLL INS S"), 0, log(exp(estc) * exp(estj)))
    ),
    #offset_dogcircle = offset_hksoak + ifelse(survey_abbrev == "DOG", 0,
    #  ifelse(survey_abbrev %in% c("HBLL INS N", "HBLL INS S"), log_offset, -(log(1.45)))
    #), # scale to dog gear

    # cpue = catch_count / exp(log_offset),
    cpue_rho = catch_count / exp(offset_rho),
    #cpue_dogcircle = catch_count / exp(offset_dogcircle)
  ) %>%
  arrange(year) %>%
  # mutate(survey = ifelse(survey_abbrev == "DOG", "dog", "hbll")) %>%
  select(!UTM.lon & !UTM.lat) %>%
  sdmTMB::add_utm_columns(ll_names = c("longitude", "latitude"), utm_crs = 32609, utm_names = c("UTM.lon", "UTM.lat"))

saveRDS(d, "output/data-index-generation-calibrated.rds")

# ggplot(d, aes(longitude, latitude, fill = cpue_rho)) +
#   geom_point(shape = 21) +
#   facet_grid(vars(year), vars(survey_abbrev)) +
#   scale_fill_viridis_c(trans = "sqrt")
#
# ggplot(d, aes(depth_m, cpue_rho)) +
#   geom_point(shape = 21) +
#   facet_grid(vars(survey_abbrev)) +
#   scale_fill_viridis_c()
#
# ggplot(d, aes(depth_m, offset_rho)) +
#   geom_point(shape = 21) +
#   facet_grid(vars(survey_abbrev)) +
#   scale_fill_viridis_c()
#
# ggplot(d, aes(depth_m, offset)) +
#   geom_point(shape = 21) +
#   facet_grid(vars(survey_abbrev)) +
#   scale_fill_viridis_c()
#
# ggplot(d, aes(year, catch_count, colour = survey_abbrev)) +
#   geom_jitter(shape = 21) +
#   scale_fill_viridis_c()

# ggplot(d, aes(year, cpue_dogcircle, colour = survey_abbrev)) +
#   geom_jitter(shape = 21) +
#   scale_fill_viridis_c()
