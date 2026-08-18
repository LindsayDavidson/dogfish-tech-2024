#change the databse , add length groups and then have the catch count per group
#need the samps database
#ignore sex for now
samps <- readRDS("output/samps_joined.rds")

samps <- samps %>%
  filter(grepl("COMPARISON", activity_desc))

id_remove <- samps %>% filter(hook_desc == "J-HOOK") %>% pull(fishing_event_id)

samps <- filter(samps, !fishing_event_id %in% id_remove) %>%
  arrange(year, fishing_event_id, survey_abbrev)

samps <- samps |>
  filter(!year %in% c(2004, 2019, 2024)) |>
  mutate(hook_name = ifelse(hooksize_desc == "13/0", "HBLL_13/0", "Dog_14/0")) |>
  dplyr::select(hook_name, sex, fishing_event_id, year, length, time_deployed, lglsp_hook_count, latitude, longitude, depth_m)

samps <- samps |> mutate(survey_abbrev = ifelse(time_deployed > "2023-09-06 09:15:21", "Dog_14/0", hook_name))

min <- min(samps$length, na.rm = TRUE)
max <- max(samps$length, na.rm = TRUE)

breaks <- c(seq(min, max, by = 10),112)
breaks <- c(min-1, 64, 84, max+1) #change to a 80 plus length bin

test <- samps |>  group_by(hook_name) |> mutate(length_bin = cut(length, breaks)) |> drop_na(length) |> ungroup()
test2 <- test |> group_by(length_bin, hook_name, fishing_event_id, year) |>
  reframe(catch_count = n(), offset = mean(lglsp_hook_count)) |>
  drop_na(length_bin) |>
  ungroup()

#df_dog <- filter(test2, hook_name == "Dog_14/0")
#df_hbll <- filter(test2, hook_name == "HBLL_13/0")

test3 <- test |> left_join(test2) |> dplyr::select(-length, -sex) |> distinct()

test4 <- test3 |>
  #filter(hook_name == "Dog_14/0") |>
  select(year, length_bin, fishing_event_id, latitude, longitude, depth_m, hook_name, catch_count) %>%
  distinct() |>
  group_by(fishing_event_id) |>
  pivot_wider(
    names_from = hook_name    ,
    values_from = catch_count
  ) |>
  rename(catch_count_hbll = "HBLL_13/0", catch_count_dog = "Dog_14/0") |>
  ungroup()

test5 <- test3 |>
  select(year, fishing_event_id, lglsp_hook_count, hook_name) %>%
  distinct() |>
  mutate(offset_name = ifelse(hook_name == "HBLL_13/0", "offset_hbll", "offset_dog")) |>
  dplyr::select(-hook_name) |>
  group_by(fishing_event_id) |>
  pivot_wider(
    names_from = offset_name    ,
    values_from = lglsp_hook_count
  ) |>
  ungroup() |>
  arrange(year, fishing_event_id) |>
  mutate(offset = log(offset_hbll) - log(offset_dog))

final <- left_join(test4, test5)

#change to UTMs

final <- add_utm_columns(
  final,
  ll_names = c("longitude", "latitude"),
  ll_crs = 4326,
  utm_names = c("UTM.lat", "UTM.lon"))


#model run for length based calibration coefficient
dummy_mesh <- sdmTMB::make_mesh(final, c("UTM.lon", "UTM.lat"), n_knots = 10)

final <- final |> mutate(cpue_hbll = catch_count_hbll/exp(offset), cpue_dog = catch_count_dog/exp(offset))
final$cpue_dog
final$catch_count_dog
final <- final |> drop_na()
weight = exp(final$offset)
dummy_mesh <- sdmTMB::make_mesh(final, c("UTM.lon", "UTM.lat"), n_knots = 10)

m2 <- sdmTMB(
  #cbind(catch_count_hbll , catch_count_dog ) ~ 0 + factor(length_bin), #length calibration coeff, then each catch should be for a length bin
  cbind(cpue_hbll , cpue_dog ) ~ 0 + factor(length_bin),
  mesh = dummy_mesh,
  spatial = "off",
  spatiotemporal = "off",
  data = final,
  #offset = final$offset,
  #family = binomial(),
  family = betabinomial(),
  weights = weight,
  control = sdmTMBcontrol(multiphase = FALSE)
)

AIC(m2)
coef(m2)
q1 <- coef(m2)["factor(length_bin)(43,64]"]
exp(q1) #1.74 for smallest length bin
q2 <- coef(m2)["factor(length_bin)(64,84]"]
exp(q2)
q3 <- coef(m2)["factor(length_bin)(84,113]"]
exp(q3)

all <- sdmTMB(
  #cbind(catch_count_hbll , catch_count_dog ) ~ 1,
  cbind(cpue_hbll , cpue_dog ) ~ 1,
  mesh = dummy_mesh,
  spatial = "off",
  spatiotemporal = "off",
  data = final,
  #offset = final$offset,
  #family = binomial(),
  family = betabinomial(),
  weights = weight,
  control = sdmTMBcontrol(multiphase = FALSE)
)

coef(all)
all2 <- tidy(all, ran.pars = TRUE)


#log_offset = log(mean(c(exp(q1), exp(q2), exp(q3))))
#log_offset
#exp(log_offset) #68 percent more fish caught on HBLL

length <- data.frame("Length (cm)" = c("43-64", "64-84", "84-113", "all data"))
q <- data.frame("rho" = round(c(exp(q1), exp(q2), exp(q3), exp(all$estimate)), 2))
table <- cbind(length, q)
table$conf.low = c(round(exp(tidy(m2)$conf.low),2), round(exp(all2$conf.low),2))
table$conf.high = c(round(exp(tidy(m2)$conf.high),2), round(exp(all2$conf.high),2))
table$CI <- paste0((table$conf.low), "-", (table$conf.high))
table <- table |> mutate(final = paste0(rho, " (", conf.low, "-", conf.high, ")"))
table <- table |> dplyr::select("Length..cm.", final )
rownames(table) <- NULL

table |>
  knitr::kable(
    format = "latex",
    col.names = c(
      "Length bin (cm)", "rho (CI)"),
    booktabs = TRUE,
    align = "c",
    caption = "rho for seasonally paired comparative sets",
    label = "length_calibration_season"
  ) |>
  #kableExtra::row_spec(0, bold = TRUE, color = "white", background = "grey")  |>  # header styling
  kableExtra::kable_styling(full_width = FALSE)


