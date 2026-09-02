library(dplyr)
library(tidyr)
library(sdmTMB)
library(ggplot2)

samps <- readRDS("output/samps_joined.rds")

samps <- samps %>%
  filter(grepl("COMPARISON", activity_desc))

id_remove <- samps %>% filter(hook_desc == "J-HOOK") %>% pull(fishing_event_id)

samps <- filter(samps, !fishing_event_id %in% id_remove) %>%
  arrange(year, fishing_event_id, survey_abbrev)

samps <- samps |>
  filter(year %in% c(2022, 2023)) |>
  mutate(hook_name = ifelse(hooksize_desc == "13/0", "HBLL_13/0", "Dog_14/0")) |>
  dplyr::select(hook_name, sex, fishing_event_id, year, length, time_deployed, lglsp_hook_count, latitude, longitude, depth_m, grouping_depth_id)

samps <- samps |> mutate(survey_abbrev = ifelse(time_deployed > "2023-09-06 09:15:21", "Dog_14/0", hook_name))
unique(samps$grouping_depth_id)

min <- min(samps$length, na.rm = TRUE)
max <- max(samps$length, na.rm = TRUE)

breaks <- c(seq(min, max, by = 10),112)
breaks <- c(min-1, 64, 84, max+1) #change to a 80 plus length bin

test <- samps |>  group_by(hook_name) |> mutate(length_bin = cut(length, breaks)) |> drop_na(length) |> ungroup()

test2 <- test |> group_by(length_bin, hook_name, fishing_event_id, year) |>
  reframe(catch_count_length = n(), offset = mean(lglsp_hook_count)) |>
  drop_na(length_bin) |>
  ungroup()

#df_dog <- filter(test2, hook_name == "Dog_14/0")
#df_hbll <- filter(test2, hook_name == "HBLL_13/0")

test3 <- test |> left_join(test2) |> dplyr::select(-length, -sex) |> distinct()

test4 <- test3 |>
  #filter(hook_name == "Dog_14/0") |>
  select(year, length_bin, fishing_event_id, latitude, longitude, depth_m, hook_name, catch_count_length) %>%
  distinct() |>
  group_by(fishing_event_id) |>
  pivot_wider(
    names_from = hook_name    ,
    values_from = catch_count_length
  ) |>
  rename(catch_count_hbll_length = "HBLL_13/0", catch_count_dog_length = "Dog_14/0") |>
  ungroup()


depth <- samps |> group_by(hook_name, fishing_event_id, year, grouping_depth_id) |>
  reframe(catch_count = n(), offset = mean(lglsp_hook_count)) |>
  ungroup() |>
  pivot_wider(
    names_from = hook_name    ,
    values_from = c(catch_count, offset)
  ) |>
  ungroup() |>
  arrange(year, fishing_event_id) |>
  drop_na("offset_HBLL_13/0", "offset_Dog_14/0") |>
  rename(offset_HBLL = "offset_HBLL_13/0" , offset_Dog = "offset_Dog_14/0") |>
  rename(catch_count_HBLL = "catch_count_HBLL_13/0" , catch_count_Dog = "catch_count_Dog_14/0") |>
  mutate(offset = log(offset_HBLL) - log(offset_Dog))

# test_all <- samps |> group_by(hook_name, fishing_event_id, year) |>
#   reframe(catch_count = n(), offset = mean(lglsp_hook_count)) |>
#   ungroup() |>
#   pivot_wider(
#     names_from = hook_name    ,
#     values_from = c(catch_count, offset)
#   ) |>
#   ungroup() |>
#   arrange(year, fishing_event_id) |>
#   drop_na("offset_HBLL_13/0", "offset_Dog_14/0") |>
#   rename(offset_HBLL = "offset_HBLL_13/0" , offset_Dog = "offset_Dog_14/0") |>
#   rename(catch_count_HBLL = "catch_count_HBLL_13/0" , catch_count_Dog = "catch_count_Dog_14/0") |>
#   mutate(offset = log(offset_HBLL) - log(offset_Dog))

test5 <- test3 |>
  select(year, fishing_event_id, lglsp_hook_count, hook_name, grouping_depth_id) %>%
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
final$grouping_depth_id


#change to UTMs
final <- add_utm_columns(
  final,
  ll_names = c("longitude", "latitude"),
  ll_crs = 4326,
  utm_names = c("UTM.lat", "UTM.lon"))


#model run for length based calibration coefficient
dummy_mesh <- sdmTMB::make_mesh(final, c("UTM.lon", "UTM.lat"), n_knots = 10)

#final <- final |> mutate(cpue_hbll_length = catch_count_hbll_length/exp(offset), cpue_dog_length = catch_count_dog_length/exp(offset))
final <- final |> drop_na()
weight = exp(final$offset)
dummy_mesh <- sdmTMB::make_mesh(final, c("UTM.lon", "UTM.lat"), n_knots = 10)

m2 <- sdmTMB(
  cbind(catch_count_hbll_length , catch_count_dog_length) ~ 0 + factor(length_bin), #length calibration coeff, then each catch should be for a length bin
  #cbind(cpue_hbll_length , cpue_dog_length ) ~ 0 + factor(length_bin),
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
exp(tidy(m2)$estimate)
exp(tidy(m2)$conf.low)
exp(tidy(m2)$conf.high)

q1 <- coef(m2)["factor(length_bin)(43,64]"]
exp(q1) #1.74 for smallest length bin
q2 <- coef(m2)["factor(length_bin)(64,84]"]
exp(q2)
q3 <- coef(m2)["factor(length_bin)(84,113]"]
exp(q3)


#no length
weight = exp(depth$offset)

all <- sdmTMB(
  cbind(catch_count_HBLL , catch_count_Dog ) ~ 1,
  mesh = dummy_mesh,
  spatial = "off",
  spatiotemporal = "off",
  data = depth,
  #offset = final$offset,
  #family = binomial(),
  family = betabinomial(),
  weights = weight,
  control = sdmTMBcontrol(multiphase = FALSE)
)

coef(all)
all2 <- tidy(all, ran.pars = TRUE)
exp(all2$estimate)
exp(all2$conf.low)
exp(all2$conf.high)



weight = exp(depth$offset)

depthm <- sdmTMB(
  cbind(catch_count_HBLL , catch_count_Dog ) ~ 1 + as.factor(grouping_depth_id),
  mesh = dummy_mesh,
  spatial = "off",
  spatiotemporal = "off",
  data = depth,
  #offset = final$offset,
  #family = binomial(),
  family = betabinomial(),
  weights = weight,
  control = sdmTMBcontrol(multiphase = FALSE)
)
coef(depthm)
depthc <- tidy(depthm, ran.pars = TRUE)
exp(depthc$estimate)
exp(depthc$conf.low)
exp(depthc$conf.high)


site <- sdmTMB(
  cbind(catch_count_HBLL , catch_count_Dog ) ~ 1,
  #cbind(cpue_hbll , cpue_dog ) ~ 1 + (1|fishing_event_id),
  mesh = dummy_mesh,
  spatial = "off",
  spatiotemporal = "off",
  data = test_all,
  #offset = final$offset,
  #family = binomial(),
  family = betabinomial(),
  weights = weight,
  control = sdmTMBcontrol(multiphase = FALSE)
)

coef(site)
site2 <- tidy(site, ran.pars = TRUE)
exp(site2$estimate)
exp(site2$conf.low)
exp(site2$conf.high)


AIC(all)
AIC(m2)
AIC(depthm)

table = (tidy(all))
exp(table$estimate)
exp(table$conf.low)
exp(table$conf.high)

#log_offset = log(mean(c(exp(q1), exp(q2), exp(q3))))
#log_offset
#exp(log_offset) #68 percent more fish caught on HBLL

length <- data.frame("Length (cm)" = c("43-64", "64-84", "84-113", "all data"))
q <- data.frame("rho" = round(c(exp(q1), exp(q2), exp(q3), exp(all2$estimate)), 2))
Data <- data.frame(Data = "paired")
table <- cbind(Data, length, q)
table$conf.low = c(round(exp(tidy(m2)$conf.low),2), round(exp(all2$conf.low),2))
table$conf.high = c(round(exp(tidy(m2)$conf.high),2), round(exp(all2$conf.high),2))
table$CI <- paste0((table$conf.low), "-", (table$conf.high))
table <- table |> mutate(final = paste0(rho, " (", conf.low, "-", conf.high, ")"))
table <- table |> dplyr::select("Data", "Length..cm.", final )
rownames(table) <- NULL

table |>
  knitr::kable(
    format = "latex",
    col.names = c("Data",
                  "Length bin (cm)", "rho (CI)"),
    booktabs = TRUE,
    align = "c",
    caption = "rho for seasonally paired comparative sets",
    label = "length_calibration_season"
  ) |>
  #kableExtra::row_spec(0, bold = TRUE, color = "white", background = "grey")  |>  # header styling
  kableExtra::kable_styling(full_width = FALSE)


