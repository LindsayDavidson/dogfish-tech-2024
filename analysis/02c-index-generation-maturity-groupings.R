# Code is for plot of length densities in SOM

# BC tail extended
# how were the early dogfish lengths measured?

# library -----------------------------------------------------------------

library(TMB)
library(sp)
library(sdmTMB)
library(here)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggsidekick)
library(kableExtra)
library(sf)
theme_set(ggsidekick::theme_sleek())


# Samps ----------------------------------------------------

samps <- readRDS("output/samps_joined.rds") |>
  filter(species_common_name %in% c("NORTH PACIFIC SPINY DOGFISH", "north pacific spiny dogfish"))
glimpse(samps)
unique(samps$survey_abbrev)
unique(samps$survey2)

ggplot(samps, aes(year, length, col = survey_abbrev)) +
  geom_jitter() + facet_wrap(~survey2)

# maturity information
library(gfplot)
data("maturity_assignment")
data("maturity_short_names") # males maturity code = 90, female maturity code is >= 77
maturity_assignment
maturity_short_names
dog_maturity_short_names <- filter(maturity_short_names, maturity_convention_desc == "DOGFISH")

# there are a lot of codes I don't know what they are
codes <- unique(dog_maturity_short_names$maturity_code)
sampsm <- samps |> filter(maturity_code %in% codes)
sort(unique(sampsm$maturity_code))
# saveRDS(dsurvey_bio2, "data-raw/dogfish_samples_cleaned_withmaturity.rds")

# 94 cm tl for females is mature based on DFO assessment
# born at 26 and 27 cm.
# suggest growth of 1.5 cm per year.
# 15 year old dogfish would be about ~50 cm
# Males 70 cm mature
# code 55 versus other maturity code is pregnant versus able to be pregnant


# all without 2004 comp work ----------------------------------------------
#
# d <- readRDS("data-raw/wrangled-hbll-dog-sets.rds") |>
#   filter(year != 2004)
# range(d$depth_m)
# d$log_botdepth2 <- d$log_botdepth * d$log_botdepth
# str(d$month)
# grid <- grid <- readRDS("output/prediction-grid-hbll-n-s-dog-2-km.rds")
# grid$log_botdepth2 <- grid$log_botdepth * grid$log_botdepth
# grid$area_km2 <- as.numeric(grid$area_km)
# years <- seq(min(d$year), 2023, 1)
# grid <- purrr::map_dfr(years, ~ tibble(grid, year = .x))
# grid$survey2 <- "hbll"
# grid$julian <- mean(d$julian)
# grid$month <- 8
# # path <- "output/fit-sog-hblldog_no2004.rds"
# # pathind <- "output/ind-sog-hblldog_no2004.rds"
# sort(unique(d$year))
# extratime <- c(1987, 1988, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2004, 2006, 2017, 2020)




# maturity cutoffs ----------------------------------------------------

samps_86 <- filter(sampsm, year %in% c(1986, 1989))
m86 <- gfplot::fit_mat_ogive(samps_86,
  type = "length",
  sample_id_re = TRUE,
  custom_maturity_at = c(NA, 55)
)
gfplot::plot_mat_ogive(m86)
mat <- m86$pred_data |> mutate(year = 1986)

samps_08 <- filter(sampsm, year == 2008)
m08 <- gfplot::fit_mat_ogive(samps_08,
  type = "length",
  sample_id_re = TRUE,
  custom_maturity_at = c(NA, 55)
)
gfplot::plot_mat_ogive(m08)
mat2 <- m08$pred_data |> mutate(year = 2008)

samps_23 <- filter(sampsm, year %in% c(2023))
m23 <- gfplot::fit_mat_ogive(samps_23,
  type = "length",
  sample_id_re = TRUE,
  custom_maturity_at = c(NA, 55)
)
gfplot::plot_mat_ogive(m23)
mat3 <- m23$pred_data |> mutate(year = 2023)

mat <- rbind(mat, mat2, mat3)
p <- ggplot(mat, aes(age_or_length, glmm_fe, group = year, colour = as.factor(year))) +
  geom_line(size = 1) +
  facet_wrap(~female) +
  scale_colour_viridis_d()
p + geom_line(data = mat, aes(age_or_length, glmm_re, group = year, colour = as.factor(year), alpha = 0.2))


m <- gfplot::fit_mat_ogive(samps,
  type = "length",
  sample_id_re = TRUE,
  custom_maturity_at = c(NA, 55)
)
gfplot::plot_mat_ogive(m)
# ggsave("Figures/maturity-ogive-inside.jpg", width = 4, height = 3 )

xx <- m$pred_data
f95 <- filter(xx, female == 1 & glmm_re >= 0.95)
range(f95$age_or_length)
f05 <- filter(xx, female == 1 & glmm_re <= 0.05)
f50 <- m$mat_perc$f.p0.5
range(f05$age_or_length)
range(f50$age_or_length)

min(f95$age_or_length)
max(f95$age_or_length)

xy <- m$pred_data
m95 <- filter(xy, female == 0 & glmm_re >= 0.95)
range(m95$age_or_length)
m05 <- filter(xy, female == 0 & glmm_re <= 0.05)
m50 <- m$mat_perc$m.p0.5
range(m05$age_or_length)
range(m95$age_or_length)

# tally by length group and calculate weight of each group
# samps2 <- samps |>
#   filter(!sex  %in% c(0,3)) |>
#   mutate(lengthgroup = ifelse(length >= max(f05$age_or_length) & length < min(f95$age_or_length) & sex == 2, "maturingf",
#     ifelse(length >= max(m05$age_or_length) & length < min(m95$age_or_length) & sex == 1, "maturingm",
#       ifelse(length >= min(f95$age_or_length) & sex == 2, "mf",
#         ifelse(length >= min(m95$age_or_length) & sex == 1, "mm",
#           ifelse(length <= max(f05$age_or_length) & sex == 2, "imm",
#             ifelse(length <= max(m05$age_or_length) & sex == 1, "imm",
#               NA
#             )
#           )
#         )
#       )
#     )
#   )) |>
#   group_by(year, lengthgroup, survey2, fishing_event_id) |>
#   mutate(catch_count_group = n()) |>
#   distinct(year, lengthgroup, catch_count_group, .keep_all = TRUE)

samps2 <- samps |>
  filter(!sex  %in% c(0,3)) |>
  mutate(lengthgroup = ifelse(length >= 85.10745 & sex == 2, "mf",
                                            ifelse(length >= 69.7079 & sex == 1, "mm",
                                                   ifelse(length < 85.10745 & sex == 2, "immf",
                                                          ifelse(length < 69.7079 & sex == 1, "immm",
                                                             NA
                                                          )
                                                   )
                                            )
                                     )
                              ) |>
  group_by(year, lengthgroup, survey2, fishing_event_id) |>
  mutate(catch_count_group = n()) |>
  distinct(year, lengthgroup, catch_count_group, .keep_all = TRUE)

samps2 |>
  group_by(year, survey_abbrev, lengthgroup) |>
  summarize(catch_count_group_sum = sum(catch_count_group)) |>
  ggplot() +
  geom_line(aes(year, log(catch_count_group_sum), group = lengthgroup, colour = lengthgroup)) +
  facet_wrap(~survey_abbrev)

samps2 |>
  group_by(year, survey2, lengthgroup) |>
  summarize(catch_count_group_sum = sum(catch_count_group)) |>
  ggplot() +
  geom_line(aes(year, log(catch_count_group_sum), group = lengthgroup, colour = lengthgroup)) +
  facet_wrap(~survey2)


saveRDS(samps2, "output/catch_by_maturitygroup.rds")

# rough figure ----------------------------------------------------
unique(samps$survey_name2)
unique(samps$survey_name)
unique(samps$survey_abbrev)

breakpts <- c(c(1986, 1996, 2002), seq(2003, 2023, 10))
breakpts <- seq(1996, 2022, 7)
samps_hist <- samps %>%
  mutate(year_group = findInterval(year, breakpts)) |>
  drop_na(length) |>
  filter(!sex %in% c(0, 3))

ggplot(samps_hist, aes(length, group = as.factor(year_group), fill = as.factor(year_group)),
  colour = as.factor(year_group)
) + # geom_vline (xintercept = samps_hist$mature, group = as.factor(samps_hist$sex) )+
  geom_density(alpha = 0.6, size = 0.25) + # , bins = 50) +
  facet_grid(~ survey2 + sex)


