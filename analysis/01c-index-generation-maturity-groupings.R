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

samps <- readRDS("data-raw/dogfish_samples_cleaned.rds") #this is from the get all function
#needs age, trip_start_date
#this is a hack for now
samps$trip_start_date <-samps$time_begin_retrieval
samps$age <-NA #age column is empty and therefore dropped. add back in with NAs

# samps <- readRDS("output/samps_joined.rds") |>
#   filter(species_common_name %in% c("NORTH PACIFIC SPINY DOGFISH", "north pacific spiny dogfish"))

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

d <- readRDS("data-raw/wrangled-hbll-dog-sets.rds")
rm <- d |>
  filter(year == 2004 & survey_abbrev == "dog-jhook")
d <- filter(d, !fishing_event_id %in% c(rm$fishing_event_id))

test <- d |>
  mutate(toohigh = ifelse(catch_count > lglsp_hook_count, "morecatch", "lesscatch")) |>
  filter(toohigh == "morecatch")

ggplot() +
  geom_point(data = d, aes(lglsp_hook_count, catch_count, colour = survey_abbrev)) + facet_wrap(~year) +
  geom_point(data = test, aes(lglsp_hook_count, catch_count), colour = 'red') + facet_wrap(~year)

# maturity cutoffs ----------------------------------------------------

samps_86 <- filter(sampsm, year %in% c(1986, 1989))

m86 <- gfplot::fit_mat_ogive(samps_86,
  type = "length",
  sample_id_re = TRUE,
  custom_maturity_at = c(NA, 55)
)
m86$mat_perc$f.p0.95
x <- gfplot::plot_mat_ogive(m86)
x <- figure(m86)
mat <- m86$pred_data |> mutate(year = 1986)

samps_08 <- filter(sampsm, year == 2008)
m08 <- gfplot::fit_mat_ogive(samps_08,
  type = "length",
  sample_id_re = TRUE,
  custom_maturity_at = c(NA, 55)
)
m08$mat_perc$f.p0.95
y <- gfplot::plot_mat_ogive(m08)
mat2 <- m08$pred_data |> mutate(year = 2008)

samps_23 <- filter(sampsm, year %in% c(2023))
m23 <- gfplot::fit_mat_ogive(samps_23,
  type = "length",
  sample_id_re = TRUE,
  custom_maturity_at = c(NA, 55)
)
m23$mat_perc$f.p0.95
z <- gfplot::plot_mat_ogive(m23)
mat3 <- m23$pred_data |> mutate(year = 2023)

cowplot::plot_grid(x, y, z, labels = c('A. 1986', 'B. 2008', "C. 2023"), label_size = 12, ncol = 1)
ggsave("Figures/maturity-ogives-sog-temporal.jpg", width = 7, height = 15)

mat <- rbind(mat, mat2, mat3)
p <- ggplot(mat, aes(age_or_length, glmm_fe, group = year, colour = as.factor(year))) +
  geom_line(size = 0.5) +
  facet_wrap(~female) +
  scale_colour_viridis_d() + theme_classic()
p
#p + geom_line(data = mat, aes(age_or_length, glmm_re, group = year, colour = as.factor(year), alpha = 0.2))
ggsave("Figures/maturity_curves_temporal.jpg", width = 5, height = 2.5)

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
  #filter(!sex  %in% c(0,3)) |>
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

test <- samps2 |> dplyr::select(year, fishing_event_id, catch_count_group , survey_abbrev, survey2)
test<- test |> group_by(year, fishing_event_id, survey_abbrev) |> reframe(sum_samps =catch_count_group)
d2 <- d |> dplyr::select(year, fishing_event_id, catch_count, survey_abbrev, survey2)
d2 <- d2 |> group_by(year, fishing_event_id, survey_abbrev) |> reframe(sum_setss =catch_count)
test2 <- left_join(test, d2)
ggplot(test2, aes(sum_samps, sum_setss)) + geom_point() + facet_wrap(~year, scales = "free")

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


