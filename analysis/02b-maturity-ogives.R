# Code is for plot of length densities in SOM

# BC tail extended
# how were the early dogfish lengths measured?

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

#load data
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
#x <- figure(m86)
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
ggsave("Figures/maturity-ogives-sog-temporal.jpg", width = 4, height = 6)

