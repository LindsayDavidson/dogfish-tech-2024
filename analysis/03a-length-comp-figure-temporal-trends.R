#plot depth through time for HBLL and DOg survey

library(ggplot2)
library(tidyverse)
library(sdmTMB)

latitude_cutoff <- 49.93883
bccrs <- 32609
# loc = "HBLL INS N"
# loc = "HBLL INS S"

# data ---------------------------------------------------------------
# all
samps <- readRDS("output/samps_joined.rds")
unique(samps$survey_abbrev)
samps <- samps |> filter(survey3 %in% c("HBLL INS S", "dog"))


# plot length comps through time for each survey --------------------------

# samps |> #<- I like a combined plot of the next plots instead of this one
#   filter(sex %in% c(1, 2)) |>
#   ggplot() +
#   geom_density(aes(length, group = year, colour = year, fill = year),
#                alpha = 0.2) +
#   facet_wrap(~survey_abbrev + sex) + theme_classic() +
#   scale_x_continuous(limits = c(45, 115))
# ggsave("Figures/length_comps_temporal.jpg", width = 5, height = 4)

samps |>
  filter(sex %in% c(2)) |>
  filter(survey_abbrev == "HBLL INS S") |>
  ggplot() +
  geom_vline(xintercept = c(70)) +
  geom_density(aes(length, group = year), fill = "red",
               alpha = 0.2) +
  facet_wrap(~year, ncol = 1) + theme_classic() +
  scale_x_continuous(limits = c(40, 120)) +
  guides(fill = "none", colour = "none")
#ggsave("Figures/lenghtcomps_time.jpg", height = 12, width = 3)

samps |>
  filter(sex %in% c(2)) |>
  filter(survey_abbrev == "DOG") |>
  ggplot() +
  geom_vline(xintercept = c(65, 85)) +
  geom_density(aes(length, group = year), fill = "red",
               alpha = 0.2) +
  facet_wrap(~year, ncol = 1) + theme_classic() +
  scale_x_continuous(limits = c(40, 120)) +
  guides(fill = "none", colour = "none")
#ggsave("Figures/lenghtcomps_dog_time.jpg", height = 12, width = 3)

samps |>
  filter(sex %in% c(1)) |>
  filter(survey_abbrev == "DOG") |>
  ggplot() +
  geom_vline(xintercept = c(75)) +
  geom_density(aes(length, group = year), fill = "grey50",
               alpha = 0.2) +
  facet_wrap(~year, ncol = 1) + theme_classic() +
  scale_x_continuous(limits = c(40, 100)) +
  guides(fill = "none", colour = "none")
#ggsave("Figures/lenghtcomps_dog_males_time.jpg", height = 12, width = 3)

samps |>
  filter(sex %in% c(1)) |>
  filter(survey_abbrev == "HBLL INS S") |>
  ggplot() +
  geom_vline(xintercept = c(75)) +
  geom_density(aes(length, group = year), fill =  "grey50",
               alpha = 0.2) +
  facet_wrap(~year, ncol = 1) + theme_classic() +
  scale_x_continuous(limits = c(40, 100)) +
  guides(fill = "none", colour = "none")
#ggsave("Figures/lenghtcomps_hbll_males_time.jpg", height = 12, width = 3)


#cowplot all of those