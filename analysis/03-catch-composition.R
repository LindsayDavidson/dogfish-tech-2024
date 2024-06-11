
# library -----------------------------------------------------------------

library(ggplot2)
library(tidyverse)

# load data ---------------------------------------------------------------

#final <- readRDS("data-generated/dogfishs_allsets_allspecies_counts.rds")
samples <- readRDS("data-raw/dogfish_samples_cleaned.rds") |>
  filter(species_code == "044")
unique(samples$survey)
unique(samples$survey_type)


# ggplot ------------------------------------------------------------------
samples |>
  filter(specimen_sex_code %in% c(1,2)) |>
  ggplot() +
  geom_histogram(aes(total_length, fill = as.factor(specimen_sex_code)), binwidth = 5) +
  scale_fill_manual(values = c("red", "grey"), labels=c('Males', 'Females')) +
  scale_colour_manual(values = c("red", "grey")) +
  facet_wrap(~year + specimen_sex_code, ncol=2) + theme_classic() +
  theme(strip.text = element_blank()) +
  labs(legend = "Sex")


samples |>
  filter(specimen_sex_code %in% c(1,2)) |>
  group_by(year, specimen_sex_code, survey) |>
  reframe(count = n()) |>
  ggplot() +
  geom_histogram(aes(total_length, colour = as.factor(specimen_sex_code),
                     fill = as.factor(specimen_sex_code))) +
  facet_wrap(~year + specimen_sex_code, ncol=2) + theme_bw() +
  theme(strip.text = element_blank())


# length comps of compative work ------------------------------------------


samples |>
  filter(year %in% c(2022, 2023)) |>
  group_by(year, survey) |>
  filter(specimen_sex_code %in% c(1,2)) |>
  ggplot() +
  #geom_histogram(aes(total_length, fill = as.factor(specimen_sex_code)), binwidth = 5) +
  geom_density(aes(total_length, fill = as.factor(specimen_sex_code)), binwidth = 5) +
  scale_fill_manual(values = c("red", "grey"), labels=c('Males', 'Females')) +
  scale_colour_manual(values = c("red", "grey")) +
  facet_wrap(~survey + year + specimen_sex_code, ncol=2) + theme_classic() +
  #theme(strip.text = element_blank()) +
  labs(legend = "Sex")
