# Length and catch composition differences between the DOG and HBLL surveys

# library -----------------------------------------------------------------
library(gfdata)
library(gfplot)
library(tidyverse)
library(here)
library(sdmTMB)
library(sf)
library(sp)


# load data ---------------------------------------------------------------
#samps <- readRDS("output/samps_joined.rds")
samps <- readRDS("data-raw/dogfish_samples_cleaned.rds")



# ggplot ------------------------------------------------------------------
samples |>
  filter(sex %in% c(1,2)) |>
  filter(survey_sep %in% ("dog")) |>
  ggplot() +
  geom_histogram(aes(total_length, fill = as.factor(sex)), binwidth = 5) +
  scale_fill_manual(values = c("red", "grey"), labels=c('Males', 'Females')) +
  scale_colour_manual(values = c("red", "grey")) +
  facet_wrap(~year + sex, ncol=2) + theme_classic() +
  theme(strip.text = element_blank()) +
  labs(legend = "Sex")

samples |>
  filter(sex %in% c(1,2)) |>
  group_by(year, sex, survey_lumped) |>
  #reframe(count = n()) |>
  ggplot() +
  geom_histogram(aes(length, colour = as.factor(sex),
                     fill = as.factor(sex))) +
  facet_wrap(~year + sex, ncol=2) + theme_bw() +
  theme(strip.text = element_blank())

# length comps of comparative work ------------------------------------------
samples |>
  filter(year %in% c(2022, 2023)) |>
  group_by(year, survey_lumped) |>
  filter(sex %in% c(1,2)) |>
  ggplot() +
  #geom_histogram(aes(total_length, fill = as.factor(specimen_sex_code)), binwidth = 5) +
  geom_density(aes(total_length, fill = as.factor(sex)), binwidth = 5) +
  scale_fill_manual(values = c("red", "grey"), labels=c('Males', 'Females')) +
  scale_colour_manual(values = c("red", "grey")) +
  facet_wrap(~survey_lumped + year + sex, ncol=2) + theme_classic() +
  #theme(strip.text = element_blank()) +
  labs(legend = "Sex")


# length comp between geartype figure for each survey --------------------------------------

comps <- samps |> filter(year %in% c(2021, 2022, 2023))

ggplot(comps, aes(year, length, group = hook_desc, colour = hook_desc)) +
  geom_point()

ggplot(comps, aes(year, length, group = hook_desc, colour = hook_desc)) +
  geom_violin() +
  facet_wrap(~survey_abbrev)

samps |>
  filter(activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS") |>
  filter(!year %in% c(2004, 2019)) |>
  filter(sex %in% c(1, 2)) |>
  ggplot(aes(hooksize_desc, length)) +
  geom_boxplot() +
  #geom_violin() +
  #geom_jitter() +
  facet_wrap(~sex) + theme_classic()

samps |>
  #filter(activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS") |>
  filter(sex %in% c(1, 2)) |>
  filter(!year %in% c(2004)) |>
  mutate(id = ifelse(year == 2019 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS", "erase", NA)) |> #get rid of the 2019
  filter(is.na(id) == TRUE) |>
  ggplot(aes(hooksize_desc, length)) +
  geom_boxplot() +
  #geom_violin() +
  #geom_jitter() +
  facet_wrap(~sex)

samps |>
  filter(sex %in% c(1,2)) |>
  filter(!year %in% c(2004)) |>
  mutate(id = ifelse(year == 2019 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS", "erase", NA)) |> #get rid of the 2019
  filter(is.na(id) == TRUE) |>
  ggplot(aes(length)) +
  geom_histogram(aes(colour = NA, fill = as.factor(sex), alpha = 0.5)) +
  facet_wrap(~survey_abbrev, ncol = 5) +
  theme_classic() +
  scale_fill_manual(values = c("grey", "red")) +
  guides(colour = "none")
ggsave("Figures/lengthcomps.jpg", width = 8, height = 5)

samps |>
  filter(sex %in% c(1,2)) |>
  filter(!year %in% c(2004)) |>
  mutate(id = ifelse(year == 2019 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS", "erase", NA)) |> #get rid of the 2019
  filter(is.na(id) == TRUE) |>
  ggplot(aes(length)) +
  geom_histogram(aes(fill = as.factor(survey_abbrev)), colour = NA, position="stack", width = 2) +
  facet_wrap(~survey_abbrev + sex, ncol = 2) +
  theme_classic() +
  #scale_fill_manual(values = c("grey", "red")) +
  guides(colour = "none") +
  scale_fill_viridis_d()
ggsave("Figures/lengthcomps_bysurvey.jpg", width = 8, height = 5)

samps |>
  filter(sex %in% c(1,2)) |>
  filter(!year %in% c(2004)) |>
  mutate(id = ifelse(year == 2019 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS", "erase", NA)) |> #get rid of the 2019
  filter(is.na(id) == TRUE) |>
  ggplot() +
  geom_boxplot(aes(survey_abbrev, length, fill = as.factor(sex)), alpha = 0.5) +
  facet_wrap(~sex, ncol = 2) +
  theme_classic() +
  scale_fill_manual(values = c("grey", "red")) +
  guides(colour = "none")
ggsave("Figures/lengthcomps_bysurvey.jpg", width = 8, height = 5)

samps |>
  filter(sex %in% c(1,2)) |>
  filter(!year %in% c(2004)) |>
  mutate(id = ifelse(year == 2019 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS", "erase", NA)) |> #get rid of the 2019
  filter(is.na(id) == TRUE) |>
  filter(activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS") |>
  ggplot() +
  geom_boxplot(aes(survey_abbrev, length, fill = as.factor(sex)), alpha = 0.5) +
  facet_wrap(~sex + year, ncol = 2) +
  theme_classic() +
  scale_fill_manual(values = c("grey", "red")) +
  guides(colour = "none")
ggsave("Figures/lengthcomps_bysurvey.jpg", width = 8, height = 5)


ggplot(samps, aes(length)) +
  geom_histogram(aes(colour = as.factor(sex))) +
  facet_wrap(~survey_abbrev, ncol = 5)

gfsynopsis::plot_lengths

samps |>
  filter(survey_abbrev == "dog") |>
  filter(sex ==2) |>
  ggplot() +
  geom_density(aes(length)) +
  facet_wrap(~year)

samps |>
  filter(survey_abbrev == "dog") |>
  filter(sex ==2) |>
  group_by(year) |>
  summarize(mean = mean(length)) |>
  ggplot() +
  geom_line(aes(year, mean))

samps |>
  filter(survey_abbrev %in% c("HBLL INS S")) |>
  filter(sex ==2) |>
  ggplot() +
  geom_density(aes(length)) +
  facet_wrap(~year + month)



# length comp between seasons figure for each survey --------------------------------------

comps <- samps |> filter(year %in c(2023))
ggplot(comps, aes(year, cathc_count, group = hook_type, colour = hook_type)) +
  geom_point()

ggplot(comps, aes(year, length, group = hook_type, colour = hook_type)) +
  geom_point()

ggplot(comps, aes(year, length, group = hook_type, colour = hook_type)) +
  geom_violin() +
  facet_wrap(~survey_abbrev)

ggplot(samps, aes(length)) +
  geom_histogram(aes(colour = as.factor(sex))) +
  facet_wrap(~survey_abbrev, ncol = 5)

gfsynopsis::plot_lengths

samps |>
  filter(survey_abbrev == "dog") |>
  filter(sex ==2) |>
  ggplot() +
  geom_density(aes(length)) +
  facet_wrap(~year)

samps |>
  filter(survey_abbrev == "dog") |>
  filter(sex ==2) |>
  group_by(year) |>
  summarize(mean = mean(length)) |>
  ggplot() +
  geom_line(aes(year, mean))

samps |>
  filter(survey_abbrev %in% c("HBLL INS S")) |>
  filter(sex ==2) |>
  ggplot() +
  geom_density(aes(length)) +
  facet_wrap(~year + month)



# # anova - are the group means different? ----------------------------------
#
# f <- filter(samps, sex == 2)
# mean(f$length[(f$name) == "DOG"])
# mean(f$length[(f$name) == "HBLL INS S"])
#
# aov <- aov(length ~ name, data = filter(samps, sex == 2))
# summary(aov)
# # plot(aov)
# tukey <- TukeyHSD(aov)
# tukey
#
# aov <- aov(length ~ name, data = filter(samps, sex == 1))
# summary(aov)
# # plot(aov)
# tukey <- TukeyHSD(aov)
# tukey
# plot(tukey)
#
#
# # GLM of sex and survey ---------------------------------------------------
#
# f <- filter(samps, sex == 2 & name %in% c("DOG", "HBLL INS S"))
# f <- filter(samps, sex == 2 & name == "DOG")
# f <- filter(samps, sex == 2 & name == "HBLL INS S")
# m <- glm(length ~ name * year, data = f, family = gaussian())
# summary(m)
#
# # Ratio of M:F  -----------------------------------------------------------
#
# # females maturing 77 - 95.5
# # males maturing
# samps
# glimpse(samps)
#
# samps |> drop_na(weight) |> tally()
# samps |> tally()
# 115952 - 6984 #lots of NAs for weight
#
# #use maturity DFO of 55
# #these legnths are from a length weight and length matuirty curve calculated in
# #split-index_by_regionandmaturity.R
# test <- samps |>
#   filter(sex %in% c(1, 2)) |>
#   mutate(lengthgroup = ifelse(length >= 77 & length < 95.5 & sex == 2, "maturingf",
#                               ifelse(length >= 65.1 & length < 76.7 & sex == 1 , "maturingm",
#                                      ifelse(length >= 95.5 & sex == 2, "mf",
#                                             ifelse(length >= 76.7 & sex == 1, "mm",
#                                                    ifelse(length < 77 & sex == 2, "imm",
#                                                           ifelse(length < 65.1 & sex == 1, "imm",
#                                                                  NA))))))) |>
#   group_by(year, lengthgroup, name) |>
#   mutate(count =  n())
#
# ggplot(filter(test, name != "DOGJhooks"), aes(year, count, group = name, colour = name)) +
#   geom_line() +
#   facet_wrap(~lengthgroup, scales = "free") + theme_classic() + geom_point()
#
#
# # Mature female ratio -----------------------------------------------------
#
# test2 <- test |>
#   group_by(year, fishing_event_id, name) |>
#   filter(lengthgroup != "immatures") |>
#   mutate(sumall =  n())
# test3 <- test |>
#   group_by(year, name) |>
#   filter(lengthgroup  == "mf") |>
#   summarize(summf = n())
# testmm <- test |>
#   group_by(year, name) |>
#   filter(lengthgroup  == "mm") |>
#   summarize(summm = n())
#
# final <- inner_join(test3, testmm, by = c("year", "name"))
# final <- final |>
#   mutate(ratio = summf/summm*100)
# ggplot(final, aes(year, ratio, group = name)) + geom_point() + geom_line() + facet_wrap(~name) +
#   ylab("Ratio (Mature females:Mature Males)")
