# calculate samples for report

samps <- readRDS("data-raw/dogfish_samples_cleaned.rds")
final <- readRDS("data-raw/wrangled-hbll-dog-sets.rds") |>  #<- no sex in the set data use sample data
 dplyr::select(fishing_event_id, lglsp_hook_count)
glimpse(final)
samps <-finalsamps <- left_join(samps, final)

# comp work summary and figures -------------------------------------------
comp <- samps |>
  filter(year == 2019 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" |
    year == 2022 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" |
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS")
jhook <- filter(comp, hooksize_desc == "12/0")
comp <- filter(comp, !fishing_event_id %in% c(jhook$fishing_event_id))

# length differences
comp |>
  filter(sex %in% c(1, 2)) |>
  group_by(sex, hooksize_desc) |>
  filter(year != 2019) |>
  drop_na(length) |>
  reframe(min = min(length), max = max(length))

# more males captured regardless of hooks or season
comp |>
  filter(sex %in% c(1, 2)) |>
  group_by(sex, hooksize_desc) |>
  filter(year != 2019) |>
  reframe(sum = n(), sumhooks = sum(lglsp_hook_count)) |> #this calc isn't right, fix, double counting fishing event id????
  mutate(cpue = sum/sumhooks)

comp |> #<- more males and more females captured in the fall, otherwise very similar
  filter(sex %in% c(1, 2)) |>
  group_by(sex, survey_timing, hooksize_desc, year) |>
  filter(year != 2019) |>
  reframe(sum = n(), sumhooks = sum(lglsp_hook_count)) |>
  mutate(cpue = sum/sumhooks)

comp |>
  filter(sex %in% c(1, 2)) |>
  group_by(sex, survey_timing) |>
  filter(year != 2019) |>
  reframe(sum = n(), sumhooks = sum(lglsp_hook_count)) |>
  mutate(cpue = sum/sumhooks)

comp |>
  filter(sex %in% c(1, 2)) |>
  group_by(sex) |>
  filter(year != 2019) |>
  reframe(sum = n(), sumhooks = sum(lglsp_hook_count)) |>
  mutate(cpue = sum/sumhooks)

# gear related differences in catch comp
comp |> # hbll pot. catching more females on hbll hooks
  filter(survey_timing != "fall") |>
  group_by(sex, survey_timing, hooksize_desc, year) |>
  filter(year != 2019) |>
  reframe(sum = n()) |>
  filter(sex %in% c(1, 2))

# ratio of m/f across different geartypes
compm <- comp |>
  group_by(hooksize_desc, sex) |>
  filter(sex == 1) |>
  reframe(count_m = n()) |>
  dplyr::select(-sex)

compf <- comp |>
  group_by(hooksize_desc, sex) |>
  filter(sex == 2) |>
  reframe(count_f = n()) |>
  dplyr::select(-sex)

compratio <- left_join(compm, compf)
compratio |> mutate(ratio = count_m / count_f)

comp$hooksize_desc <- factor(comp$hooksize_desc, labels = c("HBLL", "Dog"))

comp |>
  mutate(survey_timing = fct_relevel(survey_timing, "summer", "fall")) |>
  filter(sex %in% c(1, 2)) |>
  drop_na(length) |>
  # filter(hooksize_desc == "13/0") |>
  ggplot() +
  geom_jitter(aes(hooksize_desc, length, group = sex, colour = as.factor(sex))) +
  # geom_violin(aes(as.factor(grouping_depth_id), length, colour = sex)) +
  geom_boxplot(aes(hooksize_desc, length)) +
  facet_wrap(~ survey_timing + as.factor(sex), ncol = 2) +
  theme_classic() +
  xlab("Geartype") +
  ylab("Length (cm)") +
  labs(colour = "Geartype") +
  scale_colour_manual(values = c("grey50", "grey80")) #+
# theme(strip.text = element_blank())
ggsave("Figures/length-geartypes.jpg", width = 5, height = 5)


# comp work by season ------------------------------------------------------
comp |> # fewer dogfish in general captured on Dog, big diff between n and south hbll
  filter(sex %in% c(1, 2)) |>
  group_by(survey_timing) |>
  filter(year != 2019) |>
  reframe(sum = n())

test <- comp |> # fewer dogfish in general captured on Dog, big diff between n and south hbll
  filter(sex %in% c(1, 2)) |>
  mutate(sex = ifelse(sex == 1, "male", "female")) |>
  group_by(sex, hooksize_desc, survey_timing) |>
  filter(year != 2019) |>
  reframe(sum = n())
test
test <- test |> mutate(survey_timing = fct_relevel(survey_timing, "summer", "fall"))
ggplot(test, aes(hooksize_desc, sum, colour = survey_timing)) +
  geom_point() +
  facet_wrap(~sex)
ggplot(test, aes(survey_timing, sum, colour = as.factor(sex))) +
  geom_point() +
  facet_wrap(~hooksize_desc) +
  theme_classic() +
  ylab("Catch count") +
  xlab("Geartype") +
  labs(colour = "Sex") +
  scale_colour_manual(values = c("navyblue", "orange"))
ggsave("Figures/sexratio-geartypes.jpg", width = 5, height = 3)

compm <- comp |>
  group_by(hooksize_desc, sex, survey_timing) |>
  filter(sex == 1) |>
  reframe(count_m = n()) |>
  dplyr::select(-sex)
compf <- comp |>
  group_by(hooksize_desc, sex, survey_timing) |>
  filter(sex == 2) |>
  reframe(count_f = n()) |>
  dplyr::select(-sex)
compratio <- left_join(compm, compf)
compratio |> mutate(ratio = count_m / count_f)

compm <- comp |> # does this hold with the two shallowest depths?
  filter(grouping_depth_id %in% c("D2", "D3")) |>
  group_by(hooksize_desc, sex, survey_timing) |>
  filter(sex == 1) |>
  reframe(count_m = n()) |>
  dplyr::select(-sex)
compf <- comp |>
  filter(grouping_depth_id %in% c("D2", "D3")) |>
  group_by(hooksize_desc, sex, survey_timing) |>
  filter(sex == 2) |>
  reframe(count_f = n()) |>
  dplyr::select(-sex)
compratio <- left_join(compm, compf)
compratio |> mutate(ratio = count_m / count_f)


# length comparison across hbll and dog surveys -------------------------------------------
notcomp <- samps |>
  filter(activity_desc != "DOGFISH GEAR/TIMING COMPARISON SURVEYS")
jhook <- filter(comp, hooksize_desc == "12/0")
comp <- filter(comp, !fishing_event_id %in% c(jhook$fishing_event_id))

# comp |>
#   group_by(year, sex, survey_timing, hooksize_desc) |>
#   drop_na(length) |>
#   reframe(min = min(length), max = max(length), mean = mean(length), median = median(length)) |>
#   filter(sex %in% c(1, 2))

comp |>
  group_by(sex, survey_timing) |>
  drop_na(length) |>
  reframe(min = min(length), max = max(length), mean = mean(length), median = median(length)) |>
  filter(sex %in% c(1, 2))

comp |>
  mutate(survey_timing = fct_relevel(survey_timing, "summer", "fall")) |>
  filter(sex %in% c(1, 2)) |>
  mutate(sex = ifelse(sex == 1, "male", "female")) |>
  filter(grouping_depth_id %in% c("D2", "D3")) |>
  drop_na(length) |>
  filter(hooksize_desc == "HBLL") |>
  ggplot() +
  geom_jitter(aes(survey_timing, length, group = sex, colour = as.factor(sex))) +
  # geom_violin(aes(as.factor(grouping_depth_id), length, colour = sex)) +
  geom_boxplot(aes(survey_timing, length)) +
  facet_wrap(~ as.factor(sex), ncol = 2) +
  theme_classic() +
  ylab("Length") +
  xlab("Survey timing") +
  labs(colour = "Sex") +
  scale_colour_manual(values = c("grey50", "grey80"))
ggsave("Figures/lengthcomps-seasons.jpg", width = 5, height = 3)


# length by depth ---------------------------------------------------------


# comp |>
#   mutate(survey_timing = fct_relevel(survey_timing, "summer", "fall")) |>
#   filter(sex %in% c(1, 2)) |>
#   drop_na(length) |>
#   filter(hooksize_desc == "HBLL") |>
#   ggplot() +
#   geom_jitter(aes(as.factor(grouping_depth_id), length, group = sex, colour = as.factor(sex))) +
#   # geom_violin(aes(as.factor(grouping_depth_id), length, colour = sex)) +
#   geom_boxplot(aes(as.factor(grouping_depth_id), length)) +
#   facet_wrap(~ as.factor(sex) +  survey_timing, ncol = 2) +
#   theme_classic() +
#   scale_colour_manual(values = c("grey50", "grey80"))
# ggsave("Figures/length-by-depth-hbll.jpg", width = 5, height = 5)

# comp |>
#   mutate(survey_timing = fct_relevel(survey_timing, "summer", "fall")) |>
#   filter(sex %in% c(1, 2)) |>
#   drop_na(length) |>
#   filter(hooksize_desc == "14/0") |>
#   ggplot() +
#   geom_jitter(aes(as.factor(grouping_depth_id), length, group = sex, colour = as.factor(sex))) +
#   # geom_violin(aes(as.factor(grouping_depth_id), length, colour = sex)) +
#   geom_boxplot(aes(as.factor(grouping_depth_id), length)) +
#   facet_wrap(~ as.factor(sex) +   survey_timing, ncol = 2) +
#   theme_classic() +
#   scale_colour_manual(values = c("grey50", "grey80"))
# ggsave("Figures/length-by-depth-dog.jpg", width = 5, height = 5)

# comp |>
#   mutate(survey_timing = fct_relevel(survey_timing, "summer", "fall")) |>
#   mutate(sex = as.factor(sex)) |>
#   group_by(grouping_depth_id, sex, survey_timing, hooksize_desc) |>
#   filter(year != 2019) |>
#   reframe(sum = n()) |>
#   filter(sex %in% c(1, 2)) |>
#   ggplot() +
#   geom_point(aes(grouping_depth_id, sum, group = survey_timing, colour = survey_timing)) +
#   geom_line(aes(grouping_depth_id, sum, group = survey_timing, colour = survey_timing)) +
#   facet_wrap(~sex + hooksize_desc) +
#   theme_classic() +
#   scale_colour_manual(values = c("grey60", "black"))
# ggsave("Figures/depth-sex-survey-mf.jpg", width = 8, height = 5)
#
# comp |>
#   mutate(survey_timing = fct_relevel(survey_timing, "summer", "fall")) |>
#   filter(sex %in% c(1, 2)) |>
#   drop_na(length) |>
#   filter(hooksize_desc == "HBLL") |>
#   ggplot() +
#   geom_jitter(aes(as.factor(grouping_depth_id), length, group = sex, colour = as.factor(sex))) +
#   # geom_violin(aes(as.factor(grouping_depth_id), length, colour = sex)) +
#   geom_boxplot(aes(as.factor(grouping_depth_id), length)) +
#   facet_wrap(~ as.factor(sex) +  survey_timing, ncol = 2) +
#   theme_classic() +
#   scale_colour_manual(values = c("grey50", "grey80"))
# ggsave("Figures/length-by-depth-hbll.jpg", width = 5, height = 5)
#
# comp |>
#   mutate(survey_timing = fct_relevel(survey_timing, "summer", "fall")) |>
#   filter(sex %in% c(1, 2)) |>
#   drop_na(length) |>
#   filter(hooksize_desc == "Dog") |>
#   ggplot() +
#   geom_jitter(aes(as.factor(grouping_depth_id), length, group = sex, colour = as.factor(sex))) +
#   # geom_violin(aes(as.factor(grouping_depth_id), length, colour = sex)) +
#   geom_boxplot(aes(as.factor(grouping_depth_id), length)) +
#   facet_wrap(~ as.factor(sex) +   survey_timing, ncol = 2) +
#   theme_classic() +
#   scale_colour_manual(values = c("grey50", "grey80"))
# ggsave("Figures/length-by-depth-dog.jpg", width = 5, height = 5)
#
#
#
