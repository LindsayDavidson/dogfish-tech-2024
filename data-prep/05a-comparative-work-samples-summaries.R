# calculate samples for report

samps <- readRDS("data-raw/dogfish_samples_cleaned.rds")
final <- readRDS("data-raw/wrangled-hbll-dog-sets.rds")  #<- no sex in the set data use sample data

# comp work summary and figures -------------------------------------------
comp <- samps |>
  filter(year == 2019 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" |
    year == 2022 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" |
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS")
jhook <- filter(comp, hooksize_desc == "12/0")
comp <- filter(comp, !fishing_event_id %in% c(jhook$fishing_event_id))


final <- final |>
  filter(fishing_event_id %in% c(comp$fishing_event_id)) |>
  dplyr::select(fishing_event_id, lglsp_hook_count, hooksize_desc, year, survey_abbrev)

final_year <- final |>
  group_by(hooksize_desc, year) |>
  reframe(sumhooks = sum(lglsp_hook_count))
final_noyear <- final |>
  group_by(hooksize_desc) |>
  reframe(sumhooks = sum(lglsp_hook_count))



#summary of lengths
comp |>
  filter(sex %in% c(1, 2)) |>
  #group_by(sex, hooksize_desc) |>
  filter(year != 2019) |>
  reframe(sum = n())

comp |>
  filter(sex %in% c(1, 2)) |>
  group_by(sex) |>
  filter(year != 2019) |>
  reframe(sum = n())

comp |>
  filter(sex %in% c(1, 2)) |>
  group_by(sex, hooksize_desc, year, survey_timing, survey_sep) |>
  filter(year != 2019) |>
  reframe(sum = n())

#are the length comps the same across the hooks / survey??
comp <- comp |>
  filter(sex %in% c(1,2) ) |>
  mutate(id = paste0(year, survey_timing, hooksize_desc)) |>
  mutate(label =paste(survey_timing, year, hooksize_desc))
unique(comp$label)

fig <- comp |>
  group_by(id) |>
  #filter(id == "hbll 2023") |>
  ggplot(aes(length, group = as.factor(id), fill = as.factor(hooksize_desc))) +
  geom_histogram() + facet_wrap(~survey_timing+ year+ sex, ncol = 2, scales = "free") +
  theme_classic() +
  theme(strip.text.x = element_blank()) +
  scale_fill_manual(values = c("grey20", "grey80")) +
  geom_text(
    data = comp,
  aes(x = 60, y = 150, label = label),
  color = "black",
  size = 5
) +
  labs(fill = "Hook size")
ggsave(paste0("figures/length_histograms_sex_survey.png"), fig, height = 10, width = 10, dpi = 200)

comp |>
  group_by(sex, survey_timing, hooksize_desc) |>
  drop_na(length) |>
  reframe(min = min(length), max = max(length), mean = mean(length), median = median(length)) |>
  filter(sex %in% c(1, 2))

fig <-
  comp |>
  filter(year != 2019) |>
  group_by(id) |>
  #filter(id == "hbll 2023") |>
  ggplot(aes(as.factor(sex), length, group = as.factor(hooksize_desc), fill = as.factor(hooksize_desc))) +
  geom_boxplot() + facet_wrap(~survey_timing + sex, ncol = 2, scales = "free") +
  theme_classic() +
  #theme(strip.text.x = element_blank()) +
  scale_fill_manual(values = c("grey30", "grey80")) +
  # geom_text(
  #   data = comp,
  #   aes(x = 60, y = 150, label = label),
  #   color = "black",
  #   size = 5
  # ) +
  labs(fill = "Hook size")

fig
ggsave(paste0("figures/mean_length_boxplot.png"), fig, height = 10, width = 10, dpi = 200)


#seasonality summary
comp |>
  filter(sex %in% c(1, 2)) |>
  group_by(season, sex, hooksize_desc) |>
  filter(year != 2019) |>
  reframe(sum = n()) #make this wide then calculate ratios with gear but across seasons?

comp <- comp |>
  filter(sex %in% c(1,2) ) |>
  mutate(id_season = paste0(year, hooksize_desc)) |>
  mutate(label =paste(survey_timing, year, hooksize_desc))
unique(comp$label)

comp |>
  filter(sex %in% c(1, 2)) |>
  group_by(sex, hooksize_desc) |>
  filter(year != 2019) |>
  drop_na(length) |>
  reframe(min = min(length), max = max(length))

fig <-
  comp |>
  group_by(id) |>
  #filter(id == "hbll 2023") |>
  ggplot(aes(length, group = as.factor(season), fill = as.factor(season))) +
  geom_histogram() + facet_wrap(~sex, ncol = 2, scales = "free") +
  theme_classic() +
  #theme(strip.text.x = element_blank()) +
  scale_fill_manual(values = c("grey20", "grey80")) +
 # geom_text(
  #  data = comp,
  #  aes(x = 60, y = 150, label = label),
  #  color = "black",
  #  size = 5
 # ) +
  labs(fill = "Season")
fig
ggsave(paste0("figures/length_histograms_season.png"), fig, height = 8, width = 10, dpi = 200)

comp |>
  group_by(sex, season, hooksize_desc) |>
  drop_na(length) |>
  reframe(min = min(length), max = max(length), mean = mean(length), median = median(length)) |>
  filter(sex %in% c(1, 2))

fig <-
comp |>
  filter(year != 2019) |>
  group_by(id) |>
  #filter(id == "hbll 2023") |>
  ggplot() +
  #ggplot(aes(as.factor(sex), length, group = as.factor(season), fill = as.factor(season))) +
  #geom_boxplot() +
  facet_wrap(~ sex, ncol = 2, scales = "free") +
  geom_jitter (aes(as.factor(season), length, group = as.factor(season), colour = as.factor(season),  alpha = 0.25)) +
  geom_violin (aes(as.factor(season), length, group = as.factor(season),  fill = as.factor(season)), colour = "black") +
  theme_classic() +
  #theme(strip.text.x = element_blank()) +
  scale_colour_manual(values = c("grey30", "grey80")) +
  scale_fill_manual(values = c("grey30", "grey80")) +
  # geom_text(
  #   data = comp,
  #   aes(x = 60, y = 150, label = label),
  #   color = "black",
  #   size = 5
  # ) +
  labs(fill = "Season")

fig
ggsave(paste0("figures/mean_length_season_violin.png"), fig, height = 5, width = 8, dpi = 200)

df <- comp |> mutate(sex_factor = as.factor(sex), season_factor = as.factor(season))
one_way <- aov(length ~ season *   sex_factor, data = df)
summary(one_way)
TukeyHSD(one_way)
ggplot(df, aes(x = factor(season), y = length, fill = factor(season))) +
  geom_boxplot() +
  labs(title = "ANOVA Results", x = "Group", y = "Dependent Variable") +
  theme_minimal()

tukey.plot.test<-TukeyHSD(one_way)
plot(tukey.plot.test, las = 1)




#hbll two depths compared to dogfish survey
comp <- comp |> mutate(season = ifelse(season == 3, "summer", "fall"))
comp <- comp |> mutate(sex = ifelse(sex == 1, "male", "female"))
comp <- comp  |>
  mutate(season = factor(season,
                          levels = c("summer", "fall")))
fig <-
  comp |>
  ggplot(aes((grouping_depth_id), length,  group = grouping_depth_id, fill = (grouping_depth_id))) +
  geom_jitter(aes((grouping_depth_id), length), colour = "grey80") +
  geom_boxplot() +
  facet_wrap(~sex + season, ncol = 2) +
  theme_classic() +
  #theme(strip.text.x = element_blank()) +
  #scale_fill_manual(values = c("grey20", "grey80")) +
  labs(fill = "Depth stata") +
  scale_fill_viridis_d()
fig
ggsave(paste0("figures/length_by_depth_and_season.png"), fig, height = 8, width = 10, dpi = 200)

comp |>
  group_by(sex, season, hooksize_desc) |>
  drop_na(length) |>
  reframe(min = min(length), max = max(length), mean = mean(length), median = median(length)) |>
  filter(sex %in% c(1, 2))

fig <-
  comp |>
  filter(year != 2019) |>
  group_by(id) |>
  #filter(id == "hbll 2023") |>
  ggplot() +
  #ggplot(aes(as.factor(sex), length, group = as.factor(season), fill = as.factor(season))) +
  #geom_boxplot() +
  facet_wrap(~ sex, ncol = 2) +
  geom_jitter (aes(as.factor(season), length, group = as.factor(season), colour = as.factor(season),  alpha = 0.25)) +
  geom_violin (aes(as.factor(season), length, group = as.factor(season),  fill = as.factor(season)), colour = "black") +
  theme_classic() +
  #theme(strip.text.x = element_blank()) +
  scale_colour_manual(values = c("grey30", "grey80")) +
  scale_fill_manual(values = c("grey30", "grey80")) +
  # geom_text(
  #   data = comp,
  #   aes(x = 60, y = 150, label = label),
  #   color = "black",
  #   size = 5
  # ) +
  labs(fill = "Season")

fig
ggsave(paste0("figures/mean_length_season_violin.png"), fig, height = 5, width = 8, dpi = 200)

df <- comp |> mutate(sex_factor = as.factor(sex), season_factor = as.factor(season))
one_way <- aov(length ~ season *   sex_factor, data = df)
summary(one_way)
TukeyHSD(one_way)
ggplot(df, aes(x = factor(season), y = length, fill = factor(season))) +
  geom_boxplot() +
  labs(title = "ANOVA Results", x = "Group", y = "Dependent Variable") +
  theme_minimal()

tukey.plot.test<-TukeyHSD(one_way)
plot(tukey.plot.test, las = 1)






comp |>
  filter(sex %in% c(1, 2)) |>
  group_by(sex) |>
  filter(year != 2019) |>
  reframe(sum = n())

comp |>
  filter(sex %in% c(1, 2)) |>
  group_by(sex, hooksize_desc, year, survey_timing, survey_sep) |>
  filter(year != 2019) |>
  reframe(sum = n())




comp |> #<- more males and more females captured in the fall, otherwise very similar
  filter(sex %in% c(1, 2)) |>
  group_by(sex, survey_timing, hooksize_desc, year) |>
  filter(year != 2019) |>
  reframe(sum = n()) |>
  left_join(final_year) |>
  mutate(cpue = sum/sumhooks) |

# length differences
comp |>
  filter(sex %in% c(1, 2)) |>
  group_by(sex, hooksize_desc) |>
  filter(year != 2019) |>
  drop_na(length) |>
  reframe(min = min(length), max = max(length))

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
