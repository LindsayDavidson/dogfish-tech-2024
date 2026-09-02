# calculate samples for report
# length depth plot, and plots for SOM

library(tidyverse)

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
  #filter(year != 2019) |>
  reframe(sum = n())

#are the length comps the same across the hooks / survey??
comp <- comp |>
  filter(sex %in% c(1,2) ) |>
  mutate(id = paste0(year, survey_timing, hooksize_desc)) |>
  mutate(label =paste(survey_timing, year, hooksize_desc))
unique(comp$label)

comp <- comp |> mutate(sex_text = ifelse(sex == 1, "male", ifelse(sex == 2, "female", "NA")))
unique(comp$id)
comp <- comp  |>
  mutate(survey_timing = forcats::fct_relevel(survey_timing,
                                              "hbll", 'dog'))


comp <- comp |>
  mutate(surveytimingyear = paste(survey_timing, year))

unique(comp$surveytimingyear)

comp$surveytimingyear  <- factor(comp$surveytimingyear, levels = c("hbll 2019", "hbll 2022", "hbll 2023", "dog 2023" ))

fig <-
  comp |>
  #mutate(surveytimingyear = paste(survey_timing, year)) |>
  #mutate(surveytimingyear = factor(surveytimingyear, levels = c("HBLL 2019", "HBLL 2022", "HBLL 2023", "Dog 2023" ))) |>
  group_by(id) |>
  filter(sex_text %in% c("male", "female")) |>
  #filter(id == "hbll 2023") |>
  ggplot(aes(length, group = as.factor(id), fill = as.factor(hooksize_desc))) +
  geom_histogram() +
  facet_grid(rows = vars(surveytimingyear), cols = vars(sex_text), scales = "free") +
  theme_classic() +
  #theme(strip.text.x = element_blank()) +
  scale_fill_manual(values = c("grey20", "grey80"), guide = NULL) +
  #labs(fill = "Hook size") +
  labs(fill = "Hook size", x = "Length (TLext, cm)", y = "Count" ) +
  theme(
    axis.text = element_text(size = 15 ),
    axis.title = element_text(size = 15),
    strip.text = element_text(size = 15)
  )
fig


comp |>
  group_by(sex, survey_timing, hooksize_desc) |>
  drop_na(length) |>
  reframe(min = min(length), max = max(length), mean = mean(length), median = median(length)) |>
  filter(sex %in% c(1, 2))

fig2 <-
  comp |>
  filter(year != 2019) |>
  filter(sex_text %in% c("male", "female")) |>
  group_by(id) |>
  #filter(id == "hbll 2023") |>
  ggplot(aes(as.factor(hooksize_desc), length, group = as.factor(hooksize_desc), fill = as.factor(hooksize_desc))) +
  geom_jitter (aes(as.factor(hooksize_desc), length, group = as.factor(hooksize_desc), colour = as.factor(hooksize_desc)),  alpha = 0.25) +
  #geom_boxplot() +
  geom_violin(alpha = 0.45,
              draw_quantiles  = c(0.25, 0.5, 0.75),
              trim = FALSE
  ) +
  facet_grid(rows = vars(survey_timing), cols = vars(sex_text), scales = "free") +
  theme_classic() +
  #theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank()) +
  #theme(axis.text.x = element_blank()) +
  scale_fill_manual(values = c("grey30", "grey80")) +
  scale_colour_manual(values = c("grey30", "grey80"), guide = NULL) +
  labs(fill = "Hook size") +
  labs(x = "Hook size", y = "Length (TLext, cm)")  +
  theme(
    axis.text = element_text(size = 15 ),
    axis.title = element_text(size = 15),
    strip.text = element_text(size = 15)
  )

fig2

cv <- cowplot::plot_grid(
  fig, fig2,
  ncol = 1,
  nrow = 2,
  labels = c("(a)", "(b)"),   # Labels for each plot
  #align = "hv",
  rel_heights = c(1.5,1),
  rel_widths = rep(0.75,1)
)

cv

ggsave(paste0("figures/length_boxplot.png"), cv, height = 9, width = 7, dpi = 200)



# seasonality summary -----------------------------------------------------

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

comp <- comp |> mutate(season_text = ifelse(season == 3, "summer" , ifelse(season == 4, "fall", "NA")))
comp <- comp  |> mutate(season_text = forcats::fct_relevel(season_text,  "summer",  "fall"))

fig <-
  comp |>
  group_by(id ) |>
  filter(sex %in% c(1, 2)) |>
  #filter(id == "hbll 2023") |>
  ggplot(aes(length, group = as.factor(season), fill = as.factor(season_text))) +
  geom_histogram() +
  labs(x = "Length (TLext, cm)")  +
  facet_grid(cols = vars(sex_text), rows = vars(hooksize_desc), scales = "free") +
  theme_classic() +
  #theme(strip.text.x = element_blank()) +
  scale_fill_manual(values = c("grey80", "grey20")) +
  labs(fill = "Season") +
  theme(
    axis.text = element_text(size = 15 ),
    axis.title = element_text(size = 15),
    strip.text = element_text(size = 15)
  )
fig


fig_comp <-
  comp |>
  group_by(id ) |>
  filter(sex %in% c(1, 2)) |>
  filter(id %in% c("2023hbll13/0", "2023dog14/0", "2022hbll13/0" , "2019hbll13/0")) |>
  #filter(id == "hbll 2023") |>
  ggplot(aes(length, group = as.factor(season), fill = as.factor(season_text))) +
  geom_histogram() +
  labs(x = "Length (TLext, cm)")  +
  facet_grid(cols = vars(sex_text), scales = "free") +
  theme_classic() +
  #theme(strip.text.x = element_blank()) +
  scale_fill_manual(values = c("grey80", "grey20")) +
  labs(fill = "Season") +
  theme(
    axis.text = element_text(size = 15 ),
    axis.title = element_text(size = 15),
    strip.text = element_text(size = 15)
  )
fig_comp


comp |>
  group_by(sex, season_text, hooksize_desc) |>
  drop_na(length) |>
  reframe(min = min(length), max = max(length), mean = mean(length), median = median(length)) |>
  filter(sex %in% c(1, 2))

fig2 <-
  comp |>
  filter(year != 2019) |>
  filter(id %in% c("2023hbll13/0", "2023dog14/0", "2022hbll13/0" , "2019hbll13/0")) |>
  group_by(id) |>
  filter(sex %in% c(1, 2)) |>
  #filter(id == "hbll 2023") |>
  ggplot() +
  facet_grid(cols = vars(sex_text),   scales = "free") +
  geom_jitter (aes(as.factor(season_text), length, group = as.factor(season_text), colour = as.factor(season_text)),  alpha = 0.25) +

  geom_violin(aes(as.factor(season_text), length, group = as.factor(season_text),  fill = as.factor(season_text)), alpha = 0.45,
              draw_quantiles  = c(0.25, 0.5, 0.75),
              trim = FALSE
  ) +

  #geom_boxplot (aes(as.factor(season_text), length, group = as.factor(season_text),  fill = as.factor(season_text)), colour = "black") +
  theme_classic() +
  theme(axis.title.x = element_blank()) +
  scale_colour_manual(values = c("grey80", "grey30"), guide = NULL) +
  scale_fill_manual(values = c("grey80", "grey30" ), guide = NULL) +
  labs(fill = "Season") +
  theme(
    axis.text = element_text(size = 15 ),
    axis.title = element_text(size = 15),
    strip.text = element_text(size = 15)
  )

fig2


cv <- cowplot::plot_grid(
  fig, fig2,
  ncol = 1,
  nrow = 2,
  labels = c("(a)", "(b)"),
  #align = "hv",
  rel_heights = c(0.75,1),
  rel_widths = c(1, 0.5)
)

cv

ggsave(paste0("figures/mean_length_season_boxplot.png"), cv, height = 9, width = 7, dpi = 200)


#linear regression

#females length and season
df <- comp |> mutate(sex_factor = as.factor(sex), season_factor = as.factor(season))
df <- df |> mutate(keep = ifelse(season == 3 & hooksize_desc == "13/0" & grouping_depth_id %in% c("D2", "D3"), "keep",
                                 ifelse(season == 4 & hooksize_desc == "14/0", "keep", "erase"))) |>
  filter(keep == "keep")

df_f <- filter(df, sex == 2)
df_m <- filter(df, sex == 1)

test <- glm(length ~ season, data = df_f, family = Gamma(link = "log"))
test
coef(test)
summary(test)

preds <- predict(test, newdata = newdata, type = "link", se.fit = TRUE)
crit_val <- qnorm(0.975) # 1.96 for a standard normal distribution
link_lower <- preds$fit - crit_val * preds$se.fit
link_upper <- preds$fit + crit_val * preds$se.fit
newdata$pred_response <- test$family$linkinv(preds$fit)
newdata$ci_lower      <- test$family$linkinv(link_upper)
newdata$ci_upper      <- test$family$linkinv(link_lower)
newdata #females length and season


#males and season
test_m <- glm(length ~ season, data = df_m, family = Gamma(link = "log"))
test_m
coef(test_m)
summary(test_m)
newdata <- data.frame(season = as.factor(c(3, 4)))
preds <- predict(test_m, newdata = newdata, type = "link", se.fit = TRUE)
crit_val <- qnorm(0.975) # 1.96 for a standard normal distribution
link_lower <- preds$fit - crit_val * preds$se.fit
link_upper <- preds$fit + crit_val * preds$se.fit
newdata$pred_response <- test_m$family$linkinv(preds$fit)
newdata$ci_lower      <- test_m$family$linkinv(link_upper)
newdata$ci_upper      <- test_m$family$linkinv(link_lower)


#females and season for different hooks (HBLL versus Dog)
df <- comp |> mutate(sex_factor = as.factor(sex), season_factor = as.factor(season))
df <- df |> mutate(keep = ifelse(season == 3 & hooksize_desc == "13/0", "keep",
                                 ifelse(season == 4 & hooksize_desc == "14/0", "keep", "erase"))) |> filter(keep == "keep")
df_f <- filter(df, sex == 2)
test <- glm(length ~ season, data = df_f, family = Gamma(link = "log"))
test
coef(test)
summary(test)
preds <- predict(test, newdata = newdata, type = "link", se.fit = TRUE)
crit_val <- qnorm(0.975) # 1.96 for a standard normal distribution
link_lower <- preds$fit - crit_val * preds$se.fit
link_upper <- preds$fit + crit_val * preds$se.fit
newdata$pred_response <- test$family$linkinv(preds$fit)
newdata$ci_lower      <- test$family$linkinv(link_upper)
newdata$ci_upper      <- test$family$linkinv(link_lower)
newdata


#males and season for different hooks (HBLL versus Dog)
df <- comp |> mutate(sex_factor = as.factor(sex), season_factor = as.factor(season))
df <- df |> mutate(keep = ifelse(season == 3 & hooksize_desc == "13/0", "keep",
                                 ifelse(season == 4 & hooksize_desc == "14/0", "keep", "erase"))) |>
  filter(keep == "keep")
df_m <- filter(df, sex == 1)
test <- glm(length ~ season, data = df_m, family = Gamma(link = "log"))
test
coef(test)
summary(test)
preds <- predict(test, newdata = newdata, type = "link", se.fit = TRUE)
crit_val <- qnorm(0.975) # 1.96 for a standard normal distribution
link_lower <- preds$fit - crit_val * preds$se.fit
link_upper <- preds$fit + crit_val * preds$se.fit
newdata$pred_response <- test$family$linkinv(preds$fit)
newdata$ci_lower      <- test$family$linkinv(link_upper)
newdata$ci_upper      <- test$family$linkinv(link_lower)
newdata


# lengths by depths -------------------------------------------------------

#hbll two depths compared to dogfish survey

comp$sex <- as.factor(comp$sex)
comp <- comp |> as.factor(comp$season)
comp$season <- as.factor(comp$season)
levels(comp$sex) <- c("male", "female")
levels(comp$season) <- c("summer", "fall")

fig <-
  comp |>
  filter(sex %in% c("male", "female")) |>
  ggplot(aes((grouping_depth_id), length,  group = grouping_depth_id)) +
  geom_jitter(aes(grouping_depth_id, length, colour = sex), alpha = 0.25) +
  #geom_boxplot() +
  geom_violin(alpha = 0.25,
    draw_quantiles  = c(0.25, 0.5, 0.75),
    trim = FALSE
  ) +
  scale_colour_hue(l = 45, guide = guide_legend(override.aes = list(size = 3,
                                                                    alpha = 1))) +
  facet_grid(rows =vars(sex), cols = vars(season)) +
  theme_classic() +
  labs(x = "Depth group") +
  labs(y = "Length (TLext cm)") +
  #labs(fill = "Depth group") + #could be na
  guides(fill = "none", colour = "none") + #could be na
  scale_fill_grey(start = 0.2, end = 0.8) +
  theme(
    axis.text = element_text(size = 15 ),
    axis.title = element_text(size = 15),
    strip.text = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15)
  )
fig

ggsave(paste0("figures/mean_length_depth_boxplot.png"), fig, height = 5, width = 5, dpi = 200)

comp |>
  group_by(sex, season, hooksize_desc) |>
  drop_na(length) |>
  reframe(min = min(length), max = max(length), mean = mean(length), median = median(length)) |>
  filter(sex %in% c(1, 2))


fig2 <-
  comp |>
  filter(year != 2019) |>
  group_by(id) |>
  filter(sex %in% c("male", "female")) |>
  mutate(id2 = paste0(survey_lumped, hooksize_desc, season_text))  |>
  filter(id2 %in% c("hbll13/0summer" ,  "dog14/0fall")) |>
  mutate(id = paste0(survey_lumped, grouping_depth_id ) )  |>
  filter(id %in% c("dogD4" ,"dogD5",  "hbllD2", "dogD2",  "hbllD3" ,"dogD3")) |>
  ggplot() +
  facet_grid(cols = vars(sex_text), scales = "free") +
  geom_jitter (aes(as.factor(season_text), length, group = as.factor(season_text), colour = as.factor(season_text)),  alpha = 0.25) +
  #geom_violin (aes(as.factor(season_text), length, group = as.factor(season_text),  fill = as.factor(season_text)), colour = "black") +
  geom_boxplot (aes(as.factor(season_text), length, group = as.factor(season_text),  fill = as.factor(season_text)), colour = "black") +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15 ),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 15),
    strip.text = element_text(size = 15)
  ) +
  labs(x = NULL) +
  labs(y = "Length (TLext cm)") +
  scale_x_discrete(breaks = c("summer", "fall"),
                     labels = c("HBLL survey\n(2 depths)", "Dogfish survey\n(4 depths)")) +
  #theme(axis.title.x = element_blank()) +
  scale_colour_manual(values = c("grey30", "grey80"), guide = NULL) +
  scale_fill_manual(values = c("grey30", "grey80"), guide = NULL) +
  labs(fill = "Season")

fig2

cv <- cowplot::plot_grid(
  fig, fig2,
  ncol = 1,
  nrow = 2,
  labels = c("(a)", "(b)"),
  #align = "hv",
  rel_heights = c(2,1),
  rel_widths = rep(1)
)

cv

ggsave(paste0("figures/mean_length_depth_boxplot.png"), cv, height = 9, width = 7, dpi = 200)

