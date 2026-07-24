# outputs for manuscript

final <- readRDS("data-raw/wrangled-hbll-dog-sets.rds") # no west coast VI expansion set

# 2019 survey
d19 <- filter(final, year == 2019 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS")
range(d19$time_deployed)
dim(filter(d19, hooksize_desc == "13/0"))
dim(filter(d19, hooksize_desc == "14/0"))

d19 |>
  filter(hooksize_desc == "13/0") |>
  reframe(print(unique(grouping_desc)))
d19 |>
  filter(hooksize_desc == "14/0") |>
  reframe(print(unique(grouping_desc)))

d192 <- d19 |>
  group_by(hooksize_desc) |>
  reframe(sum = sum(catch_count), sumeffort = sum(lglsp_hook_count)) |>
  mutate(cpue = sum / sumeffort) |>
  #print()
  mutate(Survey = "HBLL", Year = 2019)


# 2022 survey
d22 <- filter(final, year == 2022 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS")
range(d22$time_deployed)
unique(d22$survey_sep)
dim(filter(d22, hooksize_desc == "13/0"))
dim(filter(d22, hooksize_desc == "14/0"))
unique(d22$grouping_desc)


d222 <- d22 |>
  group_by(hooksize_desc) |>
  reframe(sum = sum(catch_count), sumeffort = sum(lglsp_hook_count)) |>
  mutate(cpue = sum / sumeffort) |>
  #print()
  mutate(Survey = "HBLL", Year = 2022)


# 2023 sd22# 2023 survey HBLL
d23 <- filter(final, year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS")
unique(d23$survey_sep)

d23 <- d23 |> filter(season != 4)
range(d23$time_deployed)
unique(d23$survey_sep)

jhookcomp <- filter(d23, survey_sep == "dog-jhook")
d23 <- d23 |> filter(!fishing_event_id %in% c(jhookcomp$fishing_event_id))
range(d23$time_deployed)
unique(d23$survey_sep)
dim(filter(d23, hooksize_desc == "13/0"))
dim(filter(d23, hooksize_desc == "14/0"))

x <- filter(d23, hooksize_desc == "14/0")
xx <- filter(d23, hooksize_desc == "13/0" & fishing_event_id %in% (x$fishing_event_id))

d23 |>
  filter(hooksize_desc == "13/0") |>
  reframe(print(unique(grouping_desc))) |>
  print (n = 100)

d23 |>
  filter(hooksize_desc == "14/0") |>
  reframe(print(unique(grouping_desc)))


d232 <- d23 |>
  group_by(hooksize_desc) |>
  reframe(sum = sum(catch_count), sumeffort = sum(lglsp_hook_count)) |>
  mutate(cpue = sum / sumeffort) |>
  #print()
  mutate(Survey = "HBLL", Year = 2023)


# 2023 survey fall Dogfish surve7
d23 <- filter(final, year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS")
range(d23$time_deployed)
unique(d23$survey_sep)

d23 <- d23 |> filter(season == 4)
jhookcomp <- filter(d23, survey_sep == "dog-jhook")
d23 <- d23 |> filter(!fishing_event_id %in% c(jhookcomp$fishing_event_id))

range(d23$time_deployed)
dim(filter(d23, hooksize_desc == "13/0"))
dim(filter(d23, hooksize_desc == "14/0"))
x <- filter(d23, hooksize_desc == "14/0")
xx <- filter(d23, hooksize_desc == "13/0" & fishing_event_id %in% (x$fishing_event_id))
unique(d23$grouping_desc)

d23 |>
  filter(hooksize_desc == "13/0") |>
  reframe(print(unique(grouping_desc))) |>
  print(n = 100)

d23 |>
  filter(hooksize_desc == "14/0") |>
  reframe(print(unique(grouping_desc)))

d23dog <- d23 |>
  group_by(hooksize_desc) |>
  reframe(sum = sum(catch_count), sumeffort = sum(lglsp_hook_count)) |>
  mutate(cpue = sum / sumeffort) |>
  #print()
  mutate(Survey = "Dogfish", Year = 2023)


summary <- bind_rows(d23dog, d232) |>
  bind_rows(d222) |>
  bind_rows(d192)


#add in the counts and cpue by male/female

samps <- readRDS("data-raw/dogfish_samples_cleaned.rds")

comp <- samps |>
  filter(year == 2019 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" |
           year == 2022 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" |
           year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS") |>
  mutate(Survey = ifelse(survey_timing == "hbll", "HBLL", "Dogfish"))
jhook <- filter(comp, hooksize_desc == "12/0")
comp <- filter(comp, !fishing_event_id %in% c(jhook$fishing_event_id))


#need unique hook counts per survey and year
hookcount <- comp |>
  dplyr::select(lglsp_hook_count, hooksize_desc, year, survey_timing, Survey, survey_sep, fishing_event_id) |>
  distinct() |>
  group_by(hooksize_desc, year, Survey) |>
  summarize(hookcount = sum(lglsp_hook_count))


final_samps <- comp |>
  filter(sex %in% c(1, 2)) |>
  dplyr::select(lglsp_hook_count, sex, hooksize_desc, year, survey_timing, Survey, survey_sep, fishing_event_id) |>
  group_by(sex, hooksize_desc, year, survey_timing, Survey, survey_sep, fishing_event_id, lglsp_hook_count) |>
  summarize(sum_sex = n()) |>

  group_by(sex, hooksize_desc, year, survey_timing, Survey, survey_sep) |>
  summarize(sum_sex2 = sum(sum_sex))

final_samps <- final_samps |>
  left_join(hookcount) |>

  mutate(cpue_sex = sum_sex2 / hookcount) |>
  #filter(year != 2019) |>
  rename(Year = year) |>
  dplyr::select(-survey_timing, -survey_sep) |>
  ungroup()


count <- final_samps  |>
  dplyr::select(-cpue_sex, -survey_timing) |>
  pivot_wider(
    names_from = sex,
    values_from = sum_sex2
  )

cpue <- final_samps %>%
  dplyr::select(-sum_sex2) |>
  pivot_wider(
    names_from = sex,
    values_from = cpue_sex
  ) |>
  rename(malecpue = `1`, femalecpue = `2`)

sum_sex <- left_join(count, cpue)


together <- left_join(sum_sex, summary)
together <- together |>
  mutate(Hook_type = paste0(hooksize_desc, " (", Survey, ")"),
         Count_cpue = paste0(sum, " (", round(cpue,3), ")"),
         Male_cpue = paste0(`1`, " (", round(malecpue,3), ")"),
         Female_cpue = paste0(`2`, " (", round(femalecpue,3), ")")) |>
  dplyr::select(-c(hooksize_desc, `1`, `2`, sum, cpue))


together |>
  dplyr::select(Year, Survey, Hook_type, Count_cpue, Male_cpue, Female_cpue)  |>
  arrange(Year) |>
  knitr::kable(
  # format = "simple",
  #"html",
  format = "latex",
  col.names = c(
    "Year",  "Survey", "Hook type", "Count (CPUE)", "Male (CPUE)", "Female (CPUE)"),
  booktabs = TRUE,
  #align = "llllll",
  align = "c",
  caption = "Comparison of Dogfish catches and CPUE per gear type on each comparative survey (HBLL
2019, HBLL 2022, HBLL 2023, and Dogfish 2023). Each hook type, except for 2019, were on the same
set. HBLL gear is hook size 13/0 baited with squid which Dogfish gear is hook size 14/0 baited with
herring",
  label = "cpue-catches"
) |>
  kableExtra::row_spec(0, bold = TRUE)  |>  # header styling
  kableExtra::kable_styling(full_width = FALSE, stripe_color = "grey95")


