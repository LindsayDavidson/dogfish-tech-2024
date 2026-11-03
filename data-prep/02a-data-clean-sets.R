library(sf)
library(tidyverse)
library(sp)

spring <- seq(12, 25, 1)
summer <- seq(26, 38, 1)
winter <- c(seq(1, 11, 1), 52)
fall <- seq(39, 51, 1)

sets <- readRDS("data-raw/dogfish_sets_getall.rds")


# fix 2004 soak times  --------
# soak times are not in the this database but are in the 2005 tech report

sets1 <- sets |>
  mutate(erase = ifelse(year == 2004 & survey_abbrev == "OTHER", "erase", "keep")) |>
  filter(erase == "keep") |>
  dplyr::select(-erase)
comp2004 <- sets |> filter(year == 2004 & survey_abbrev == "OTHER")

test <- comp2004 |>
  dplyr::select(time_retrieved, grouping_desc, duration_min, latitude, longitude) |>
  distinct()
test <- test |>
  dplyr::select(time_retrieved) |>
  mutate(time_retrieved = as.POSIXct(time_retrieved, format("%H:%M:%S"), tz = "UTC"))
test <- test %>% arrange(time_retrieved)
time <- format(test$time_retrieved, "%H:%M:%S")

location <- c("Hornby Island", "Hornby Island", "Hornby Island", "Hornby Island", "Sinclair Bank", "Sinclair Bank", "Sinclair Bank", "Sinclair Bank", "Epsom Point", "Epsom Point", "Epsom Point", "Halibut Bank", "Halibut Bank", "Halibut Bank", "Halibut Bank", "Porlier Pass", "Porlier Pass", "Porlier Pass", "Porlier Pass", "Active Pass", "Active Pass", "Active Pass", "Northumberland Channel")
depth <- c(1, 2, 3, 4, 4, 3, 2, 1, 4, 2, 1, 3, 3, 4, 2, 4, 3, 1, 2, 5, 4, 5, NA)
duration_min <- c(124, 144, 167, 121, 128, 178, 129, 130, 132, 179, 107, 126, 136, 132, 143, 139, 135, 131, 122, 128, 128, 96, 84)
time <- time

comp <- data.frame(location, depth, duration_min, time)
depthd <- data.frame(depth = c(1, 2, 3, 4, 5), depthm = c("0 - 55 m", "56 - 110 m", "111 - 165 m", "166 - 220 m", "> 220 m"))
comp <- left_join(comp, depthd)
comp <- comp |> mutate(grouping_desc = paste0("SoG Dogfish Site ", location, ", Depth Stratum ", depth, ": ", depthm))
comp <- comp |> dplyr::select(grouping_desc, duration_min, time)

comp2004 <- comp2004 |>
  mutate(time = format(time_retrieved, "%H:%M:%S")) |>
  dplyr::select(-duration_min)
comp2004 <- left_join(comp2004, comp) |>
  dplyr::select(-time)


sets <- rbind(sets1, comp2004)

# QA/QC dates and depth--------------------------------
# create a consistent grouping depth id
# can use the depth_m column in the model however, may be useful to have a consistent grouping depth id if we change the grid prediction cells to shallow or deep

# check depths
unique(sets$grouping_desc) # NAs and a 'SOG Dogfish Site'
unique(sets$grouping_depth_id) # inconsistent

sets_nas <- sets |>
  filter(is.na(grouping_desc) == TRUE) |>
  mutate(grouping_desc = case_when(
    depth_m <= 55 & survey_abbrev %in% c("DOG", "OTHER") ~ "SoG Dogfish 0 - 55 m",
    depth_m > 55 & depth_m <= 110 & survey_abbrev %in% c("DOG", "OTHER") ~ "SoG Dogfish 56 - 110 m",
    depth_m > 110 & depth_m <= 165 & survey_abbrev %in% c("DOG", "OTHER") ~ "SoG Dogfish 111 - 165 m",
    depth_m > 166 & depth_m <= 220 & survey_abbrev %in% c("DOG", "OTHER") ~ "SoG Dogfish 166 - 220 m",
    depth_m > 220 & survey_abbrev %in% c("DOG", "OTHER") ~ "SoG Dogfish > 200 m",
    depth_m <= 70 & survey_abbrev == "HBLL INS N" ~ "HBLL IN North, 40 - 70 m",
    depth_m > 70 & survey_abbrev == "HBLL INS N" ~ "HBLL IN North, 71 - 100 m",
    depth_m <= 70 & survey_abbrev == "HBLL INS S" ~ "HBLL IN South, 40 - 70 m",
    depth_m > 70 & survey_abbrev == "HBLL INS S" ~ "HBLL IN South, 71 - 100 m"
  ))

sets <- bind_rows(sets_nas, filter(sets, is.na(grouping_desc) != TRUE))

unique(sets$grouping_depth_id) # inconsistent

sets <- sets |>
  mutate(grouping_depth_id = case_when(
    grouping_desc == "SoG Dogfish 0 - 55 m" ~ 1,
    grouping_desc == "SoG Dogfish 56 - 110 m" ~ 2,
    grouping_desc == "SoG Dogfish 111 - 165 m" ~ 3,
    grouping_desc == "SoG Dogfish 166 - 220 m" ~ 4,
    grouping_desc == "SoG Dogfish > 200 m" ~ 5,
    grouping_desc == "SoG Dogfish > 220 m" ~ 6,
    grouping_desc == "HBLL IN North, 40 - 70 m" ~ 1,
    grouping_desc == "HBLL IN South, 40 - 70 m" ~ 1,
    grouping_desc == "HBLL IN North, 71 - 100 m" ~ 2,
    grouping_desc == "HBLL IN South, 71 - 100 m" ~ 2
  ))

# check
sets |>
  filter(grouping_desc == "SoG Dogfish Site") # none, fixed now
unique(sets$grouping_depth_id) # good


# QA/QC soak time  -----------------------------------------------------

range(sets$duration_min, na.rm = TRUE)

sets <- sets |>
   mutate(
     retrieve = as.Date(time_begin_retrieval, format = "%Y-%m-%d h:m:s"),
     julian = lubridate::yday(retrieve),
     week = lubridate::week(retrieve))

# some soaks are NA - fix this!
sets |>
  filter(is.na(duration_min) == TRUE) |>
  distinct(fishing_event_id, .keep_all = TRUE) |>
  tally() # 44 fishing events are missing soak times as the deployment time wasnt recorded

sets |>
  filter(is.na(duration_min == TRUE)) |>
  distinct(fishing_event_id, .keep_all = TRUE) |>
  group_by(year) |>
  tally() # lots of 2005s missing too soak time should have been consistently 2 hours at this time, the time_end_deployment was not recorded in 2005

# 2005 was two hour soak so I am going to add that in
sets <- sets |> mutate(soak = ifelse(is.na(duration_min) == TRUE, 60, duration_min))

# sets <- sets |>
#   mutate(
#     retrieve = as.Date(time_begin_retrieval, format = "%Y-%m-%d h:m:s"),
#     julian = lubridate::yday(retrieve),
#     week = lubridate::week(retrieve))

# Add grouping code for survey ---------------------------------------------

# this is so I can look at trends through time, designed based, that don't include the comp work
final <- sets |>
  mutate(survey_sep = case_when(
    survey_abbrev == "HBLL INS S" ~ "HBLL INS S",
    survey_abbrev == "HBLL INS N" ~ "HBLL INS N",
    year %in% c(1986, 1989) ~ "dog-jhook",
    year == 2024 & hooksize_desc == "12/0" ~ "dog-jhook",
    year == 2024 & hooksize_desc == "14/0" ~ "dog comp",
    year %in% c(2005, 2008, 2011, 2014, 2019) & survey_abbrev == "DOG" ~ "dog",
    year == 2019 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" &
      hooksize_desc == "13/0" ~ "hbll comp",
    year == 2019 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" &
      hooksize_desc == "14/0" ~ "dog comp",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" &
      hooksize_desc == "12/0" ~ "dog-jhook",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" &
      hooksize_desc == "14/0" & month == 9 & day >= 27 ~ "dog comp",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" &
      hooksize_desc == "13/0" & month == 9 & day >= 27 ~ "hbll comp",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" &
      hooksize_desc == "13/0" & month == 9 & day < 27 ~ "hbll comp",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" &
      hooksize_desc == "14/0" & month == 9 & day < 27 ~ "dog comp",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" &
      hooksize_desc == "13/0" & month == 8 ~ "hbll comp",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" &
      hooksize_desc == "14/0" & month == 8 ~ "dog comp",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" &
      hooksize_desc == "13/0" & month == 10 ~ "hbll comp",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" &
      hooksize_desc == "14/0" & month == 10 ~ "dog comp",
    year == 2022 & hooksize_desc == "13/0" & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" ~ "hbll comp",
    year == 2022 & hooksize_desc == "14/0" & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" ~ "dog comp",
    year == 2004 & hooksize_desc == "14/0" & survey_abbrev == "OTHER" ~ "dog comp",
    year == 2004 & hooksize_desc == "12/0" & survey_abbrev == "OTHER" ~ "dog-jhook"
  ))

final <- final |>
  mutate(survey_lumped = case_when(
    hooksize_desc == "13/0" ~ "hbll",
    hooksize_desc == "14/0" ~ "dog",
    hooksize_desc == "12/0" ~ "dog-jhook"))

# add season based on week of survey and definitions above
final <- final |>
  mutate(season = ifelse(week %in% spring, "2", ifelse(week %in% summer, "3", ifelse(week %in% fall, "4", "1"))))

range(final$soak)
filter(final, is.na(soak) == TRUE) |> tally()

#final <- final |> mutate(cpue = catch_count / (lglsp_hook_count * soak))
# final <- filter(final, usability_code != 0) #usability_code filters out the comparison sets
final <- filter(final, lglsp_hook_count != 0)

final$offset_hksoak <- log(final$lglsp_hook_count * final$soak) # nas created, thats ok
final$offset <- log(final$lglsp_hook_count)
final$log_botdepth <- log(final$depth_m)
saveRDS(final, "data-generated/dogfish_sets_cleaned_getall.rds")

