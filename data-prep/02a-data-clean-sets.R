# library -----------------------------------------------------------------
library(sf)
library(ggplot2)
library(tidyverse)
library(sp)

spring <- seq(12, 25, 1)
summer <- seq(26, 38, 1)
winter <- c(seq(1, 11, 1), 52)
fall <- seq(39, 51, 1)

# load data ---------------------------------------------------------------

sets <- readRDS("data-raw/dogfish_sets_getall.rds") # get all function



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

# still NAs - WHY
sets |>
  filter(is.na(grouping_desc) == TRUE) # all depths are in there


# QA/QC soak time  -----------------------------------------------------

d <- sets |>
  mutate(
    deployhr = lubridate::hour(time_end_deployment),
    deploymin = lubridate::minute(time_end_deployment),
    retrieve = as.Date(time_begin_retrieval, format = "%Y-%m-%d h:m:s"),
    deployed = as.Date(time_deployed, format = "%Y-%m-%d h:m:s"),
    month = lubridate::month(retrieve),
    retrievehr = lubridate::hour(retrieve),
    retrievemin = lubridate::minute(retrieve),
    dmy = lubridate::ymd(retrieve),
    julian = lubridate::yday(retrieve),
    week = lubridate::week(retrieve)
  ) |>
  mutate(
    hr_diff = (retrievehr - deployhr) * 60,
    min_diff = abs(retrievemin - deploymin),
    soak = (hr_diff + min_diff) / 60
  )

d |> filter(is.na(week)== TRUE) |> tally() #some dates are NAs
d <- d |>
  mutate(week = ifelse(is.na(week) == TRUE, lubridate::week(deployed), week))
d |> filter(is.na(week)== TRUE) |> tally() #fixed


# some soaks are NA - fix this!
d |>
  filter(is.na(soak) == TRUE) # mostly 2004

d |>
  filter(is.na(soak) == TRUE) |>
  distinct(fishing_event_id, .keep_all = TRUE) |>
  tally() # 66 fishing events are missing soak times as the deployment time wasnt recorded
# most are in 2004 when fishing times were between 1.5 - 3 hours.

d |>
  filter(is.na(soak) == TRUE) |>
  distinct(fishing_event_id, .keep_all = TRUE) |>
  group_by(year) |>
  tally() # lots of 2005s missing too soak time should have been consistently 2 hours at this time, the time_end_deployment was not recorded in 2005

# Add grouping code for survey ---------------------------------------------

# this is so I can look at trends through time, designed based, that don't include the comp work
final <- d |>
  mutate(survey_sep = case_when(
    survey_abbrev == "HBLL INS S" ~ "HBLL INS S",
    survey_abbrev == "HBLL INS N" ~ "HBLL INS N",
    year %in% c(1986, 1989) ~ "dog-jhook",
    year %in% c(2005, 2008, 2011, 2014) & survey_abbrev == "DOG" ~ "dog",
    year == 2019 & survey_abbrev == "DOG" ~ "dog",
    year == 2019 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" & hooksize_desc == "13/0" ~ "hbll comp",
    year == 2019 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" & hooksize_desc == "14/0" ~ "dog comp",
    # year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" & hooksize_desc == "14/0" ~ "dog",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" & hooksize_desc == "12/0" ~ "dog-jhook",
    # year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" & hooksize_desc == "13/0" ~ "hbll comp",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" & hooksize_desc == "14/0" & month == 9 & day >= 27 ~ "dog comp",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" & hooksize_desc == "13/0" & month == 9 & day >= 27 ~ "hbll comp",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" & hooksize_desc == "13/0" & month == 9 & day < 27 ~ "hbll comp",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" & hooksize_desc == "14/0" & month == 9 & day < 27 ~ "dog comp",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" & hooksize_desc == "13/0" & month == 8 ~ "hbll comp",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" & hooksize_desc == "14/0" & month == 8 ~ "dog comp",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" & hooksize_desc == "13/0" & month == 10 ~ "hbll comp",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" & hooksize_desc == "14/0" & month == 10 ~ "dog comp",
    year == 2022 & hooksize_desc == "13/0" & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" ~ "hbll comp",
    year == 2022 & hooksize_desc == "14/0" & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" ~ "dog comp",
    year == 2004 & hooksize_desc == "14/0" & survey_abbrev == "OTHER" ~ "dog comp",
    year == 2004 & hooksize_desc == "12/0" & survey_abbrev == "OTHER" ~ "dog-jhook"
  ))


#add season based on week of survey and definitions above
final <- final |>
  mutate(season = ifelse(week %in% spring, "2", ifelse(week %in% summer, "3", ifelse(week %in% fall, "4", "1"))))

# Put the different surveys into a model and account for julian date after (the seasonal component of the comparison work)
final <- final |>
  mutate(survey_lumped = case_when(
    survey_sep %in% c("HBLL INS S", "HBLL INS N", "hbll", "hbll comp") ~ "hbll",
    survey_sep == "dog-jhook" ~ "dog-jhook",
    survey_sep %in% c("dog", "dog comp") ~ "dog"
  ))


final <- final |> mutate(cpue = catch_count / (lglsp_hook_count * soak))
# final <- filter(final, usability_code != 0) #you lose the jhook is you do this
final <- filter(final, lglsp_hook_count != 0)
# final <- filter(final, soak != 0) #will lose all the 2004s do this later
final <- final |>
  mutate(soak = ifelse(year %in% c(2005) & survey_abbrev == "DOG", 2, soak)) # safe to assume these are ~2 hours
final$offset <- log(final$lglsp_hook_count * final$soak) # nas created, thats ok
final$log_botdepth <- log(final$depth_m)
saveRDS(final, "data-generated/dogfish_sets_cleaned_getall.rds")

