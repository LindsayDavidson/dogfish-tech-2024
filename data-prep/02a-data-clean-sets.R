# library -----------------------------------------------------------------
library(sf)
library(ggplot2)
library(tidyverse)
library(sp)

# load data ---------------------------------------------------------------

sets <- readRDS("data-raw/dogfish_sets_getall.rds") # get all function


# # QA/QC location names - didn't use with the get_all function as no locations names are in the db -------------------------------------------------------------
#
# unique(sets$grouping_spatial_id) #NAs
#
# sets <- sets |>
#   mutate(grouping_spatial_id =
#     case_when(str_detect(fe_fishing_ground_comment, "French") ~ "FC",
#               str_detect(fe_fishing_ground_comment, "Hornby") ~ "HI",
#     str_detect(fe_fishing_ground_comment, "Galiano") ~ "GI",
#     str_detect(fe_fishing_ground_comment, "Gabriola") ~ "EI",
#     str_detect(fe_fishing_ground_comment, "Entrance") ~ "EI",
#     str_detect(fe_fishing_ground_comment, "Epso") ~ "EP",
#     str_detect(fe_fishing_ground_comment, "Espo") ~ "EP", #sp mistake in df
#     str_detect(fe_fishing_ground_comment, "Salamanca") ~ "AP",
#     str_detect(fe_fishing_ground_comment, "Active") ~ "AP",
#     str_detect(fe_fishing_ground_comment, "Porlier") ~ "PP",
#     str_detect(fe_fishing_ground_comment, "Polier") ~ "PP", #sp mistake in df
#     str_detect(fe_fishing_ground_comment, "Sturgeon") ~ "SB",
#     str_detect(fe_fishing_ground_comment, "White") ~ "HB",
#     str_detect(fe_fishing_ground_comment, "Halibut") ~ "HB",
#     str_detect(fe_fishing_ground_comment, "Ajax") ~ "AE",
#     str_detect(fe_fishing_ground_comment, "Lazo") ~ "CL",
#     str_detect(fe_fishing_ground_comment, "Mudge") ~ "CM",
#     str_detect(fe_fishing_ground_comment, "Oyster") ~ "OR",
#     str_detect(fe_fishing_ground_comment, "Sinclair") ~ "SB",
#     str_detect(fe_fishing_ground_comment, "Grant") ~ "GR",
#     str_detect(fe_fishing_ground_comment, "Stillwater") ~ "SB", #I overlaid a map and these points to derive this, see below
#     str_detect(fe_fishing_ground_comment, "North East Pont") ~ "SB", #see overlay
#     str_detect(fe_fishing_ground_comment, "Mid Straits") ~ "CL", #see overlay
#     is.na(fe_fishing_ground_comment) ~ NA
#   ))
#
# #some points have the name galiano but should be active pass based on location
# #based on overlay below I am changing them here
# sets <- sets |>
#   mutate(grouping_spatial_id = ifelse(fishing_event_id %in% c(3369551,3369551,3369552,3369552,336955, 3369553), "AP", grouping_spatial_id))
#
# ggplot() +
#   geom_point(data = sets, aes(longitude, latitude, colour = as.factor(grouping_spatial_id)))
# #that NA is fine, its outside of any dogfish survey site
#
#
# # polygon and set point overlay - didn't use -------------------------------------------
#
#
# #leaving this here to show how I checked the spatial location.
# # # overlay data locations with polygons
# # sites <- st_read("data-raw", "dogfish_polygons_noproj2")
# # plot(st_geometry(sites), col = "red")
# # site_name <- unique(sites$site_name)
# # df <- data.frame(cbind(site_name, site_gis = c(
# #   "Ajax Exeter", "Active Pass", "Grants Reef", "Halibut Bank", "Sturgeon Bank",
# #   "Oyster River", "Epsom Point", "Sinclair Bank", "Porlier Pass", "Cape Mudge", "French Creek",
# #   "Cape Lazo", "Entrance Island", "Hornby Island"
# # ), site_shortname = c(
# #   "AE", "AP", "GR", "HB", "SB", "OR", "EP", "SB", "PP", "CM", "FC",
# #   "CL", "EI", "HI"
# # )))
# #
# # p1 <- ggplot(sites) +
# #   geom_sf(aes(colour = site_name), fill = NA)
# # p2 <- p1 + geom_point(data = sets, aes(longitude, latitude, colour = as.factor(grouping_spatial_id)))
# # p2
# #
# # sites <- left_join(sites, df)
# # finalsp <- sets
# # coordinates(finalsp) <- c("longitude", "latitude")
# # proj4string(finalsp) <- CRS("+proj=longlat + datum=WGS84")
# # finalsp <- st_as_sf(finalsp)
# # finalsp2 <- finalsp %>%
# #   mutate(
# #     latitude = unlist(purrr::map(finalsp$geometry, 2)),
# #     longitude = unlist(purrr::map(finalsp$geometry, 1))
# #   )
# # ptsint <- st_join(sites, finalsp2) # lose the points that dont intersect
# # ptsint
# #
# # #based on this,
# # #if a point has a name now but was NA before keep new name
# # #if point is now NA but has name before keep old name
# # #if point name doesn't match old point name, keep new name
# # test <- ptsint |> filter(is.na(grouping_spatial_id) == TRUE) #these are dropped
# # test <- ptsint |> filter(is.na(site_gis) == TRUE) #these are dropped
# # test <- ptsint |> filter(site_shortname != grouping_spatial_id) #these are dropped
#

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
    month = lubridate::month(retrieve),
    retrievehr = lubridate::hour(time_begin_retrieval),
    retrievemin = lubridate::minute(time_begin_retrieval),
    dmy = lubridate::ymd(retrieve),
    julian = lubridate::yday(dmy)
  ) |>
  mutate(
    hr_diff = (retrievehr - deployhr) * 60,
    min_diff = abs(retrievemin - deploymin),
    soak = (hr_diff + min_diff) / 60
  )

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

final <- final |>
  mutate(survey_timing = case_when(
    survey_abbrev == "HBLL INS S" ~ "summer",
    survey_abbrev == "HBLL INS N" ~ "summer",
    year %in% c(1986, 1989) ~ "fall",
    year %in% c(2005, 2008, 2011, 2014) & survey_abbrev == "DOG" ~ "fall",
    year == 2019 & survey_abbrev == "DOG" ~ "fall",
    year == 2019 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" & hooksize_desc == "13/0" ~ "summer",
    year == 2019 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" & hooksize_desc == "14/0" ~ "summer",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" & hooksize_desc == "12/0" ~ "fall",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" & hooksize_desc == "14/0" & month == 9 & day >= 27 ~ "fall",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" & hooksize_desc == "13/0" & month == 9 & day >= 27 ~ "fall",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" & hooksize_desc == "13/0" & month == 9 & day < 27 ~ "summer",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" & hooksize_desc == "14/0" & month == 9 & day < 27 ~ "summer",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" & hooksize_desc == "13/0" & month == 8 ~ "summer",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" & hooksize_desc == "14/0" & month == 8 ~ "summer",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" & hooksize_desc == "13/0" & month == 10 ~ "fall",
    year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" & hooksize_desc == "14/0" & month == 10 ~ "fall",
    year == 2022 & hooksize_desc == "13/0" & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" ~ "summer",
    year == 2022 & hooksize_desc == "14/0" & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" ~ "summer",
    year == 2004 & hooksize_desc == "14/0" & survey_abbrev == "OTHER" ~ "fall",
    year == 2004 & hooksize_desc == "12/0" & survey_abbrev == "OTHER" ~ "fall"
  ))

# so I can put the different surveys into a model and account for julian date after (the seasonal component of the comparison work)
final <- final |>
  mutate(survey_lumped = case_when(
    survey_sep == "HBLL INS S" ~ "hbll",
    survey_sep == "HBLL INS N" ~ "hbll",
    survey_sep == "dog-jhook" ~ "dog-jhook",
    survey_sep == "dog" ~ "dog",
    survey_sep == "hbll comp" ~ "hbll",
    survey_sep == "dog comp" ~ "dog",
    survey_sep == "hbll" ~ "hbll"
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
