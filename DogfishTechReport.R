# pull dogfish data
# all surveys
# qa/qc
# there is a branch on PBS assess that has this code.
# https://github.com/pbs-assess/Dogfish-survey/blob/main/Dogfish_data_pull.R
# how do I integrate that?

# Code for creating one database of all Dogfish surveys including comparisons, j hooks, and dogfish surveys
# Note
# SURVEY_SERIES_ID == 48) #2004, 2019 survey since it was 2 different sets with different hooks
# SURVEY_SERIES_ID == 93) # 2005 onwards dogfish survey
# SURVEY_SERIES_ID == 76) # 1986 onwards dogfish survey DROP THIS ONE
# SURVEY_SERIES_ID == 92) # 1986, 1989 survey

# note yelloweye rockfish were not sampled in earlier years. 1986/1989 maybe not 2004?
# note 2004 comparison work had two gear types per set
# note 2019 comparison work dropped separate lines per gear type
# note 2022 comparison work has two gear types per set

# pull from csas down -----------------------------------------------------
# https://github.com/pbs-assess/csasdown
# install.packages("remotes")
# remotes::install_github("pbs-assess/csasdown", dependencies = TRUE)
# setwd("C:/Dogfish_GearComp_TechReport_2024/DogfishGearComp/Report")
# csasdown::draft("techreport")


# library -----------------------------------------------------------------
library(tidyverse)
library(sf)
library(ggplot2)
library(here)
library(sp)





# Pull dogfish survey samples and sets --------------------------------------------------------------
# this is the code from pbs-assess
info <- gfdata::run_sql("GFBioSQL", "SELECT
S.SURVEY_SERIES_ID,
SURVEY_SERIES_DESC, FE.FE_MISC_COMMENT, FE.FE_FISHING_GROUND_COMMENT,
S.SURVEY_ID, SURVEY_DESC, FE.FE_MAJOR_LEVEL_ID, SK.FE_SUB_LEVEL_ID,
SK.HOOK_DESC, SK.HOOKSIZE_DESC,
YEAR(TR.TRIP_START_DATE) AS YEAR,
FE.FISHING_EVENT_ID,
FE.FE_PARENT_EVENT_ID,
LGLSP_HOOK_COUNT,
FE_START_LATTITUDE_DEGREE + FE_START_LATTITUDE_MINUTE / 60 AS LATITUDE,
-(FE_START_LONGITUDE_DEGREE + FE_START_LONGITUDE_MINUTE / 60) AS LONGITUDE,
FE_END_DEPLOYMENT_TIME, FE_BEGIN_RETRIEVAL_TIME, FE.GROUPING_CODE, GROUPING_DESC, GROUPING_SPATIAL_ID, GROUPING_DEPTH_ID,
TR.TRIP_START_DATE,
TR.TRIP_END_DATE,
FE_BEGINNING_BOTTOM_DEPTH AS DEPTH_M,
FE.TRIP_ID
FROM FISHING_EVENT FE
LEFT JOIN (
  SELECT TRIP_ID, FE.FISHING_EVENT_ID, LLSP.LGLSP_HOOK_COUNT, FE_MAJOR_LEVEL_ID, FE_SUB_LEVEL_ID, H.HOOK_DESC, HSZ.HOOKSIZE_DESC
  FROM FISHING_EVENT FE
  INNER JOIN LONGLINE_SPECS LLSP ON LLSP.FISHING_EVENT_ID = FE.FISHING_EVENT_ID
  LEFT JOIN HOOK H ON H.HOOK_CODE = LLSP.HOOK_CODE
  LEFT JOIN HOOKSIZE HSZ ON HSZ.HOOKSIZE_CODE = LLSP.HOOKSIZE_CODE
) SK ON SK.TRIP_ID = FE.TRIP_ID AND SK.FE_MAJOR_LEVEL_ID = FE.FE_MAJOR_LEVEL_ID
INNER JOIN TRIP_SURVEY TS ON FE.TRIP_ID = TS.TRIP_ID
INNER JOIN TRIP TR ON FE.TRIP_ID = TR.TRIP_ID
INNER JOIN SURVEY S ON S.SURVEY_ID = TS.SURVEY_ID
INNER JOIN SURVEY_SERIES SS ON SS.SURVEY_SERIES_ID = S.SURVEY_SERIES_ID
LEFT JOIN GROUPING G ON G.GROUPING_CODE = FE.GROUPING_CODE
WHERE S.SURVEY_SERIES_ID IN (48, 76, 92, 93)
AND FE.FE_MAJOR_LEVEL_ID < 1000 AND FE_PARENT_EVENT_ID IS NULL
ORDER BY YEAR, TRIP_ID, FE_MAJOR_LEVEL_ID,  FE_SUB_LEVEL_ID")

# what is this for?
# AND FE.FE_MAJOR_LEVEL_ID < 1000 AND FE_PARENT_EVENT_ID IS NULL

dsurvey_bio <- gfdata::run_sql("GFBioSQL", "SELECT
A.ACTIVITY_DESC,
FE_SUB_LEVEL_ID,
SS.SURVEY_SERIES_ID,
FE_PARENT_EVENT_ID,
YEAR(B21.TRIP_START_DATE) AS YEAR,
B21.TRIP_COMMENT,
FISHING_EVENT_ID,
B21.TRIP_ID,
B21.FE_MAJOR_LEVEL_ID,
B21.SPECIES_CODE,
S.SPECIES_SCIENCE_NAME,
S.SPECIES_COMMON_NAME,
B22.SPECIMEN_ID,
B22.SPECIMEN_SEX_CODE,
B22.Total_Length,
B22.Round_Weight
FROM GFBioSQL.dbo.B21_Samples B21
INNER JOIN GFBioSQL.dbo.B22_Specimens B22 ON B22.SAMPLE_ID = B21.SAMPLE_ID
INNER JOIN GFBioSQL.dbo.TRIP_ACTIVITY TA ON TA.TRIP_ID = B21.TRIP_ID
INNER JOIN TRIP_SURVEY TS ON B21.TRIP_ID = TS.TRIP_ID
INNER JOIN TRIP TR ON TS.TRIP_ID = TR.TRIP_ID
INNER JOIN SURVEY SR ON TS.SURVEY_ID = SR.SURVEY_ID
INNER JOIN SURVEY_SERIES SS ON SR.SURVEY_SERIES_ID = SS.SURVEY_SERIES_ID
INNER JOIN GFBioSQL.dbo.ACTIVITY A ON A.ACTIVITY_CODE = TA.ACTIVITY_CODE
INNER JOIN GFBioSQL.dbo.SPECIES S ON S.SPECIES_CODE = B21.SPECIES_CODE
WHERE SR.SURVEY_SERIES_ID IN (48, 76, 92, 93)
ORDER BY B21.TRIP_ID, B21.FE_MAJOR_LEVEL_ID, B22.SPECIMEN_ID")

# all 2004, 2022 comparison work should have a parent_event_id
dsurvey_bio |>
  filter(YEAR %in% c(2004, 2022)) |>
  group_by(FISHING_EVENT_ID, YEAR) |>
  distinct(FISHING_EVENT_ID, .keep_all = TRUE) |>
  reframe(sum = is.na(FE_PARENT_EVENT_ID)) |>
  filter(sum == TRUE)

# all other years do not have a parent event id
dsurvey_bio |>
  filter(!(YEAR %in% c(2004, 2022))) |>
  group_by(FISHING_EVENT_ID, YEAR) |>
  distinct(FISHING_EVENT_ID, .keep_all = TRUE) |>
  reframe(sum = is.na(FE_PARENT_EVENT_ID)) |>
  filter(sum == FALSE)

# note yelloweye rockfish not sampled, therefore not entries.
x <- filter(dsurvey_bio, YEAR == 1986)
unique(x$SPECIES_COMMON_NAME)

# this has the catch count per species
catchcount <- gfdata::run_sql("GFBioSQL", "SELECT
FEC.FISHING_EVENT_ID,
FE.FE_PARENT_EVENT_ID,
FE.FE_SUB_LEVEL_ID,
C.SPECIES_CODE,
SP.SPECIES_COMMON_NAME,
SP.SPECIES_SCIENCE_NAME,
SUM(CATCH_COUNT) CATCH_COUNT
FROM FISHING_EVENT_CATCH FEC
INNER JOIN FISHING_EVENT FE ON FE.FISHING_EVENT_ID = FEC.FISHING_EVENT_ID
INNER JOIN CATCH C ON C.CATCH_ID = FEC.CATCH_ID
INNER JOIN TRIP_SURVEY TS ON TS.TRIP_ID = FEC.TRIP_ID
INNER JOIN SURVEY S ON S.SURVEY_ID = TS.SURVEY_ID
INNER JOIN GFBioSQL.dbo.SPECIES SP ON SP.SPECIES_CODE = C.SPECIES_CODE
WHERE SURVEY_SERIES_ID IN (48, 76, 92, 93)
GROUP BY FEC.TRIP_ID,
FEC.FISHING_EVENT_ID,
FE.FE_PARENT_EVENT_ID,
FE.FE_SUB_LEVEL_ID,
C.SPECIES_CODE,
SP.SPECIES_COMMON_NAME,
SP.SPECIES_SCIENCE_NAME
ORDER BY FEC.FISHING_EVENT_ID")


saveRDS(dsurvey_bio, "data/raw/dogfish_samples.rds")
saveRDS(catchcount, "data/raw/dogfish_counts.rds")
saveRDS(info, "data/raw/dogfish_sets.rds")


# QA/QC location names -------------------------------------------------------------
samples <- readRDS("data/raw/dogfish_samples.rds")
sets <- readRDS("data/raw/dogfish_sets.rds")

names(samples) <- tolower(names(samples))
names(sets) <- tolower(names(sets))

sets |>
  select(grouping_spatial_id) |>
  unique()

unique(sets$grouping_spatial_id)

# FIX LOCATION NAMES
sets |> filter(is.na(grouping_spatial_id)) # lots of NAs for grouping_spatial_id

# overlay data locations with polygons
sites <- st_read("data/raw", "dogfish_polygons_noproj2")
plot(st_geometry(sites), col = "red")
site_name <- unique(sites$site_name)
df <- data.frame(cbind(site_name, site_gis = c(
  "Ajax Exeter", "Active Pass", "Grants Reef", "Halibut Bank", "Sturgeon Bank",
  "Oyster River", "Epsom Point", "Sinclair Bank", "Porlier Pass", "Cape Mudge", "French Creek",
  "Cape Lazo", "Entrance Island", "Hornby Island"
), site_shortname = c(
  "AE", "AP", "GR", "HB", "ST", "OR", "EP", "SB", "PP", "CM", "FC",
  "CL", "EI", "HI"
)))

sites <- left_join(sites, df)
#sites <- sites |> select(site_gis)
# convert center utms to lat and longs
finalsp <- sets
coordinates(finalsp) <- c("longitude", "latitude")
proj4string(finalsp) <- CRS("+proj=longlat + datum=WGS84")
finalsp <- st_as_sf(finalsp)
finalsp2 <- finalsp %>%
  mutate(
    latitude = unlist(purrr::map(finalsp$geometry, 2)),
    longitude = unlist(purrr::map(finalsp$geometry, 1))
  )
ptsint <- st_join(sites, finalsp2) # lose the points that dont intersect
ptsint

#check no NAs in the site_gis
filter(ptsint, is.na(grouping_spatial_id) == TRUE) |>
  dplyr::select(site_gis) |>
  distinct()
#put in the missing grouping spatial ids
ptsint <- ptsint |>
  mutate(grouping_spatial_id = ifelse(is.na(grouping_spatial_id) == TRUE,
                                      site_shortname,
                                      grouping_spatial_id)) |>
  dplyr::select(-site_name)
#st_geometry(sites) <- NULL
# some points have NA in the grouping_spatial_id so fix that here
st_geometry(ptsint) <- NULL


# intersect of sets and dogfish polygons, red points fall outside of the polygons
p1 <- ggplot(sites) +
  geom_sf(aes(fill = site_gis))
p2 <- p1 + geom_point(data = sets, aes(longitude, latitude), colour = "red")
p3 <- p2 + geom_point(data = ptsint, aes(longitude, latitude))
p3

# a couple points fall outside of the polygons
missing <- anti_join(dplyr::select(sets, year, fishing_event_id, longitude, latitude),
                     dplyr::select(ptsint, year, fishing_event_id))
missing |>
  distinct(.keep_all = TRUE)
setsmissing <- filter(sets, sets$fishing_event_id %in% missing$fishing_event_id)
p3 + geom_point(data = setsmissing, aes(longitude, latitude, col = grouping_spatial_id))

# check the missing points
df <- data.frame(
  fishing_event_id = c(
    3369466, 3369471, 3369508, 4356186, 3369598, 5490376,
    5490373, 5490374, 5490375
  ),
  grouping_spatial_id = c("EI", NA, "CL", "CL", "OR", NA, "AP", "AP", "AP")
)
ptsint2 <- sets |>
  dplyr::select(-grouping_spatial_id) |>
  filter(fishing_event_id %in% df$fishing_event_id) |>
  left_join(df) |>
  bind_rows(dplyr::select(ptsint, -site_gis))

saveRDS(ptsint2, "data/generated/sets_cleaned.rds")

# check points are close to
# could do this with a buffer around points and see what intersects
p1 <- ggplot(sites) +
  geom_sf(aes(fill = site_gis))
p2 <- p1 + geom_sf(data = filter(sites, site_gis == "Entrance Island"), fill = "red")
p2 + geom_point(
  data = filter(missing, fishing_event_id == 3369466),
  aes(longitude, latitude), colour = "red"
)

p1 <- ggplot(sites) +
  geom_sf(aes(fill = site_gis))
p2 <- p1 + geom_sf(data = filter(sites, site_gis == "Porlier Pass"), fill = "red")
p2 + geom_point(
  data = filter(missing, fishing_event_id == 3369471),
  aes(longitude, latitude), colour = "red"
) # nope too far

p1 <- ggplot(sites) +
  geom_sf(aes(fill = site_gis))
p2 <- p1 + geom_sf(data = filter(sites, site_gis == "Cape Lazo"), fill = "red")
p2 + geom_point(
  data = filter(missing, fishing_event_id == c(3369508, 4356186)),
  aes(longitude, latitude), colour = "red"
)

p1 <- ggplot(sites) +
  geom_sf(aes(fill = site_gis))
p2 <- p1 + geom_sf(data = filter(sites, site_gis == "Oyster River"), fill = "red")
p2 + geom_point(
  data = filter(missing, fishing_event_id == 3369598),
  aes(longitude, latitude), colour = "red"
)

p1 <- ggplot(sites) +
  geom_sf(aes(fill = site_gis))
p2 <- p1 + geom_sf(data = filter(sites, site_gis == "Active Pass"), fill = "red")
p2 + geom_point(
  data = filter(missing, fishing_event_id == c(5490373, 5490374, 5490375)),
  aes(longitude, latitude), colour = "red"
)

p1 <- ggplot(sites) +
  geom_sf(aes(fill = site_gis))
# p2 <- p1 + geom_sf(data = filter(sites, site_gis == "Active Pass"), fill = "red")
p1 + geom_point(
  data = filter(missing, fishing_event_id == c(5490376)),
  aes(longitude, latitude), colour = "red"
)


ptsint2 |>
  dplyr::select(grouping_spatial_id, site_gis, fe_fishing_ground_comment) |>
  distinct()
ggplot(ptsint2, aes(longitude, latitude, colour = grouping_spatial_id, group = fe_fishing_ground_comment)) +
  geom_point()

# QA/QC dates and depth--------------------------------
sets <- readRDS("data/generated/sets_cleaned.rds") |>
  filter(is.na(grouping_spatial_id) == FALSE)

# check depths
unique(sets$grouping_desc) # NAs and a 'SOG Dogfish Site'

# fix
sets <- sets |>
  mutate(grouping_desc = ifelse(depth_m <= 55, "SoG Dogfish 0 - 55 m",
    ifelse(depth_m > 55 & depth_m <= 110, "SoG Dogfish 56 - 110 m",
      ifelse(depth_m > 110 & depth_m <= 165, "SoG Dogfish 111 - 165 m",
        ifelse(depth_m > 166 & depth_m <= 220, "SoG Dogfish 166 - 220 m",
          ifelse(depth_m > 220, "SoG Dogfish > 200 m",
            NA
          )
        )
      )
    )
  ))

sets <- sets |>
  mutate(grouping_depth_id = ifelse(grouping_desc == "SoG Dogfish 0 - 55 m", 1,
    ifelse(grouping_desc == "SoG Dogfish 56 - 110 m", 2,
      ifelse(grouping_desc == "SoG Dogfish 111 - 165 m", 3,
        ifelse(grouping_desc == "SoG Dogfish 166 - 220 m", 4,
          ifelse(grouping_desc == "SoG Dogfish > 200 m", 5,
            NA
          )
        )
      )
    )
  ))

# check
sets |>
  filter(grouping_desc == "SoG Dogfish Site") # none, fixed now

# still NAs - WHY
sets |>
  filter(is.na(grouping_desc) == TRUE) # comment says missing depth in
sets <- filter(sets, is.na(grouping_desc) != TRUE)

sets |>
  group_by(year, grouping_spatial_id) |>
  distinct(grouping_desc) |>
  tally() |>
  print(n = 50)

# how many sites fished
sets |>
  group_by(year) |>
  distinct(grouping_spatial_id) |>
  tally()

# Calculate and QA/QC soak time -----------------------------------------------------
glimpse(sets$fe_end_deployment_time)
d <- sets |>
  mutate(
    deployhr = lubridate::hour(fe_end_deployment_time),
    deploymin = lubridate::minute(fe_end_deployment_time),
    retrive = as.Date(fe_begin_retrieval_time, format = "%Y-%m-%d h:m:s"),
    retrivehr = lubridate::hour(fe_begin_retrieval_time),
    retrievemin = lubridate::minute(fe_begin_retrieval_time)
  ) |>
  mutate(
    hr_diff = (retrivehr - deployhr) * 60,
    min_diff = abs(retrievemin - deploymin),
    soak = hr_diff + min_diff
  )

# some soaks are NA - fix this!
d |>
  filter(is.na(soak) == TRUE)

d |>
  filter(is.na(soak) == TRUE) |>
  distinct(fishing_event_id, .keep_all = TRUE) |>
  tally() # 64 fishing events are missing soak times as the deployment time wasnt recorded
# most are in 2004 when fishing times were between 1.5 - 3 hours.

saveRDS(d, "data/generated/sets_cleaned2.rds")


# MERGE SETS AND CATCH COUNTS ---------------------------------------------
sets <- readRDS("data/generated/sets_cleaned2.rds")
count <- readRDS("data/raw/dogfish_counts.rds")

names(sets) <- tolower(names(sets))
names(samples) <- tolower(names(samples))
names(count) <- tolower(names(count))

regsurveys <- sets |>
  filter(survey_series_id %in% c(93, 92)) |>
  left_join(count)
unique(regsurveys$year)

compsurveys <- sets |>
  filter(survey_series_id == 48 & year %in% c(2004, 2022)) |>
  left_join(count, by = c("fishing_event_id" = "fe_parent_event_id", "fe_sub_level_id" = "fe_sub_level_id")) |>
  select(-fishing_event_id.y)
unique(compsurveys$year)

compsurveys2019 <- sets |>
  filter(survey_series_id == 48 & year == 2019) |>
  left_join(count)
unique(compsurveys2019$year)

final <- rbind(regsurveys, compsurveys, compsurveys2019)
unique(final$year)

ggplot(final, aes(species_code, catch_count)) +
  geom_point() +
  facet_wrap(~year)
ggplot(final, aes(species_code, lglsp_hook_count)) +
  geom_point() +
  facet_wrap(~year)

saveRDS(final, "data/generated/dogfishs_allsets_allspecies_counts.rds")


# summary tables and figures for sets and counts --------------------------
final <- readRDS("data/generated/dogfishs_allsets_allspecies_counts.rds")

# how many rockfish captured at depth two across sites?
final |>
  filter(species_code == "442") |>
  filter(is.na(grouping_depth_id) != TRUE) |>
  group_by(grouping_spatial_id, grouping_depth_id) |>
  reframe(yelloweye_catch_count = sum(catch_count), yelloweye_mean_catchcount = mean(catch_count)) |>
  filter(grouping_depth_id == 2)

final |>
  filter(species_code == "442") |>
  filter(is.na(grouping_depth_id) != TRUE) |>
  group_by(grouping_spatial_id, grouping_depth_id, year) |>
  reframe(yelloweye_catch_count = sum(catch_count)) |>
  filter(grouping_depth_id == 2)

final |>
  filter(species_code == "442") |>
  filter(is.na(grouping_depth_id) != TRUE) |>
  group_by(grouping_spatial_id, grouping_depth_id) |>
  reframe(sum = sum(catch_count)) |>
  ggplot() +
  geom_point(aes(grouping_depth_id, sum, group = grouping_spatial_id, colour = grouping_spatial_id)) +
  geom_line(aes(grouping_depth_id, sum, group = grouping_spatial_id, colour = grouping_spatial_id)) +
  facet_wrap(~grouping_spatial_id)

# 2019 hook comparison catch and effort results
final |>
  filter(species_code == "044") |>
  filter(year == 2019 & survey_series_id == 48) |>
  filter(is.na(grouping_depth_id) != TRUE) |>
  group_by(hooksize_desc, grouping_depth_id) |>
  reframe(
    sum = sum(catch_count),
    sumeffort = sum(lglsp_hook_count * soak),
    sumcpue = sum / sumeffort
  ) |>
  drop_na(hooksize_desc) |>
  ggplot() +
  geom_point(aes(grouping_depth_id, sum,
    group = as.factor(hooksize_desc),
    colour = as.factor(hooksize_desc)
  ), size = 2) +
  geom_line(aes(grouping_depth_id, sum,
    group = as.factor(hooksize_desc),
    colour = as.factor(hooksize_desc)
  ), size = 1)

final |>
  filter(species_code == "044") |>
  filter(year == 2019 & survey_series_id == 48) |>
  filter(is.na(grouping_depth_id) != TRUE) |>
  group_by(hooksize_desc, grouping_depth_id, fishing_event_id) |>
  reframe(cpue = sum(catch_count / sum(lglsp_hook_count * soak))) |>
  group_by(hooksize_desc, grouping_depth_id) |>
  reframe(sumcpue = sum(cpue)) |>
  drop_na(hooksize_desc) |>
  ggplot() +
  geom_point(aes(grouping_depth_id, sumcpue,
    group = as.factor(hooksize_desc),
    colour = as.factor(hooksize_desc)
  ), size = 2) +
  geom_line(aes(grouping_depth_id, sumcpue,
    group = as.factor(hooksize_desc),
    colour = as.factor(hooksize_desc)
  ), size = 1)

# MERGE SETS AND SAMPLES ---------------------------------------------------------
#sets <- readRDS("data/generated/dogfishs_allsets_allspecies_clean.rds")
sets <- readRDS("data/generated/sets_cleaned2.rds")
samples <- readRDS("data/raw/dogfish_samples.rds")

names(sets) <- tolower(names(sets))
names(samples) <- tolower(names(samples))

regsurveys <- samples |>
  filter(survey_series_id %in% c(93, 92)) |>
  inner_join(sets)

compsurveys <- samples |>
  filter(survey_series_id == 48 & year != 2019) |>
  left_join(sets, by = c(
    "fe_parent_event_id" = "fishing_event_id",
    "fe_sub_level_id" = "fe_sub_level_id",
    "survey_series_id" = "survey_series_id",
    "year" = "year",
    "trip_id" = "trip_id",
    "fe_major_level_id" = "fe_major_level_id"
  )) |>
  select(-"fe_parent_event_id.y")

compsurveys2019 <- samples |>
  filter(survey_series_id == 48 & year == 2019) |>
  left_join(sets)

final <- rbind(regsurveys, compsurveys, compsurveys2019)
unique(final$grouping_desc)

saveRDS(final, "data/raw/dogfishs_allsets_allsamples.rds")



# SUMMARY FIGURES - SETS --------------------------------------------------
sets <- readRDS("data/generated/dogfishs_allsets_allspecies_counts.rds")

df <- filter(sets, species_code == "442")
df <- filter(sets, species_code == "044")

sets |>
  filter(species_code == "442") |>
  group_by(year, grouping_depth_id) |>
  summarize(count = sum(catch_count)) |>
  print(n = 35)

df <- df |> mutate(cpue = catch_count / (lglsp_hook_count * soak))
glimpse(df)
ggplot(df, aes(grouping_depth_id, catch_count, group = year, colour = year)) +
  geom_point() +
  geom_line() +
  facet_wrap(~grouping_spatial_id)

ggplot(df, aes(grouping_depth_id, catch_count, group = grouping_spatial_id, colour = grouping_spatial_id)) +
  geom_point() +
  geom_line() +
  facet_wrap(~year)

ggplot(df, aes(grouping_depth_id, catch_count, group = hooksize_desc, colour = hooksize_desc)) +
  geom_jitter() +
  facet_wrap(~year, scales = "free_y")

df |>
  group_by(grouping_depth_id, year, hooksize_desc) |>
  drop_na(soak) |>
  reframe(sum = sum(catch_count) / sum(lglsp_hook_count * soak)) |>
  ggplot(aes(grouping_depth_id, sum, group = hooksize_desc, colour = hooksize_desc)) +
  geom_point() +
  geom_line() +
  facet_wrap(~year, scales = "free_y")

df |>
  group_by(grouping_depth_id, year, hooksize_desc) |>
  reframe(sum = sum(catch_count)) |>
  ggplot(aes(grouping_depth_id, sum, group = hooksize_desc, colour = hooksize_desc)) +
  geom_point() +
  geom_line() +
  facet_wrap(~year, scales = "free_y")

# SUMMARY FIGURES - samples ---------------------------------------------------------
d <- readRDS("data/raw/dogfishs_allsets_allsamples.rds")

d |>
  ggplot() +
  geom_point(aes(year, soak)) +
  theme_classic()

d |>
  filter(species_code == "027") |>
  filter(specimen_sex_code %in% c(1, 2)) |>
  ggplot() +
  geom_point(aes((grouping_depth_id), total_length,
    colour = as.factor(hooksize_desc)
  )) +
  geom_line(aes((grouping_depth_id), total_length,
    colour = as.factor(hooksize_desc)
  )) +
  # geom_boxplot(position = position_dodge(1)) +
  facet_wrap(~ specimen_sex_code + year, nrow = 2) +
  theme_classic()

d |>
  filter(species_code == "044") |>
  filter(specimen_sex_code %in% c(1, 2)) |>
  ggplot() +
  geom_boxplot(aes((grouping_depth_id), total_length,
    colour = as.factor(hooksize_desc)
  )) +
  # geom_boxplot(position = position_dodge(1)) +
  facet_wrap(~ specimen_sex_code + year, nrow = 2) +
  theme_classic()

d |>
  filter(species_code == "044") |>
  filter(specimen_sex_code %in% c(1, 2)) |>
  ggplot() +
  geom_boxplot(aes(as.factor(year), total_length,
    colour = as.factor(hooksize_desc)
  )) +
  # geom_boxplot(position = position_dodge(1)) +
  facet_wrap(~ specimen_sex_code + grouping_depth_id, nrow = 2) +
  theme_classic()
