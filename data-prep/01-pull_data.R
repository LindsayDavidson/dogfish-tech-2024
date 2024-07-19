# pull dogfish data
# all surveys
# qa/qc
# there is a branch on PBS assess that has this code.
# https://github.com/pbs-assess/Dogfish-survey/blob/main/Dogfish_data_pull.R

# Code for creating one database of all Dogfish surveys including comparisons, j hooks, and dogfish surveys
# Note
# SURVEY_SERIES_ID == 48) #comp work 2004, 2019, 2022, 2023
# SURVEY_SERIES_ID == 93) #circle hook dog surveys 2005 onwards (2005, 2008, 2011, 2014, 2019)
# SURVEY_SERIES_ID == 76) #all jhook and circle hook dog surveys 1986 onwards does not include 2004 (1986, 1989, 2005, 2008, 2011, 2014, 2019)
# SURVEY_SERIES_ID == 92) #j hook dog surveys 1986, 1989 only

# yelloweye rockfish were not sampled in earlier years. 1986/1989 maybe not 2004?
# 2004 comparison work had two gear types per set
# 2019 comparison work dropped separate lines per gear type
# 2022 comparison work has two gear types per set
# 2023 comparison work has two gear types per set  and was completed during the HBLL and DOG survey

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
library(gfdata)



# pull inside DOG survey for maturities -----------------------------------


allsamps <- get_survey_samples("north pacific spiny dogfish")
unique(allsamps$survey_series_desc)
dogsamps <- filter(allsamps, survey_series_desc %in% c(
  "Hard Bottom Longline Inside North ",
  "Strait of Georgia Dogfish Longline", #this doesn't include comp work
  "Hard Bottom Longline Inside South "
))

saveRDS(dogsamps, "data-raw/samples-hbll-dog-samps.rds")

# pull inside samples data ------------------------------------------------

allsamps <- get_survey_samples("north pacific spiny dogfish")
unique(allsamps$survey_series_desc)
test <- filter(allsamps, survey_series_desc == "Strait of Georgia Dogfish Longline")
unique(test$year)

dogsamps <- filter(allsamps, survey_series_desc %in% c(
  "Hard Bottom Longline Inside North ",
  #"Strait of Georgia Dogfish Longline", #this doesn't include comp work
  "Hard Bottom Longline Inside South "
))

saveRDS(dogsamps, "data-raw/samples-hbll-dog.rds")

# pull HBLL inside data ---------------------------------------------------

if (!file.exists("data-raw/HBLLsamples.rds")) {
  hbllsamples <- get_survey_samples(species = "north pacific spiny dogfish", ssid = c(39,40))
  saveRDS(hbllsamples, "data-raw/HBLLsamples.rds")
}

if (!file.exists("data-raw/hbllsets.rds")) {
  hbllsets <- get_survey_sets(species = "north pacific spiny dogfish", ssid = c(39,40))
  saveRDS(hbllsets, "data-raw/hbllsets.rds")
}


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

#it would be helful to bring in hook desc and hook size here
#add these SK.HOOK_DESC, SK.HOOKSIZE_DESC,


dsurvey_bio <- gfdata::run_sql("GFBioSQL", "SELECT
A.ACTIVITY_DESC,
SURVEY_DESC,
FE_SUB_LEVEL_ID,
SS.SURVEY_SERIES_ID,
FE_PARENT_EVENT_ID,
YEAR(B21.TRIP_START_DATE) AS YEAR,
B21.TRIP_COMMENT,
FISHING_EVENT_ID,
TR.TRIP_START_DATE,
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

#add in maturity_code, maturity_name,
#maturity_convention_code, maturity_desc, trip_start_date, usability_code,
#sample_id

# dsurvey_bio <- gfdata::run_sql("GFBioSQL", "SELECT
# A.ACTIVITY_DESC,
# SURVEY_DESC,
# FE_SUB_LEVEL_ID,
# SS.SURVEY_SERIES_ID,
# FE_PARENT_EVENT_ID,
# MAT.MATURITY_DESC,
# USABILITY_CODE,  <- WHERE DOES THIS COME FROM
# SSAMP.SAMPLE_ID,
# YEAR(B21.TRIP_START_DATE) AS YEAR,
# B21.TRIP_COMMENT,
# FISHING_EVENT_ID,
# SM.MATURITY_CODE,
# TR.TRIP_START_DATE,
# B21.TRIP_ID,
# B21.FE_MAJOR_LEVEL_ID,
# B21.SPECIES_CODE,
# S.SPECIES_SCIENCE_NAME,
# S.SPECIES_COMMON_NAME,
# B22.SPECIMEN_ID,
# B22.SPECIMEN_SEX_CODE,
# B22.Total_Length,
# B22.Round_Weight,
# SSTOM.MATURITY_CODE,
# SSTOM.MATURITY_CONVENTION_CODE
# FROM GFBioSQL.dbo.B21_Samples B21
# INNER JOIN MATURITY MM MM.MATURITY_CODE = B21.MATURITY_CODE
# INNER JOIN MATURITY MM MM.MATURITY_CODE = SSTOM.MATURITY_CODE
# INNER JOIN SPECIMEN SM SM.SPECIMEN_ID = SSAMP.SPECIMEN_ID
# INNER JOIN SPECIMEN_STOMACH SSTOM.SPECIMEN_ID = B21.SPECIMEN_ID
# INNER JOIN GFBioSQL.dbo.B22_Specimens B22.SPECIMEN_ID = B21.SAMPLE_ID
# INNER JOIN SPECIMEN_SAMPLE SSAMP ON SSAMP.SPECIMEN_ID = B21.SAMPLE_ID
# INNER JOIN GFBioSQL.dbo.TRIP_ACTIVITY TA ON TA.TRIP_ID = B21.TRIP_ID
# INNER JOIN TRIP_SURVEY TS ON B21.TRIP_ID = TS.TRIP_ID
# INNER JOIN TRIP TR ON TS.TRIP_ID = TR.TRIP_ID
# INNER JOIN SURVEY SR ON TS.SURVEY_ID = SR.SURVEY_ID
# INNER JOIN SURVEY_SERIES SS ON SR.SURVEY_SERIES_ID = SS.SURVEY_SERIES_ID
# INNER JOIN GFBioSQL.dbo.ACTIVITY A ON A.ACTIVITY_CODE = TA.ACTIVITY_CODE
# INNER JOIN GFBioSQL.dbo.SPECIES S ON S.SPECIES_CODE = B21.SPECIES_CODE
# WHERE SR.SURVEY_SERIES_ID IN (48, 76, 92, 93)
# ORDER BY B21.TRIP_ID, B21.FE_MAJOR_LEVEL_ID, B22.SPECIMEN_ID")

# all 2004, 2022 comparison work should have a parent_event_id
dsurvey_bio |>
  filter(YEAR %in% c(2004, 2023)) |>
  group_by(FISHING_EVENT_ID, YEAR) |>
  distinct(FISHING_EVENT_ID, .keep_all = TRUE) |>
  reframe(sum = is.na(FE_PARENT_EVENT_ID)) |>
  filter(sum == TRUE)

# all other years do not have a parent event id
dsurvey_bio |>
  filter(!(YEAR %in% c(2004, 2023))) |>
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


saveRDS(dsurvey_bio, "data-raw/dogfish_samples.rds")
saveRDS(catchcount, "data-raw/dogfish_counts.rds")
saveRDS(info, "data-raw/dogfish_sets.rds")


