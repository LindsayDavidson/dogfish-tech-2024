
# library -----------------------------------------------------------------
library(sf)
library(ggplot2)
library(tidyverse)
library(sp)

# load data ---------------------------------------------------------------

samples <- readRDS("data-raw/dogfish_samples.rds")
sets <- readRDS("data-raw/dogfish_sets.rds")
catchcount <- readRDS("data-raw/dogfish_counts.rds")

names(samples) <- tolower(names(samples))
names(sets) <- tolower(names(sets))

unique(samples$trip_comment)
samples <- samples |>
  mutate(category = case_when(year %in% c(1986, 1989) ~ "dog",
                              year %in%  c(2005, 2008, 2011, 2014) ~ "dog",
                              year == 2019 & trip_comment == "The 2019 Strait of Georgia Dogfish Longline Survey." ~ "dog",
                              year == 2019 & trip_comment == "The 2019 Dogfish Gear/Timing Comparison Survey. Originally part of 2019 HBLL trip 85073." ~ "comp-hbll",
                              year == 2023 & trip_comment == "The 2023 Dogfish longline gear comparison survey on the Neocaligus. Samples are added at the skate level"  ~ "comp-dog",
                              year == 2023 & trip_comment == "The 2023 Summer Dogfish gear comparison survey on the Neocaligus. Conducted simultaneously with the HBLL survey. No CTDs on first half."  ~ "comp-hbll",
                              year == 2022 ~ "comp-hbll",
                              year == 2023 & trip_comment ==  "The 2023 Dogfish longline gear comparison survey on the Neocaligus. Samples are added at the skate level." ~ "comp-dog",
                              year == 2004 ~ "comp-dog"))
saveRDS(samples, "data-raw/dogfish_samples.rds")

# QA/QC location names -------------------------------------------------------------


sets |>
  select(grouping_spatial_id) |>
  unique()

unique(sets$grouping_spatial_id)

# FIX LOCATION NAMES
sets |> filter(is.na(grouping_spatial_id)) # lots of NAs for grouping_spatial_id

# overlay data locations with polygons
sites <- st_read("data-raw", "dogfish_polygons_noproj2")
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

saveRDS(ptsint2, "data-generated/sets_cleaned.rds")

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
  dplyr::select(grouping_spatial_id, fe_fishing_ground_comment) |>
  distinct()
ggplot(ptsint2, aes(longitude, latitude, colour = grouping_spatial_id, group = fe_fishing_ground_comment)) +
  geom_point()

# QA/QC dates and depth--------------------------------
sets <- readRDS("data-generated/sets_cleaned.rds") |>
  filter(is.na(grouping_spatial_id) == FALSE)

# check depths
unique(sets$grouping_desc) # NAs and a 'SOG Dogfish Site'
unique(sets$grouping_depth_id) # NAs and a 'SOG Dogfish Site'

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

saveRDS(d, "data-generated/sets_cleaned2.rds")


# MERGE SETS AND CATCH COUNTS ---------------------------------------------
sets <- readRDS("data-generated/sets_cleaned2.rds")
count <- readRDS("data-raw/dogfish_counts.rds")

names(sets) <- tolower(names(sets))
names(samples) <- tolower(names(samples))
names(count) <- tolower(names(count))

regsurveys <- sets |>
  filter(survey_series_id %in% c(93, 92)) |>
  left_join(count)

compsurveys <- sets |>
  filter(survey_series_id == 48 & year %in% c(2004, 2022,2023)) |>
  left_join(count, by = c("fishing_event_id" = "fe_parent_event_id", "fe_sub_level_id" = "fe_sub_level_id")) |>
  select(-fishing_event_id.y)

compsurveys2019 <- sets |>
  filter(survey_series_id == 48 & year == 2019) |>
  left_join(count)

final <- rbind(regsurveys, compsurveys, compsurveys2019)
unique(final$year)

final <- final |>
  mutate(category = case_when(year %in% c(1986, 1989) ~ "dog",
            year %in%  c(2005, 2008, 2011, 2014) ~ "dog",
            year == 2019 & survey_desc == "2019 Strait of Georgia Longline Dogfish Survey" ~ "dog",
            year == 2019 & survey_desc == "2019 Dogfish Gear/Timing Comparison Survey" ~ "comp-hbll",
            year == 2023 & survey_desc == "2023 Dogfish Gear Comparison Survey"  ~ "comp-dog",
            year == 2023 &
            survey_desc == "The 2023 Summer Dogfish gear comparison survey on the Neocaligus."  ~ "comp-hbll",
            year == 2022 ~ "comp-hbll",
            year == 2004 ~ "comp-dog"))

final <- final |> mutate(cat2 = paste0(category, hook_desc))
final <- final |> mutate(julian = lubridate::yday(retrive))
final <- final |> mutate(cpue = catch_count / (lglsp_hook_count * soak))
saveRDS(final, "data-generated/dogfishs_allsets_allspecies_counts.rds")


