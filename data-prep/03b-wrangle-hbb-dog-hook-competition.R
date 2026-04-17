#NOTE <! to be checked
#2019 is the only year of survey 76 that has count of empty hooks - why?
#for now, dog survey has no hook comp

#hook competition
#hook data for IPHC and HBLL
#library(gfiphc)


#?gfdata::get_skate_level_counts
#??load_iphc_dat
#x <- gfdata::load_iphc_dat()
#glimpse(x)
#iphc_data <- load_iphc_dat() #this has books observation and the number of hooks returned with bait
##https://github.com/pbs-assess/gfdata/blob/master/R/load-iphc-dat.R

d <- readRDS("data-raw/wrangled-hbll-dog-sets.rds") #for the number of hooks fished and bait counts

#dog survey and count of baited hooks
dog <- filter(d , survey_series_id == 76)
unique(dog$count_bait_only)
unique(dog$year)
notna <- filter(dog , !is.na(count_bait_only))
unique(notna$year)
na <- filter(dog , is.na(count_bait_only))
unique(na$survey_series_id)
unique(na$year)

# #another way to do it
# ssid <- c(39, 40, # hbll ins n/s
#           76, 48 )# dogfish comp work, j hook, and circle surveys
#
# # Use bait counts only because other columns have questionable data quality
#
# bait_counts <- gfdata::get_ll_hook_data(species = "north pacific spiny dogfish", ssid = ssid) |>
#   dplyr::select(ssid, year, fishing_event_id, count_bait_only)
#
# bait_counts |> filter(is.na(count_bait_only)) |> tally() #0 NAs but doesn't pull all events (ie ssid ==76)

glimpse(d)
h <- d |> dplyr::select(fishing_event_id, year, survey_series_id, count_bait_only, lglsp_hook_count)
h |> filter(is.na(count_bait_only)) |> tally() #312 NAs, why?? dog survey
h <- h |> filter(!is.na(count_bait_only))

#get to the business of hook comp
h$count_bait_only[h$count_bait_only == 0] <- 1 #cant be zero otherwise you get NAs (zero/hooks observed)
h <- h[h$lglsp_hook_count > 0, ] #they are all above zero

h$prop_bait_hooks <- h$count_bait_only / h$lglsp_hook_count
range(h$prop_bait_hooks)
h$hook_adjust_factor <- -log(h$prop_bait_hooks) / (1 - h$prop_bait_hooks)
plot(h$hook_adjust_factor, h$lglsp_hook_count)
abline(0, 1)
range(h$hook_adjust_factor)

ggplot(h, aes(prop_bait_hooks, hook_adjust_factor)) + geom_point()

#bait/hksosbseved
h$offset = log(h$lglsp_hook_count)
h$offset_hk <- log(h$lglsp_hook_count / h$hook_adjust_factor) # hook comp
unique(h$offset_hk)
range(h$fishing_event_id)

d_hk <- left_join(d, h[,c("fishing_event_id", "year", "survey_series_id", "offset", "offset_hk")], by = c("fishing_event_id", "year", "survey_series_id"))
unique(d_hk$offset_hk)
saveRDS(d_hk, "data-raw/wrangled-hbll-dog-sets-hk.rds")



# # get hook info at the parent level ---------------------------------------
#
# ??get_skate_level_counts
# #pull data from get_event-data.sql
# #does this pull in comm hook data and link better to the comm catches?
#
#
#
# # IPHC hook data from gfiphc ----------------------------------------------
# https://github.com/pbs-assess/gfdata/blob/master/R/load-iphc-dat.R
# ?load_iphc_dat(species = NULL)
# ?get_iphc_hook_data
#
# #IPHC hook data
# # #' Get IPHC hook counts for all survey years
# # #'
# # #' @param path Path where the 'iphc-hook-counts.rds' file is saved to
# # #' @param species Species common name to query, defaults to 'pacific halibut'
# # #'   because to date this species does not affect the output.
# # #'
# # #' @returns An RDS object containing a dataframe with the following columns:
# # #'   - year: The year of the data.
# # #'   - station: The station identifier.
# # #'   - lat: The latitude coordinates.
# # #'   - lon: The longitude coordinates.
# # #'   - baited_hooks: The number of baited hooks.
# # #'   - setID: The set identifier (used by the IPHC, may not match GFBio output)
# # #'   - obsHooksPerSet: The observed hooks per set.
# # #'   - effSkateIPHC: The effective skate IPHC (International Pacific Halibut Commission).
# # #'   - iphcUsabilityCode: The IPHC usability code.
# # #'   - iphcUsabilityDesc: The description of the IPHC usability code.
# # #'
# # #' @export
# # #'
# # get_iphc_hook_data <- function(path = ".", species = "pacific halibut") {
# #   sp_file <- paste0(clean_name(species), ".rds")
# #   sp_dat <- readRDS(file.path(path, sp_file))$set_counts |>
# #     dplyr::mutate(species = species)
# #   # Get hook_bait counts matching GFBio species counts: 1995:2022
#
# #   hook_bait <- readRDS(file.path(path, "hook-with-bait.rds"))$set_counts |>
# #     dplyr::mutate(baited_hooks = ifelse(!is.na(N_it), N_it, N_it20)) |>
# #     dplyr::select(year, station, lat, lon, baited_hooks)
# #   sp_dat <- dplyr::left_join(sp_dat, hook_bait)
#
# #   # Need total observed hook counts to calculate prop_removed
# #   # ---------------------------------------------------------
# #   # Get set information for years 2003:2012; 2014:2019; 2022
# #   if (!file.exists(file.path(path, "iphc-set-info.rds"))) {
# #     message("File: <iphc_sets_info.rds> not found, querying GFBio using
# #         gfiphc::get_iphc_sets_info() - requires VPN connection\n")
# #     iphc_set_info <- get_iphc_sets_info() # requires VPN connection
# #     saveRDS(iphc_set_info, file.path(path, "iphc-set-info.rds"))
# #   }
#
# #   iphc_set_info <- readRDS(file.path(path, "iphc-set-info.rds")) |>
# #     dplyr::rename(lon = "long") |>
# #     dplyr::filter(year != 2022) # these hook counts are wrong from GFBio
#
# #   # Hook counts for 1996 - 2002
# #   set_1996_2002 <-
# #     gfiphc::data1996to2002 |>
# #     dplyr::mutate(species = tolower(spNameIPHC), station = as.character(station)) |>
# #     dplyr::rename(N_it = "catchCount", obsHooksPerSet = hooksObserved) |>
# #     dplyr::select(year, station, lat, lon, obsHooksPerSet, usable)
#
# #   # Hook counts for 1995 and 2013
# #   # Need to sum observations of all 'species' observed to get hook counts
# #   set_1995 <- dplyr::left_join(
# #     gfiphc::setData1995,
# #     gfiphc::countData1995 |> group_by(station) |> summarise(obsHooksPerSet = sum(specCount))
# #   ) |>
# #     dplyr::mutate(year = 1995)
#
# #   set_2013 <- dplyr::left_join(
# #     gfiphc::setData2013,
# #     gfiphc::countData2013 |> group_by(station) |> summarise(obsHooksPerSet = sum(specCount))
# #   )
#
# #   # Hook counts for 2020, 2021, 2022
# #   set_2020_2021_2022 <- dplyr::bind_rows(gfiphc::setData2020, gfiphc::setData2021, gfiphc::setData2022) |>
# #     dplyr::rename(obsHooksPerSet = "hooksObs") |>
# #     dplyr::select(year, station, lat, lon, obsHooksPerSet, usable, standard)
#
# #   set_info <-
# #     dplyr::bind_rows(iphc_set_info, set_1995, set_1996_2002, set_2013, set_2020_2021_2022) %>%
# #     dplyr::select(year, setID, station, obsHooksPerSet, effSkateIPHC, iphcUsabilityCode, iphcUsabilityDesc) %>%
# #     dplyr::distinct(year, setID, station, .keep_all = TRUE)
#
# #   # unique(sp_dat$year)[!unique(sp_dat$year) %in% unique(c(set_info$year))]
#
# #   # Combine set information with species count data
# #   sp_with_hooks <- dplyr::left_join(sp_dat, set_info) |>
# #     dplyr::select(-species, -(E_it:C_it20))
#
# #   # Resolve the many-to-many by using lat/lon as additional key values
# #   year2019_stations <-
# #     dplyr::left_join(
# #       dplyr::filter(sp_dat, (year == 2019 & station %in% c("2099", "2107"))),
# #       dplyr::filter(set_info, (year == 2019 & station %in% c("2099", "2107")))
# #     ) |>
# #     dplyr::select(tidyr::all_of(colnames(sp_with_hooks)))
#
# #   iphc_hook_out <-
# #     sp_with_hooks |>
# #     # simplest to remove unresolved many-to-many and add proper values in
# #     dplyr::filter(!(year == 2019 & station %in% c("2099", "2107"))) |>
# #     dplyr::bind_rows(year2019_stations) |>
# #     dplyr::arrange(year, station) |>
# #     dplyr::select(-usable, -standard)
#
# #   saveRDS(iphc_hook_out, file.path(path, "iphc-hook-counts.rds"))
# # }