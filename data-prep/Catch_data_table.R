
#catch record for each of the comparative work surveys
survey, date,  species... total catch

d <- readRDS("output/samps_joined.rds")

d <- readRDS("data-raw/dogfish_counts.rds")
saveRDS(info, "data-raw/dogfish_sets.rds")



hbllc <- readRDS( "data-raw/hbll_allspeciescatch.rds")
hbllinfor <- readRDS( "data-raw/hbll_sets.rds")

hbll <- left_join(hbllinfor, hbllc)
glimpse(hbll)

names(hbll) <- tolower(names(hbll))

x <- hbll |>
  dplyr::select(fishing_event_id, year, species_common_name, catch_count, hooksize_desc, trip_start_date, survey_series_desc) |>
  filter(year == 2019) |>
  filter(hooksize_desc == "14/0") #|>

unique(x$survey_series_desc)
  mutate(fishing_event_id = as.factor(fishing_event_id), catch_count = as.integer(catch_count), species_common_name = as.factor(species_common_name)) |>
  distinct(fishing_event_id, .keep_all = TRUE) |>
  tidyr::pivot_wider(names_from = species_common_name, values_from = catch_count, values_fill = 0)

hbll |>
  dplyr::select(fishing_event_id, year, species_common_name, catch_count) |>
  filter(year == 2022) |>
  mutate(fishing_event_id = as.factor(fishing_event_id), catch_count = as.integer(catch_count), species_common_name = as.factor(species_common_name)) |>
  distinct(fishing_event_id, .keep_all = TRUE) |>
  tidyr::pivot_wider(names_from = species_common_name, values_from = catch_count, values_fill = 0)

hbll |>
  dplyr::select(fishing_event_id, year, species_common_name, catch_count) |>
  filter(year == 2023) |>
  mutate(fishing_event_id = as.factor(fishing_event_id), catch_count = as.integer(catch_count), species_common_name = as.factor(species_common_name)) |>
  distinct(fishing_event_id, .keep_all = TRUE) |>
  tidyr::pivot_wider(names_from = species_common_name, values_from = catch_count, values_fill = 0)


glimpse(d)

d <- d |> dplyr::select(species_common_name, catch_count, fishing_event_id, sample_date , year, month, day, time_deployed, survey_abbrev, activity_desc )
unique(d$species_common_name)

d |>
  dplyr::select(species_common_name, catch_count, year, survey_abbrev) |>
  pivot_wider(names_from = species_common_name, values_from = catch_count, values_fill = 0)

d %>%
  pivot_wider(names_from = species_common_name, values_from = as.numeric(catch_count), values_fill = 0)


d |>
  dplyr::select(fishing_event_id, species_common_name, catch_count) |>
  mutate(fishing_event_id = as.factor(fishing_event_id), catch_count = as.integer(catch_count), species_common_name = as.factor(species_common_name)) |>
  distinct(fishing_event_id, .keep_all = TRUE) |>
  tidyr::pivot_wider(names_from = species_common_name, values_from = catch_count, values_fill = 0)

fish_encounters
fish_encounters |>
  pivot_wider(names_from = station, values_from = seen)

# Fill in missing values
fish_encounters |>
  pivot_wider(names_from = station, values_from = seen, values_fill = 0)


df |>
  # #choose one
  # filter(year == 2019 & survey_abbrev == "OTHER") |> #2019 dogfish comp work
  # mutate(survey_abbrev = "HBLL INS N") |>

  # filter(year == 2022 & survey_abbrev == "OTHER") |> #2022 comp work
  # mutate(survey_abbrev = "HBLL INS S") |>

  #filter(year == 2023 & survey_abbrev == "OTHER" & month %in% c(8, 9) & day < 27) |> #2023 HBLL comp work
  #mutate(survey_abbrev = "HBLL INS N") |>

  filter(year == 2023 & survey_abbrev == "OTHER" & month %in% c(9,10)) |>  #2023 Dogfish comp work
  mutate(HBLLsurvey = ifelse(month == 9 & day < 27, "erase","keep")) |>
  filter(HBLLsurvey == "keep") |>
  mutate(survey_abbrev = "DOG") |>


  mutate(latitude = round(latitude, 2), latitude_end = round(latitude_end, 2), longitude_end = round(longitude_end,1), longitude = round(longitude, 1)) |>
  mutate(depth_avg = mean(depth_m)) |>
  mutate(usable = ifelse(usability_code == 1, "yes", "no")) |>
  select(survey_abbrev, deployed, hooksize_desc, duration_min, latitude, longitude, latitude_end, longitude_end, depth_begin, depth_end, depth_m, lglsp_hook_count) |>

  knitr::kable(
    # format = "simple",
    #"html",
    format = "latex",
    col.names = c(
      "Survey name",  "Date", "Hook size", "Soak time (min)", "Latitude", "Longitude", "End Latitude", "End Longitude", "Start Depth (m)", "End Depth (m)", "mean Depth (m)", "Hooks fished"),
    booktabs = TRUE,
    #align = "llllll",
    align = "c",
    caption = "Bridge log data for each of the comparative sets.",
    label = "model-configs"
  ) |>
  #kableExtra::column_spec(1, width = "6.5cm") |>
  #kableExtra::add_header_above(c(" " = 1, "Start" = c(5,6), "End" = c(7,8)))  |>
  #kableExtra::column_spec(2:7, width = "4cm") |>
  #kableExtra::column_spec(1:14, width = "4cm") |>
  kableExtra::row_spec(0, bold = TRUE, color = "white", background = "grey")  |>  # header styling
  kableExtra::kable_styling(full_width = FALSE, stripe_color = "grey95")


