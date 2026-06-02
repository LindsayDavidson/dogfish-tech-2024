
d <- readRDS("data-raw/wrangled-hbll-dog-sets.rds")
glimpse(d)
#survey_abbrev, year, set, date, soak time (min, lat (start), long (start), lat (end), long(end), depth - start, end, average, hooks fished, useable set?)

#2019 HBLL inside catch numbers by set
#common name, scientific name, 1,2,3,4 and then catch numbers under the set


d2019 <-
  df |>
  filter(year == 2019 & survey_abbrev == "OTHER") |>
  mutate(latitude = round(latitude, 2), latitude_end = round(latitude_end, 2), longitude_end = round(longitude_end,1), longitude = round(longitude, 1)) |>
  mutate(survey_abbrev = "HBLL INS N") |>
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

d2019
#cat(d2019, file = "C:/Dogfish_GearComp_TechReport_2024/DogfishGearComp-2/figures/mytable.tex")

d2022 <-
  df |>
  filter(year == 2022 & survey_abbrev == "OTHER") |>
  mutate(latitude = round(latitude, 2), latitude_end = round(latitude_end, 2), longitude_end = round(longitude_end,1), longitude = round(longitude, 1)) |>
  mutate(survey_abbrev = "HBLL INS S") |>
  mutate(depth_avg = mean(depth_m)) |>
  mutate(usable = ifelse(usability_code == 1, "yes", "no")) |>
  select(survey_abbrev, deployed, hooksize_desc, duration_min, latitude, longitude, latitude_end, longitude_end, depth_begin, depth_end, depth_m, lglsp_hook_count) |>

  knitr::kable(
    # format = "simple",
    #"html",
    format = "latex",
    col.names = c(
      "Survey name", "Date", "Hook size", "Soak time (min)", "Lat", "Long", "End Lat", "End Long", "Start Depth (m)", "End Depth (m)", "mean Depth (m)", "Hooks fished"),
    booktabs = TRUE,
    #align = "llllll",
    align = "c",
    caption = "Bridge log data for each of the comparative sets compelted in 2022 on the HBLL inside south survey.",
    label = "model-configs"
  ) |>
  #kableExtra::column_spec(1, width = "6.5cm") |>
  #kableExtra::add_header_above(c(" " = 1, "Start" = c(5,6), "End" = c(7,8)))  |>
  #kableExtra::column_spec(2:7, width = "4cm") |>
  #kableExtra::column_spec(1:14, width = "4cm") |>
  kableExtra::row_spec(0, bold = TRUE, color = "white", background = "grey")  |>  # header styling
  kableExtra::kable_styling(full_width = FALSE, stripe_color = "grey95")

d2022


d2023_hbllsurvey <-
  df |>
  filter(year == 2023 & survey_abbrev == "OTHER" & month %in% c(8, 9) & day < 27) |>
  mutate(latitude = round(latitude, 2), latitude_end = round(latitude_end, 2), longitude_end = round(longitude_end,1), longitude = round(longitude, 1)) |>
  mutate(survey_abbrev = "HBLL INS S") |>
  mutate(depth_avg = mean(depth_m)) |>
  mutate(usable = ifelse(usability_code == 1, "yes", "no")) |>
  select(survey_abbrev, deployed, hooksize_desc, duration_min, latitude, longitude, latitude_end, longitude_end, depth_begin, depth_end, depth_m, lglsp_hook_count) |>

  knitr::kable(
    # format = "simple",
    #"html",
    format = "latex",
    col.names = c(
      "Survey name", "Date", "Hook size", "Soak time (min)", "Lat", "Long", "End Lat", "End Long", "Start Depth (m)", "End Depth (m)", "mean Depth (m)", "Hooks fished"),
    booktabs = TRUE,
    #align = "llllll",
    align = "c",
    caption = "Bridge log data for each of the comparative sets compelted in 2022 on the HBLL inside south survey.",
    label = "model-configs"
  ) |>
  #kableExtra::column_spec(1, width = "6.5cm") |>
  #kableExtra::add_header_above(c(" " = 1, "Start" = c(5,6), "End" = c(7,8)))  |>
  #kableExtra::column_spec(2:7, width = "4cm") |>
  #kableExtra::column_spec(1:14, width = "4cm") |>
  kableExtra::row_spec(0, bold = TRUE, color = "white", background = "grey")  |>  # header styling
  kableExtra::kable_styling(full_width = FALSE, stripe_color = "grey95")

d2023_hbllsurvey

d2023_dogfishsurvey <-
  df |>
  filter(year == 2023 & survey_abbrev == "OTHER" & month %in% c(9,10)) |>
  mutate(HBLLsurvey = ifelse(month == 9 & day < 27, "erase","keep")) |>
  filter(HBLLsurvey == "erase") |>
  mutate(latitude = round(latitude, 2), latitude_end = round(latitude_end, 2), longitude_end = round(longitude_end,1), longitude = round(longitude, 1)) |>
  mutate(survey_abbrev = "HBLL INS S") |>
  mutate(depth_avg = mean(depth_m)) |>
  mutate(usable = ifelse(usability_code == 1, "yes", "no")) |>
  select(survey_abbrev, deployed, hooksize_desc, duration_min, latitude, longitude, latitude_end, longitude_end, depth_begin, depth_end, depth_m, lglsp_hook_count) |>

  knitr::kable(
    # format = "simple",
    #"html",
    format = "latex",
    col.names = c(
      "Survey name", "Date", "Hook size", "Soak time (min)", "Lat", "Long", "End Lat", "End Long", "Start Depth (m)", "End Depth (m)", "mean Depth (m)", "Hooks fished"),
    booktabs = TRUE,
    #align = "llllll",
    align = "c",
    caption = "Bridge log data for each of the comparative sets compelted in 2022 on the HBLL inside south survey.",
    label = "model-configs"
  ) |>
  #kableExtra::column_spec(1, width = "6.5cm") |>
  #kableExtra::add_header_above(c(" " = 1, "Start" = c(5,6), "End" = c(7,8)))  |>
  #kableExtra::column_spec(2:7, width = "4cm") |>
  #kableExtra::column_spec(1:14, width = "4cm") |>
  kableExtra::row_spec(0, bold = TRUE, color = "white", background = "grey")  |>  # header styling
  kableExtra::kable_styling(full_width = FALSE, stripe_color = "grey95")

d2023_dogfishsurvey
