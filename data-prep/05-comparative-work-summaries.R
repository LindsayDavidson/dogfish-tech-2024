# params
cols <- c("#e69b99", "#24492e", "#015b58", "#2c6184", "#89689d")
cols <- c("#d7191c", "#fdae61", "#2c6184", "#2c7bb6")

d <- readRDS("data-raw/wrangled-hbll-dog-sets.rds") # no west coast VI expansion set
# d <- readRDS("data-raw/wrangled-hbll-dog-sets-hblls.rds") #no expansion set, no hbll north except for the 2008 year, note 2004 got dropped when we dropped NAs in soak time
#d <- filter(d, soak >= 0)
#d <- filter(d, is.na(soak) != TRUE) # get rid of 2004 that has no soak time

#2019 survey
d19 <- filter(final, year == 2019 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" )
dim(filter(d19, hooksize_desc == "13/0"))
dim(filter(d19, hooksize_desc == "14/0"))
unique(d19$grouping_desc_original)
d19 |> filter(hooksize_desc == "13/0") |>
  reframe(print(unique(grouping_desc_original)))
d19 |> filter(hooksize_desc == "14/0") |>
  reframe(print(unique(grouping_desc_original)))
d19 |> group_by(hooksize_desc, hook_desc) |>
  reframe(sum = sum(lglsp_hook_count))
d19 |> group_by(hooksize_desc, hook_desc) |>
  reframe(sum = sum(catch_count), sumeffort = sum(lglsp_hook_count)) |>
  mutate(cpue = sum/sumeffort)


#2022 survey
d22 <- filter(final, year == 2022 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" )
unique(d22$survey_sep)
dim(filter(d22, hooksize_desc == "13/0"))
dim(filter(d22, hooksize_desc == "14/0"))
unique(d22$grouping_desc_original)
d22 |> group_by(hooksize_desc, hook_desc) |>
  reframe(sum = sum(lglsp_hook_count))
d22 |> group_by(hooksize_desc, hook_desc) |>
  reframe(sum = sum(catch_count), sumeffort = sum(lglsp_hook_count)) |>
  mutate(cpue = sum/sumeffort)

#2023 survey HBLL
d23 <- filter(final, year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" )
unique(d23$survey_sep)
d23 <- d23 |> filter(survey_timing != "fall")
jhookcomp <- filter(d23, survey_sep == "dog-jhook")
d23 <- d23 |> filter(!fishing_event_id %in% c(jhookcomp$fishing_event_id) )
dim(filter(d23, hooksize_desc == "13/0"))
dim(filter(d23, hooksize_desc == "14/0"))
x <- filter(d23, hooksize_desc == "14/0")
xx <- filter(d23, hooksize_desc == "13/0" & fishing_event_id %in% (x$fishing_event_id))
unique(d23$grouping_desc_original)
d23 |> filter(hooksize_desc == "13/0") |>
  reframe(print(unique(grouping_desc_original)))
d23 |> filter(hooksize_desc == "14/0") |>
  reframe(print(unique(grouping_desc_original)))
d23 |> group_by(hooksize_desc, hook_desc) |>
  reframe(sum = sum(lglsp_hook_count))
d23 |> group_by(hooksize_desc, hook_desc) |>
  reframe(sum = sum(catch_count), sumeffort = sum(lglsp_hook_count)) |>
  mutate(cpue = sum/sumeffort)

#2023 survey falls
d23 <- filter(final, year == 2023 & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS" )
unique(d23$survey_sep)
d23 <- d23 |> filter(survey_timing == "fall")
jhookcomp <- filter(d23, survey_sep == "dog-jhook")
d23 <- d23 |> filter(!fishing_event_id %in% c(jhookcomp$fishing_event_id) )
dim(filter(d23, hooksize_desc == "13/0"))
dim(filter(d23, hooksize_desc == "14/0"))
x <- filter(d23, hooksize_desc == "14/0")
xx <- filter(d23, hooksize_desc == "13/0" & fishing_event_id %in% (x$fishing_event_id))
unique(d23$grouping_desc_original)
d23 |> filter(hooksize_desc == "13/0") |>
  reframe(print(unique(grouping_desc_original))) |>
  print(n = 100)
d23 |> filter(hooksize_desc == "14/0") |>
  reframe(print(unique(grouping_desc_original)))
d23 |> group_by(hooksize_desc, hook_desc) |>
  reframe(sum = sum(lglsp_hook_count))
d23 |> group_by(hooksize_desc, hook_desc) |>
  reframe(sum = sum(catch_count), sumeffort = sum(lglsp_hook_count)) |>
  mutate(cpue = sum/sumeffort)
