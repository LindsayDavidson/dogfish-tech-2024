library(dplyr)
library(tidyverse)

samps <- readRDS("data-raw/comp_sets_allspecies.rds") |> filter(survey_abbrev == "OTHER") |> filter( year %in% c(2019, 2022, 2023))

id_remove <- samps %>% filter(hooksize_desc == "12/0") %>% pull(fishing_event_id)
samps <- filter(samps, !fishing_event_id %in% id_remove) %>%
  arrange(year, fishing_event_id, survey_abbrev)

species <- samps |> drop_na(species_common_name) |> reframe(species_common_name = unique(species_common_name))
samps <- samps |> drop_na(species_common_name)

samps |> dplyr::select(species_common_name, catch_count, year) |>
  filter(catch_count != 0) |>
  group_by(year) |>
  distinct(species_common_name, .keep_all = TRUE) |>
  drop_na(species_common_name) |>
  tally()

samps |> dplyr::select(species_common_name, catch_count, year) |>
  filter(catch_count != 0) |>
  #group_by(year) |>
  distinct(species_common_name, .keep_all = TRUE) |>
  drop_na(species_common_name) |>
  tally()


dim(species)
#pick aspeciestbl#pick a year
#samps <- samps |> filter(year == 2019)
#samps <- samps |> filter(year == 2022)
#samps <- samps |> filter(year == 2023 & time_deployed < "2023-09-27")
samps <- samps |> filter(year == 2023 & time_deployed >= "2023-09-27")

samps <- samps |> dplyr::select(time_deployed, hooksize_desc, species_common_name, catch_count) |> data.frame()

count <- samps |>
  #filter(catch_count >0) |>
  mutate(catch_count = as.numeric(catch_count))

count <- count  |>
  pivot_wider(
    names_from = species_common_name,
    values_from = (catch_count)
  )

count <- data.frame(count)
count[is.na(count)]  <- " "


count <- count %>% select(where(~ any(. != 0)))


x <- colnames(count)
x <- result <- gsub("\\.", " ", x)
x <- str_to_title(x)

length(x) - 2

colnames(count) <- x

# speciestbl <- count |>
#   arrange("Time Deployed") |> distinct(.keep_all = TRUE)



count  |>
  arrange(Time_deployed )  |>
  knitr::kable(
    # format = "simple",
    #"html",
    format = "latex",
    col.names = x,
    booktabs = TRUE,
    #align = "llllll",
    align = "c",


    #caption = "Total catch in pieces by set and hook type of all species encountered in comparative sets fishing in 2019.",
    #label = "species-catch2019"

    #caption = "Total catch in pieces by set and hook type of all species encountered in comparative sets fishing in 2022.",
    #label = "species-catch2022"

    #caption = "Total catch in pieces by set and hook type of all species encountered in comparative sets fishing in 2023 on the HBLL survey.",
    #label = "species-catch2023HBLL"

    caption = "Total catch in pieces by set and hook type of all species encountered in comparative sets fishing in 2023 on the SoG dogfish survey.",
    label = "species-catch2023dog"

  ) |>

  #kableExtra::column_spec(2, extra_css = "transform: rotate(-45deg); transform-origin: left bottom;")

  #kableExtra::column_spec(1, width = "6.5cm") |>
  #kableExtra::add_header_above(c(" " = 1, "Start" = c(5,6), "End" = c(7,8)))  |>
  #kableExtra::column_spec(2:7, width = "4cm") |>
  #kableExtra::column_spec(1:14, width = "4cm") |>
  kableExtra::row_spec(0, bold = TRUE, angle = 90)  |>  # header styling
  kableExtra::kable_styling(full_width = FALSE)

