#table of the biological data collected

#species, latin, num of samples (what does that mean?), length, weight, sex, age, maturity
# I need to pull this in? How do I pull in all of the samples for all of the species encountered

library(tidyverse)

d <- readRDS("data-raw/dogfish_samples.rds") |> filter(YEAR == 2023 & SURVEY_DESC == "2023 Dogfish Gear Comparison Survey")
species <- unique(d$SPECIES_COMMON_NAME)

d <- get_all_survey_samples(species = species, ssid = c(48), include_event_info = TRUE)
saveRDS(d, "data-raw/samples-all-species.rds")

id_remove <- d %>% filter(hooksize_desc == "12/0") %>% pull(fishing_event_id)

d <- filter(d, !fishing_event_id %in% id_remove) %>%
  arrange(year, fishing_event_id, survey_abbrev)

d <- filter(d, year %in% c(2019, 2022, 2023))

sppcount <- d |> group_by(year, species_common_name) |>
  reframe(num.individuals = n())

length <- d |>  group_by(year, species_common_name) |>
  drop_na(length) |>
  filter(length != 0) |>
  reframe(length = n())

weight <- d |>  group_by(year, species_common_name) |>
  drop_na(weight) |>
  filter(weight != 0) |>
  reframe(weight = n())

sex <- d |>  group_by(year, species_common_name) |>
  drop_na(sex) |>
  filter(sex != 0) |>
  reframe(sex = n())

maturity <- d |>  group_by(year, species_common_name) |>
  drop_na(maturity_desc) |>
  reframe(maturity = n())

age <- d |>  group_by(year, species_common_name) |>
  drop_na(age_specimen_collected) |>
  filter(age_specimen_collected == 1) |>
  reframe(age_collected = n())

table <- left_join(sppcount, weight) |>
  left_join(length) |>
  left_join(sex) |>
  left_join(maturity) |>
  left_join(age)

saveRDS(table, "data-raw/dogfish_samples_table.rds")

table[is.na(table)] <- 0
table$species_common_name <- str_to_title(table$species_common_name)

table |>
knitr::kable(
  #format.args = list(big.mark = ","),
    format = "latex",
    col.names = c(
      "Year",  "Species common name", "Total number of individuals sampled", "Weight", "Length", "Sex", "Maturity", "Age"),
    booktabs = TRUE,
    #align = "llllll",
    align = "c",
    caption = "Number of biological samples and individuals examined by species collected on the 2019, 2022, 2023 comparative sets.",
    label = "table-data-collected"
  ) |>
  #kableExtra::row_spec(0, bold = TRUE, color = "white", background = "grey")  |>  # header styling
  kableExtra::kable_styling(full_width = FALSE) |>
  kableExtra::add_header_above(c(" " = 3, "Number of individuals sampled for:" = 5))




