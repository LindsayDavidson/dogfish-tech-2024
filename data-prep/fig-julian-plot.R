# julian plot

library(dplyr)
library(ggplot2)
library(sf)
library(tidyr)
# remotes::install_github("pbs-assess/gfplot")
library(gfdata)
library(gfplot)
sf::sf_use_s2(FALSE)


d <- readRDS("data-raw/wrangled-hbll-dog-sets.rds") # no west coast VI expansion set
# d <- readRDS("data-raw/wrangled-hbll-dog-sets-hblls.rds") #no expansion set, no hbll north except for the 2008 year, note 2004 got dropped when we dropped NAs in soak time

# d <- filter(d, soak >= 0)
# d <- filter(d, is.na(soak) != TRUE) # get rid of 2004 that has no soak time

id_remove <- d %>%
  filter(grepl("COMPARISON", activity_desc) & !year %in% c(2004, 2023)) |>
  pull(fishing_event_id)

id_remove2 <- dat %>%
  filter(grepl("COMPARISON", activity_desc) & hooksize_desc == "12/0" & year %in% c(2022, 2023, 2024)) |>
  pull(fishing_event_id)

d |>
  #filter(survey_sep != "hbll comp") |>
  #filter(survey_sep != "dog comp") |>
  #filter(survey_abbrev != "OTHER") |> # I want to keep the 2004 and 2023 comp work
  ## filter(survey_abbrev == "hbll") |>

  filter(!fishing_event_id %in% id_remove) %>%
  filter(!fishing_event_id %in% id_remove2) %>%

  mutate(survey_abbrev = ifelse(year == 2023 & time_deployed > as.POSIXct("2023-09-06 09:15:21") & hooksize_desc == "14/0" & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS", "DOG",
    ifelse(year == 2023 & time_deployed > as.POSIXct("2023-09-06 09:15:21") & hooksize_desc == "13/0" & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS", "erase", # don't want this one
      ifelse(year == 2023 & time_deployed <= as.POSIXct("2023-09-06 09:15:21") & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS", "erase",
        ifelse(year == 2004 & hooksize_desc == "14/0" & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS", "DOG",
          ifelse(year == 2004 & hooksize_desc == "12/0" & activity_desc == "DOGFISH GEAR/TIMING COMPARISON SURVEYS", "DOG",
            survey_abbrev
          )
        )
      )
    )
  )) |>
  filter(survey_abbrev != "erase") |>
group_by(survey_abbrev, year) |>
  ggplot() +
  geom_jitter(aes(year, julian, colour = catch_count, size = catch_count), alpha = 0.25) +
  theme_classic() +
  facet_wrap(~survey_abbrev, scales = "free_y") +
  scale_colour_viridis_c() +
  labs(y = "Julian day", x = "Year") +
  guides(size = "none") +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12)
  ) +
  labs(colour = "Catch count")

ggsave("Figures/summary_julian.png", width = 9, height = 3)
