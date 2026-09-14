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

id_remove2 <- d %>%
  filter(grepl("COMPARISON", activity_desc) & hooksize_desc == "12/0" & year %in% c(2022, 2023, 2024)) |>
  pull(fishing_event_id)

d <- d |>
  #filter(survey_sep != "hbll comp") |>
  #filter(survey_sep != "dog comp") |>
  #filter(survey_abbrev != "OTHER") |> # I want to keep the 2004 and 2023 comp work
  ## filter(survey_abbrev == "hbll") |>

  filter(!fishing_event_id %in% id_remove) %>%
  filter(!fishing_event_id %in% id_remove2) %>%
  mutate(month_text = ifelse(month == 7, "July",
                             ifelse(month == 8, "Aug.",
                                    ifelse(month == 9, "Sept.",
                                           ifelse(month == 10, "Oct.",
                                                  ifelse(month  == 11, "Nov.", NA)))))) |>

  mutate(month_day = paste(month_text, day)) |>

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
  filter(survey_abbrev != "erase")




# Plot using numeric 'julian' for positioning and 'date_text' for axis labels
ggplot(d) +
  geom_jitter(aes(x = year, y = julian, colour = catch_count, size = catch_count), alpha = 0.25) +
  scale_y_continuous(
    breaks = julian_labels$julian,
    labels = julian_labels$month_day
  ) +
  theme_classic()


d <- d |>
  mutate(month_text = forcats::fct_relevel(month_text,
                                           c("Nov.", "Oct.", "Sept.", "Aug.", "July")))  |>
  drop_na(month, julian, catch_count, year, survey_abbrev) |>
  group_by(survey_abbrev, year)

gg <- ggplot() +
  geom_rect(data = d,
            aes(
              xmin = -Inf, xmax = Inf,
              ymin = min(d$julian), ymax = 212
            ),
            fill = "grey90", alpha = 0.15, inherit.aes = FALSE
  ) +
  geom_rect(data = d,
            aes(
              xmin = -Inf, xmax = Inf,
              ymin = 244, ymax = 273
            ),
            fill = "grey90", alpha = 0.15, inherit.aes = FALSE
  ) +
  geom_rect(data = d,
            aes(
              xmin = -Inf, xmax = Inf,
              ymin = 305, ymax = 334
            ),
            fill = "grey90", alpha = 0.15, inherit.aes = FALSE
  ) +
  geom_jitter(data= d, aes(year, julian,
                  #colour = month_text, size = catch_count
                  colour = catch_count, size = catch_count
                  ), alpha = 0.15) +
  theme_classic() +
  #facet_wrap(~survey_abbrev, scales = "free_y") +
  facet_wrap(~survey_abbrev) +
  #scale_size(range = c(0.05, 10)) +
  scale_colour_viridis_c(guide = guide_legend(override.aes = list(size = 3))) +
  scale_y_continuous(expand = c(0,0)) +
  #   breaks = c(min(julian_labels$julian), max(julian_labels$julian, 10)),
  #   labels = c(min(julian_labels$month_day), max(julian_labels$julian), 10)
  # ) +
  labs(y = "Julian day", x = "Year") +
  guides(size = "none") +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12)
  ) +
  labs(colour = "Catch count", size = NULL)

cowplot::ggdraw(gg) +
  cowplot::draw_text(text = "July", x = 0.10, y = 0.2, size = 10, color = "black", hjust = 0.5, vjust = 0.5) +
  cowplot::draw_text(text = "Aug.", x = 0.10, y = 0.35, size = 10, color = "black", hjust = 0.5, vjust = 0.5) +
  cowplot::draw_text(text = "Sept.", x = 0.1, y = 0.53, size = 10, color = "black", hjust = 0.5, vjust = 0.5) +
  cowplot::draw_text(text = "Oct.", x = 0.13, y = 0.72, size = 10, color = "black", hjust = 0.5, vjust = 0.5) +
  cowplot::draw_text(text = "Nov.", x = 0.10, y = 0.88, size = 10, color = "black", hjust = 0.5, vjust = 0.5)

ggsave("Figures/summary_julian.png", width = 9, height = 4)
