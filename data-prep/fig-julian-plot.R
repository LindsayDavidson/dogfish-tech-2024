#julian plot

library(dplyr)
library(ggplot2)
library(sf)
library(tidyr)
#remotes::install_github("pbs-assess/gfplot")
library(gfdata)
library(gfplot)
sf::sf_use_s2(FALSE)


d <- readRDS("data-raw/wrangled-hbll-dog-sets.rds") # no west coast VI expansion set
# d <- readRDS("data-raw/wrangled-hbll-dog-sets-hblls.rds") #no expansion set, no hbll north except for the 2008 year, note 2004 got dropped when we dropped NAs in soak time
d <- filter(d, soak >= 0)
d <- filter(d, is.na(soak) != TRUE) # get rid of 2004 that has no soak time

d |>
  filter(survey_sep != "hbll comp") |>
  filter(survey_sep != "dog comp") |>
  filter(survey_abbrev != "OTHER") |>
  # filter(survey_abbrev == "hbll") |>
  group_by(survey_abbrev, year) |>
  ggplot() +
  geom_jitter(aes(year, julian, colour = catch_count, size = catch_count), alpha = 0.25) +
  theme_classic() +
  facet_wrap(~survey_abbrev) +
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



