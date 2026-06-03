# Code is for plot of length densities in SOM

# BC tail extended
# how were the early dogfish lengths measured?

library(TMB)
library(sp)
library(sdmTMB)
library(here)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggsidekick)
library(kableExtra)
library(sf)
theme_set(ggsidekick::theme_sleek())

#load data
samps <- readRDS("data-raw/dogfish_samples_cleaned.rds") #this is from the get all function
#needs age, trip_start_date
#this is a hack for now
samps$trip_start_date <-samps$time_begin_retrieval
samps$age <-NA #age column is empty and therefore dropped. add back in with NAs

# samps <- readRDS("output/samps_joined.rds") |>
#   filter(species_common_name %in% c("NORTH PACIFIC SPINY DOGFISH", "north pacific spiny dogfish"))

# maturity information
library(gfplot)
data("maturity_assignment")
data("maturity_short_names") # males maturity code = 90, female maturity code is >= 77
maturity_assignment
maturity_short_names
dog_maturity_short_names <- filter(maturity_short_names, maturity_convention_desc == "DOGFISH")

# there are a lot of codes I don't know what they are
codes <- unique(dog_maturity_short_names$maturity_code)
sampsm <- samps |> filter(maturity_code %in% codes)
sort(unique(sampsm$maturity_code))
# saveRDS(dsurvey_bio2, "data-raw/dogfish_samples_cleaned_withmaturity.rds")

# 94 cm tl for females is mature based on DFO assessment
# born at 26 and 27 cm.
# suggest growth of 1.5 cm per year.
# 15 year old dogfish would be about ~50 cm
# Males 70 cm mature
# code 55 versus other maturity code is pregnant versus able to be pregnant


# all without 2004 comp work ----------------------------------------------

d <- readRDS("data-raw/wrangled-hbll-dog-sets.rds")
rm <- d |>
  filter(year == 2004 & survey_abbrev == "dog-jhook")
d <- filter(d, !fishing_event_id %in% c(rm$fishing_event_id))

test <- d |>
  mutate(toohigh = ifelse(catch_count > lglsp_hook_count, "morecatch", "lesscatch")) |>
  filter(toohigh == "morecatch")

ggplot() +
  geom_point(data = d, aes(lglsp_hook_count, catch_count, colour = survey_abbrev)) + facet_wrap(~year) +
  geom_point(data = test, aes(lglsp_hook_count, catch_count), colour = 'red') + facet_wrap(~year)

# maturity cutoffs ----------------------------------------------------

samps_86 <- filter(sampsm, year %in% c(1986, 1989))

m86 <- gfplot::fit_mat_ogive(samps_86,
  type = "length",
  sample_id_re = TRUE,
  custom_maturity_at = c(NA, 55)
)
m86$mat_perc$f.p0.95
x <- gfplot::plot_mat_ogive(m86)
#x <- figure(m86)
mat <- m86$pred_data |> mutate(year = 1986)

samps_08 <- filter(sampsm, year == 2008)
m08 <- gfplot::fit_mat_ogive(samps_08,
  type = "length",
  sample_id_re = TRUE,
  custom_maturity_at = c(NA, 55)
)
m08$mat_perc$f.p0.95
y <- gfplot::plot_mat_ogive(m08)
mat2 <- m08$pred_data |> mutate(year = 2008)

samps_23 <- filter(sampsm, year %in% c(2023))
m23 <- gfplot::fit_mat_ogive(samps_23,
  type = "length",
  sample_id_re = TRUE,
  custom_maturity_at = c(NA, 55)
)
m23$mat_perc$f.p0.95
z <- gfplot::plot_mat_ogive(m23)
mat3 <- m23$pred_data |> mutate(year = 2023)

cowplot::plot_grid(x, y, z, labels = c('A. 1986', 'B. 2008', "C. 2023"), label_size = 12, ncol = 1)
ggsave("Figures/maturity-ogives-sog-temporal.jpg", width = 4, height = 6)



#test figure
#m <- readRDS("output/survey_samples_codedmaturity.rds")
m86
m08
m23

raw <- m86$data
m$data <- m86$data |> mutate(mature_num = ifelse(mature == "FALSE", 0.5, 0.5))
#m$pred_data <- filter(m$pred_data, female == 1)  #<- plot females only
nd_re <- m$pred_data
n_re <- length(unique(nd_re$sample_id)) / 5
n_re2 <- ifelse(n_re < 15, 15, n_re)

ann_text <- data.frame(
  age_or_length = c(40, 40, 40, 40), glmm_re = c(0.80, 0.88, 0.60, 0.68),
  lab = c("F05 = 77.0", "F95 = 95.6", "M05 = 65.1", "M95 = 76.7"),
  female = factor(c("1", "1", "0", "0"), levels = c("0", "1"))
)

# string <- data.frame(
#   length = c(
#     as.numeric(fm$age_or_length), as.numeric(fi$age_or_length) + 1,
#     as.numeric(mi$age_or_length), as.numeric(mm$age_or_length - 1)
#   ),
#   female = c(1, 1, 0, 0)
# )

# string <- data.frame(
#   length = c(
#     as.numeric(fm$age_or_length), as.numeric(fi$age_or_length) + 1,
#     as.numeric(mi$age_or_length), as.numeric(mm$age_or_length - 1)
#   ),
#   female = c(1, 1, 0, 0)
# )

p <-
  ggplot() +
  geom_line(
    data = nd_re, aes(age_or_length,
                      as.numeric(glmm_re),
                      group = paste(sample_id, female),
                      #fill = as.character(female),
                      colour = as.character(female)
                      #linetype = as.character(female)
    ),
    #colour = c("red", "grey"),
    inherit.aes = FALSE,
    alpha = 1 / n_re2,
    show.legend = FALSE
  ) +
  # scale_linetype_manual(values = c("dotdash", "solid")) +
  scale_colour_manual(values = c("grey10", "grey50")) +
  scale_x_continuous(limits = c(0,125)) +
  geom_rug(
    data = filter(m$data, mature == "FALSE"), aes(
      x = age_or_length,
      y = mature_num
      #colour = "female"
      #linetype = as.character(female)
    ),
    #colour = c("red", "grey"),
    sides = "b",
    length = unit(c(0.04), "npc"),
    alpha = c(0.05),
    lty = 1,
    show.legend = FALSE
  ) +
  geom_rug(
    data = filter(m$data, mature == "TRUE"), aes(
      x = age_or_length,
      y =mature_num
      #colour = "female"
      #linetype = as.character(female)
    ),
    #colour = c("red", "grey"),
    sides = "t",
    alpha = 0.05,
    # lty = 1,
    show.legend = FALSE
  ) +
  labs(x = "Length (cm)", y = "Probability mature") +
  theme(plot.margin = unit(c(5, 1, 1, 1), "lines")) +
  # geom_vline(
  #   data = string,
  #   aes(
  #     xintercept = length,
  #     #colour = female
  #     #linetype = as.character(female)
  #   ),
  #   colour = c("grey10", "grey10", "grey50", "grey50"),
  #   show.legend = FALSE,
  # ) +
  #facet_wrap(~female, nrow = 1) +
  theme(strip.text.x = element_blank()) +
  theme_classic()

p <- p + geom_text(data = ann_text, aes(age_or_length, glmm_re, label = lab),
                   colour = c("grey10", "grey10", "grey10", "grey10"),
                   show.legend = FALSE)
p
g_mat <- p
