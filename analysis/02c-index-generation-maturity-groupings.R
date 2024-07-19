#Code is for plot of length densities in SOM

#BC tail extended
#how were the early dogfish lengths measured?

# library -----------------------------------------------------------------

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


# Samps ----------------------------------------------------

samps <- readRDS("output/samps_CoastalTrawl.rds")

# maturity information
library(gfplot)
data("maturity_assignment")
data("maturity_short_names") # males maturity code = 90, female maturity code is >= 77
maturity_assignment
maturity_short_names

# 94 cm tl for females is mature based on DFO assessment
# born at 26 and 27 cm.
# suggest growth of 1.5 cm per year.
# 15 year old dogfish would be about ~50 cm
# Males 70 cm mature



# cut maturity cutoffs ----------------------------------------------------


m <- gfplot::fit_mat_ogive(survey_samples,
                           type = "length",
                           sample_id_re = TRUE,
                           custom_maturity_at = c(NA, 55))
gfplot::plot_mat_ogive(m)

xx <- m$pred_data
f95<- filter(xx, female == 1 & glmm_re >= 0.95)
range(f95$age_or_length)
f05 <- filter(xx, female == 1 & glmm_re <= 0.05)
range(f05$age_or_length)
min(m95$age_or_length)
max(m95$age_or_length)

xy <- m$pred_data
m95<- filter(xy, female == 0 & glmm_re >= 0.95)
range(m95$age_or_length)
m05 <- filter(xy, female == 0 & glmm_re <= 0.05)
range(m05$age_or_length)
min(m95$age_or_length)
min(m95$age_or_length)

#tally by length group and calculate weight of each group
count3 <- count2 |>
  filter(sex != 0) |>
  mutate(lengthgroup = ifelse(length >= 77 & length < 95.5 & sex == 2, "maturingf",
                              ifelse(length >= 65.1 & length < 76.7 & sex == 1 , "maturingm",
                                     ifelse(length >= 95.5 & sex == 2, "mf",
                                            ifelse(length >= 76.7 & sex == 1, "mm",
                                                   ifelse(length < 77 & sex == 2, "imm",
                                                          ifelse(length < 65.1 & sex == 1, "imm",
                                                                 NA))))))) |>
  group_by(year, lengthgroup, survey_name, fishing_event_id) |>
  mutate(sumweightsamps = sum(weightcomplete)) |>
  distinct(year, lengthgroup, .keep_all = TRUE) |>
  group_by(year, survey_name, fishing_event_id) |>
  mutate(sumtotalweight = sum(sumweightsamps)) |>
  mutate(ratio = sumweightsamps/sumtotalweight*100) |>
  distinct(year, lengthgroup, sumweightsamps, .keep_all = TRUE)

ggplot(count3, aes(year, log(ratio), group = lengthgroup, colour = lengthgroup)) +
  geom_line() +
  facet_wrap(~survey_abbrev)

# SOM figure ----------------------------------------------------
unique(samps$survey_name2)
unique(samps$survey_name)
unique(samps$survey_abbrev)

breakpts <- c(c(1986, 1996, 2002), seq(2003, 2023, 10))
breakpts <- seq(1996, 2022, 7)
samps_hist <- samps %>%
  mutate(year_group = findInterval(year, breakpts)) |>
  drop_na(length_ext_cm) |>
  filter(!sex %in% c(0, 3))

ggplot(samps_hist, aes(length_ext_cm, group = as.factor(year_group), fill = as.factor(year_group)),
       colour = as.factor(year_group)
) + # geom_vline (xintercept = samps_hist$mature, group = as.factor(samps_hist$sex) )+
  geom_density(alpha = 0.6, size = 0.25) + # , bins = 50) +
  facet_grid(~ survey_name2 + sex)


samps <- filter(samps, !survey_name %in% c("AFSC.Slope", "Triennial"))
range(samps$year)
# breakpts <- c(c(1996, 2002),  2013, 2023)
breakpts <- seq(1996, 2022, 7)
unique(samps$survey_name)

range(samps$length_ext_cm)

samps_hist |>
  mutate(length_ext_cm = as.numeric(length_ext_cm)) |>
  filter(length_ext_cm > 12) |>
  group_by(survey_name) |>
  summarize(sex_length_mean = mean(length_ext_cm)) |>
  ungroup()

samps |>
  filter(sex %in% c(1, 2)) |>
  filter(length_ext_cm <=12)

samps |>
  filter(sex %in% c(1, 2)) |>
  filter(length_ext_cm >12) |>
  group_by(sex) |>
  drop_na(length_ext_cm) |>
  summarise(min = min(length_ext_cm), max = max(length_ext_cm))

samps |>
  filter(sex %in% c(1, 2)) |>
  group_by(sex) |>
  drop_na(length_ext_cm) |>
  summarise(min = min(length_ext_cm), max = max(length_ext_cm))

samps |>
  filter(sex %in% c(1, 2)) |>
  filter(length_ext_cm >12) |>
  group_by(survey_name2, sex) |>
  drop_na(length_ext_cm) |>
  summarise(min = min(length_ext_cm), max = max(length_ext_cm), mean(length_ext_cm),
            median(length_ext_cm))

dissamps_hist <- samps %>%
  mutate(year_group = findInterval(year, breakpts)) |>
  drop_na(length_ext_cm) |>
  filter(!sex %in% c(0, 3))

# means <- samps_hist |>
#   mutate(length_ext_cm = as.numeric(length_ext_cm)) |>
#   group_by(year_group, survey_name2, sex) |>
#   summarize(sex_length_mean = mean(length_ext_cm)) |>
#   ungroup()

samps_hist$survey_name2 <- factor(samps_hist$survey_name2,
                                  levels = c("GOA", "BC", "NWFSC.Combo.pass2", "NWFSC.Combo.pass1")
)

year.labs <- c("'96-02", "'03-'09", "'10-'16", "'17-'22")
names(year.labs) <- c("1", "2", "3", "4")


#label names
unique(samps_hist$survey_name2)
samps_hist$survey_name2 <- factor(samps_hist$survey_name2,
                                  levels = c("GOA", "BC", "NWFSC.Combo.pass2", "NWFSC.Combo.pass1")
)

survey.labs <- c("NWFSC 1", "NWFSC 2", "BC", "GOA")
names(survey.labs) <- c("NWFSC.Combo.pass1", "NWFSC.Combo.pass2", "BC", "GOA")

sex.labs <- c("Males", "Females")
names(sex.labs) <- c("1", "2")

samps_hist$mature <- ifelse(samps_hist$sex == 2, 97, 70) # mature code 55

# install.packages("wesanderson")
library(wesanderson)
names(wes_palettes)
x <- wes_palette("Moonrise2", n = 4, type = c("continuous"))

#install.packages("PNWColors")
x <- PNWColors::pnw_palette(name="Mushroom",n=6,type="discrete")

ggplot(samps_hist, aes(length_ext_cm, group = as.factor(year_group), fill = as.factor(year_group)),
       colour = as.factor(year_group)
) +
  geom_vline(
    data = samps_hist,
    aes(xintercept = mature)
  ) +
  # geom_vline (xintercept = samps_hist$mature, group = as.factor(samps_hist$sex) )+
  geom_density(alpha = 0.6, size = 0.25) + # , bins = 50) +
  facet_grid(
    rows = vars(survey_name2), cols = vars(sex), scales = "free",
    labeller = ggplot2::labeller(sex = sex.labs, survey_name2 = survey.labs)
  ) +
  scale_x_continuous(
    breaks = c(0, 25, 75, 125), labels = c(0, 25, 75, 125),
    limits = c(0, 125), name = "Length (cm)"
  ) +
  scale_y_continuous(breaks = c(0, 0.03, 0.06), labels = c(0, 0.03, 0.06), name = "Density") +
  # scale_fill_viridis_d( ) +
  # scale_colour_manual(values = "black") +
  scale_fill_manual("Years", values = x, labels = c("'96-02", "'03-'09", "'10-'16", "'17-'22")) +
  scale_colour_manual(values = x) +
  theme(
    plot.background = element_rect(fill = "NA", colour = "NA"),
    # text = element_text(family= "Gill Sans MT"),
    axis.line.x = element_line(colour = "grey60"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    plot.margin = margin(1, 0, 1, 0.5, "cm"),
    panel.background = element_rect(fill = "white", colour = "grey60"),
    axis.text.x = element_text(size = 10, vjust = 1, colour = "grey20"),
    axis.text.y = element_text(size = 10, colour = c("grey20")),
    axis.title.x = element_text(size = 10, colour = "grey20"),
    axis.title.y = element_text(size = 10, colour = "grey20"),
    axis.ticks.length = unit(0.15, "cm"),
    strip.text = element_text(size = 10),
    axis.ticks.x = element_line(colour = "grey60"),
    axis.ticks.y = element_line(colour = "grey60")
  )

ggsave("Figures/length_summary_all.png", width = 6, height = 4)




# iphc size class figures by region ---------------------------------------
unique(samps_hist$fmp)
samps_hist <- samps_hist |> filter(fmp %in% c("WC", "CAN", "GOA"))


# SOM Figure for manuscript: density figure of IPHC and trawl, regions combined -------------------------------

breakpts <- c(c(1996, 2002), seq(2003, 2023, 10))

sampsTL <- readRDS("output/samps_CoastalTrawl.rds") |>
  drop_na(length_ext_cm) |>
  mutate(survey_name2 = ifelse(survey_name == "NWFSC.Combo",
                               survey_abbrev,
                               survey_name
  )) |>
  filter(!survey_name %in% c("Triennial", "AFSC.Slope"))

unique(sampsTL$survey_name)

samps_histtl <- sampsTL %>%
  mutate(year_group = findInterval(year, breakpts)) |>
  drop_na(length_ext_cm) |>
  filter(!sex %in% c(0, 3)) |>
  mutate(survey_name2 = ifelse(survey_name2 == "BC", "BC",
                               ifelse(survey_name == "GOA", "GOA",
                                      "WC"
                               ))) |>
  mutate(survey_name3 = ifelse(survey_name2 == "BC", "BC",
                               ifelse(survey_name == "GOA", "GOA",
                                      ifelse(survey_abbrev == "NWFSC.Combo.pass1", "nwpass1",
                                             "nwpass2")))) |>
  dplyr::select(year, length_ext_cm, sex, survey_name2, survey_name3, year_group) |>
  mutate(survey_group = "trawl")

samps_histip <- iphcsamps %>%
  mutate(year_group = findInterval(year, breakpts), sexMF = sex) |>
  drop_na(length) |>
  mutate(sex = ifelse(sexMF == "F", 2, ifelse(sexMF == "M", 1, 0))) |>
  filter(sex %in% c(1, 2)) |>
  mutate(survey_name2 = ifelse(fmp == "CAN", "BC", fmp),
         survey_name3 = survey_name2) |>
  dplyr::select(year, length_ext_cm = length, sex, survey_name2,survey_name3, year_group) |>
  mutate(survey_group = "iphc")

samps_hist <- rbind(samps_histip, samps_histtl)
unique(samps_hist$survey_name2)
unique(samps_hist$survey_name3)

samps_hist$survey_name2 <- factor(samps_hist$survey_name2,
                                  levels = c("GOA", "BC", "WC")
)

year.labs <- c("'96-02", "'03-'13", "'14-'22")
names(year.labs) <- c("1", "3", "4")


survey.labs <- c("WC", "BC", "GOA")
names(survey.labs) <- c("WC", "BC", "GOA")

sex.labs <- c("Males", "Females")
names(sex.labs) <- c("1", "2")

samps_hist$mature <- ifelse(samps_hist$sex == 2, 95.5, 76.7) #see split-index_byregionsandmaturity.R for maturity ogive
samps_hist$immature <- ifelse(samps_hist$sex == 2, 77, 65.1)

# install.packages("wesanderson")
library(wesanderson)
names(wes_palettes)
x2 <- wes_palette("FantasticFox1", 3, type = c("continuous"))
# x <- wes_palette("Moonrise2", n = 4, type = c("continuous"))
# x2 <- c(x[3], x[2], x[1])

ggplot(samps_histtl, aes(length_ext_cm, group = as.factor(survey_name2), fill = as.factor(survey_name2)),
       colour = as.factor(survey_name2)
) +
  geom_vline(
    data = samps_hist,
    aes(xintercept = mature)
  ) +

  # geom_vline (xintercept = samps_hist$mature, group = as.factor(samps_hist$sex) )+
  geom_density(alpha = 0.6, size = 0.25) + # , bins = 50) +
  facet_grid(
    cols = vars(sex)
  )

ggplot(samps_hist, aes(length_ext_cm, group = as.factor(survey_name3), fill = as.factor(survey_name3)),
       colour = as.factor(survey_name3)
) +
  geom_vline(
    data = samps_hist,
    aes(xintercept = mature)
  ) +

  # geom_vline (xintercept = samps_hist$mature, group = as.factor(samps_hist$sex) )+
  geom_density(alpha = 0.6, size = 0.25) + # , bins = 50) +
  facet_grid(
    rows = vars(survey_group), cols = vars(sex), # , scales = "free",
    labeller = ggplot2::labeller(sex = sex.labs, survey_name = survey.labs)
  ) +
  scale_x_continuous(
    breaks = c(0, 25, 75, 125), labels = c(0, 25, 75, 125),
    limits = c(0, 125), name = "Length (cm)"
  ) +
  scale_y_continuous(breaks = c(0, 0.05, 0.09), labels = c(0, 0.05, 0.09), name = "Density") +
  theme(
    plot.background = element_rect(fill = "NA", colour = "NA"),
    # text = element_text(family= "Gill Sans MT"),
    axis.line.x = element_line(colour = "grey60"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    plot.margin = margin(1, 0, 1, 0.5, "cm"),
    panel.background = element_rect(fill = "white", colour = "grey60"),
    axis.text.x = element_text(size = 10, vjust = 1, colour = "grey20"),
    axis.text.y = element_text(size = 10, colour = c("grey20")),
    axis.title.x = element_text(size = 10, colour = "grey20"),
    axis.title.y = element_text(size = 10, colour = "grey20"),
    axis.ticks.length = unit(0.15, "cm"),
    strip.text = element_text(size = 10),
    axis.ticks.x = element_line(colour = "grey60"),
    axis.ticks.y = element_line(colour = "grey60")
  )

ggsave("Figures/length_summary_trawliphc_pass1pass2.png", width = 6, height = 4)


ggplot(samps_hist, aes(length_ext_cm, group = as.factor(survey_name2), fill = as.factor(survey_name2)),
       colour = as.factor(survey_name2)
) +
  geom_vline(
    data = samps_hist,
    aes(xintercept = mature)
  ) +

  # geom_vline (xintercept = samps_hist$mature, group = as.factor(samps_hist$sex) )+
  geom_density(alpha = 0.6, size = 0.25) + # , bins = 50) +
  facet_grid(
    rows = vars(survey_group), cols = vars(sex), # , scales = "free",
    labeller = ggplot2::labeller(sex = sex.labs, survey_name = survey.labs)
  ) +
  scale_x_continuous(
    breaks = c(0, 25, 75, 125), labels = c(0, 25, 75, 125),
    limits = c(0, 125), name = "Length (cm)"
  ) +
  scale_y_continuous(breaks = c(0, 0.05, 0.09), labels = c(0, 0.05, 0.09), name = "Density") +
  scale_fill_manual("Region", values = x2) + # , labels = c("'96-02", "'03-'13", "'14-'22")) +
  # scale_colour_manual(values = "black") +
  scale_fill_grey("Region") +
  theme(
    plot.background = element_rect(fill = "NA", colour = "NA"),
    # text = element_text(family= "Gill Sans MT"),
    axis.line.x = element_line(colour = "grey60"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    plot.margin = margin(1, 0, 1, 0.5, "cm"),
    panel.background = element_rect(fill = "white", colour = "grey60"),
    axis.text.x = element_text(size = 10, vjust = 1, colour = "grey20"),
    axis.text.y = element_text(size = 10, colour = c("grey20")),
    axis.title.x = element_text(size = 10, colour = "grey20"),
    axis.title.y = element_text(size = 10, colour = "grey20"),
    axis.ticks.length = unit(0.15, "cm"),
    strip.text = element_text(size = 10),
    axis.ticks.x = element_line(colour = "grey60"),
    axis.ticks.y = element_line(colour = "grey60")
  )

ggsave("Figures/length_summary_trawliphc.png", width = 6, height = 4)

unique(samps_hist$mature)


x <- PNWColors::pnw_palette("Cascades", 3)
samps_hist$survey_group <- factor(samps_hist$survey_group,
                                  levels = c("trawl",  "iphc"))

# ggplot(filter(samps_hist, survey_group == "trawl"), aes(length_ext_cm, group =
#                                                           as.factor(survey_name2),
#                                                         fill = as.factor(survey_name2)),
#        colour = as.factor(survey_name2)
# ) +
ggplot(samps_hist, aes(length_ext_cm, group = as.factor(survey_name2), fill = as.factor(survey_name2)),
       colour = as.factor(survey_name2)
) +
  geom_vline(
    data = samps_hist,
    aes(xintercept = mature), colour = "grey70"
  ) +
  geom_vline(
    data = samps_hist,
    aes(xintercept = immature), colour = "grey70"
  ) +
  # geom_vline (xintercept = samps_hist$mature, group = as.factor(samps_hist$sex) )+
  geom_density(alpha = 0.6, size = 0.25) + # , bins = 50) +
  facet_grid(
    rows = vars(survey_group),
    cols = vars(sex), # , scales = "free",
    labeller = ggplot2::labeller(sex = sex.labs, survey_name2 = survey.labs)
  ) +
  scale_x_continuous(
    breaks = c(0, 25, 75, 125), labels = c(0, 25, 75, 125),
    limits = c(0, 125), name = "Length (cm)"
  ) +
  scale_y_continuous(breaks = c(0, 0.02,  0.04,  0.06,  0.08),
                     labels = c(0, 0.02, 0.04, 0.06, 0.08), name = "Density") +
  scale_fill_manual("Region", values = x) + # , labels = c("'96-02", "'03-'13", "'14-'22")) +
  # scale_colour_manual(values = "black") +
  #scale_fill_grey("Region") +
  theme(
    plot.background = element_rect(fill = "NA", colour = "NA"),
    # text = element_text(family= "Gill Sans MT"),
    axis.line.x = element_line(colour = "grey60"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    plot.margin = margin(1, 0, 1, 0.5, "cm"),
    panel.background = element_rect(fill = "white", colour = "grey60"),
    axis.text.x = element_text(size = 12, vjust = 1, colour = "grey20"),
    axis.text.y = element_text(size = 12, colour = c("grey20")),
    axis.title.x = element_text(size = 12, colour = "grey20"),
    axis.title.y = element_text(size = 12, colour = "grey20"),
    axis.ticks.length = unit(0.15, "cm"),
    strip.text = element_text(size = 12),
    axis.ticks.x = element_line(colour = "grey60"),
    axis.ticks.y = element_line(colour = "grey60")
  )
ggsave("Figures/length_summary_trawl.png", width = 5, height = 4)




