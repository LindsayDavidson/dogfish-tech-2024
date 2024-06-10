# Length and catch composition differences between the DOG and HBLL surveys

# library -----------------------------------------------------------------
library(gfdata)
library(gfplot)
library(tidyverse)
library(here)
library(sdmTMB)
library(sf)
library(sp)


# load sample data get_survey_samples --------------------------------------------------------
allsamps <- get_survey_samples("north pacific spiny dogfish")
unique(allsamps$survey_series_desc)
test <- filter(allsamps, survey_series_desc == "Strait of Georgia Dogfish Longline")
unique(test$year)

dogsamps <- filter(allsamps, survey_series_desc %in% c(
  "Hard Bottom Longline Inside North ",
  "Strait of Georgia Dogfish Longline",
  "Hard Bottom Longline Inside South "
))

saveRDS(dogsamps, "output/dogfish_samps.rds")


# load data ---------------------------------------------------------------
samps <- readRDS("output/dogfish_samps.rds")

# filter data  -----------------------------------------------------------
samps <- samps |> mutate(name = ifelse(year %in% c(1986, 1989), "DOGJhooks", survey_abbrev))
samps <- samps %>%
  mutate(dmy = lubridate::ymd(trip_start_date)) |>
  mutate(julian = lubridate::yday(dmy))

ggplot(samps, aes(year, julian, group = name, colour = name)) +
  geom_point() +
  geom_line() +
  theme_classic()

samps %>%
  filter(survey_abbrev == "HBLL INS N") %>%
  mutate(max = max(julian)) |>
  select(year, julian) |> distinct()

# id date
samps <- filter(samps, sex %in% c(1, 2))
# samps <- filter(samps, year >= 2000)
range(samps$length)
samps |>
  group_by(year) |>
  filter(length < 25) |>
  tally()

samps <- filter(samps, length > 25)
sort(unique(samps$year))
sort(unique(samps$sex)) # 1 male, 2 female, 3 unknown?

# density plots -----------------------------------------------------------
ggplot() +
  geom_density(
    data = samps, aes(length,
                      group = as.factor(name),
                      fill = as.factor(name)
    ),
    alpha = 0.35, size = 1, colour = "black"
  ) +
  facet_wrap(~sex) +
  theme_classic() +
  geom_rug(data = samps, aes(length)) +
  scale_fill_manual(values = c("lightblue", "darkblue", "yellow", "orange")) +
  # scale_fill_viridis_d() +
  ylab(label = "Density") +
  xlab(label = "Length")

ggplot() +
  geom_density(
    data = filter(samps, name != "DOGJhooks"), aes(length,
                                                   group = as.factor(name),
                                                   fill = as.factor(name)
    ),
    alpha = 0.35, size = 1, colour = "black"
  ) +
  facet_wrap(~sex) +
  theme_classic() +
  scale_fill_manual(values = c("lightblue", "yellow", "orange")) +
  # scale_fill_viridis_d() +
  ylab(label = "Density") +
  xlab(label = "Length")

ggplot() +
  geom_density(
    data = filter(samps, name %in% c("DOG", "HBLL INS S")), aes(length,
                                                                group = as.factor(name),
                                                                fill = as.factor(name)
    ),
    alpha = 0.35, size = 1, colour = "black"
  ) +
  facet_grid(~sex) +
  theme_classic() +
  geom_rug(data = samps, aes(length)) +
  # scale_fill_manual(values = c("lightblue", "darkblue", "yellow", "orange")) +
  scale_fill_viridis_d() +
  ylab(label = "Density") +
  xlab(label = "Length")

# ggplot() +
#   # geom_jitter(data = samps, aes(year, length,
#   #            colour = as.factor(survey_series_desc), alpha = 0.15)) +
#   geom_jitter(
#     data = samps, aes(name, length,
#                       group = as.factor(name),
#                       fill = as.factor(name)
#     ),
#     alpha = 0.15, size = 0.5, colour = "grey10"
#   ) +
#   geom_violin(
#     data = samps, aes(name, length,
#                       group = as.factor(name),
#                       fill = as.factor(name)
#     ),
#     size = 1, colour = "black", draw_quantiles = c(0.5)
#   ) +
#   facet_grid(~sex) +
#   theme_classic() +
#   # geom_rug(data = samps, aes(length)) +
#   #scale_fill_manual(values = c("lightblue", "darkblue", "yellow", "orange")) +
#   # scale_fill_viridis_d() +
#   ylab(label = "Length") +
#   xlab(label = "Year")

ggplot() +
  # geom_jitter(data = samps, aes(year, length,
  #            colour = as.factor(survey_series_desc), alpha = 0.15)) +
  geom_boxplot(
    data = filter(samps, sex %in% c(1, 2)), aes(as.factor(year), length,
                                                fill = as.factor(name)
    ),
    alpha = 0.35, size = 1, colour = "black"
  ) +
  # facet_wrap(~survey_series_desc)
  facet_grid(~ survey_abbrev + sex) +
  theme_classic() +
  scale_x_discrete(breaks = c(2003, 2009, 2015, 2022)) +
  # geom_rug(data = samps, aes(length)) +
  # scale_fill_manual(values = c("blue", "darkblue", "yellow")) +
  # scale_fill_viridis_d() +
  ylab(label = "Length") +
  xlab(label = "Year")

x <- filter(samps, name %in% c("HBLL INS S", "DOG"))
ggplot() +
  # geom_jitter(data = samps, aes(year, length,
  #            colour = as.factor(survey_series_desc), alpha = 0.15)) +
  geom_boxplot(
    data = filter(x, sex %in% c(1, 2)), aes(as.factor(year), length,
                                            fill = as.factor(name)
    ),
    alpha = 0.35, size = 1, colour = "black"
  ) +
  # facet_wrap(~survey_series_desc)
  facet_grid(~ sex) +
  theme_classic() +
  scale_x_discrete(breaks = c(2003, 2009, 2015, 2022)) +
  # geom_rug(data = samps, aes(length)) +
  # scale_fill_manual(values = c("blue", "darkblue", "yellow")) +
  # scale_fill_viridis_d() +
  ylab(label = "Length") +
  xlab(label = "Year")


# anova - are the group means different? ----------------------------------

f <- filter(samps, sex == 2)
mean(f$length[(f$name) == "DOG"])
mean(f$length[(f$name) == "HBLL INS S"])

aov <- aov(length ~ name, data = filter(samps, sex == 2))
summary(aov)
# plot(aov)
tukey <- TukeyHSD(aov)
tukey

aov <- aov(length ~ name, data = filter(samps, sex == 1))
summary(aov)
# plot(aov)
tukey <- TukeyHSD(aov)
tukey
plot(tukey)


# GLM of sex and survey ---------------------------------------------------

f <- filter(samps, sex == 2 & name %in% c("DOG", "HBLL INS S"))
f <- filter(samps, sex == 2 & name == "DOG")
f <- filter(samps, sex == 2 & name == "HBLL INS S")
m <- glm(length ~ name * year, data = f, family = gaussian())
summary(m)

# Ratio of M:F  -----------------------------------------------------------

# females maturing 77 - 95.5
# males maturing
samps
glimpse(samps)

samps |> drop_na(weight) |> tally()
samps |> tally()
115952 - 6984 #lots of NAs for weight

#use maturity DFO of 55
#these legnths are from a length weight and length matuirty curve calculated in
#split-index_by_regionandmaturity.R
test <- samps |>
  filter(sex %in% c(1, 2)) |>
  mutate(lengthgroup = ifelse(length >= 77 & length < 95.5 & sex == 2, "maturingf",
                              ifelse(length >= 65.1 & length < 76.7 & sex == 1 , "maturingm",
                                     ifelse(length >= 95.5 & sex == 2, "mf",
                                            ifelse(length >= 76.7 & sex == 1, "mm",
                                                   ifelse(length < 77 & sex == 2, "imm",
                                                          ifelse(length < 65.1 & sex == 1, "imm",
                                                                 NA))))))) |>
  group_by(year, lengthgroup, name) |>
  mutate(count =  n())

ggplot(filter(test, name != "DOGJhooks"), aes(year, count, group = name, colour = name)) +
  geom_line() +
  facet_wrap(~lengthgroup, scales = "free") + theme_classic() + geom_point()


# Mature female ratio -----------------------------------------------------

test2 <- test |>
  group_by(year, fishing_event_id, name) |>
  filter(lengthgroup != "immatures") |>
  mutate(sumall =  n())
test3 <- test |>
  group_by(year, name) |>
  filter(lengthgroup  == "mf") |>
  summarize(summf = n())
testmm <- test |>
  group_by(year, name) |>
  filter(lengthgroup  == "mm") |>
  summarize(summm = n())

final <- inner_join(test3, testmm, by = c("year", "name"))
final <- final |>
  mutate(ratio = summf/summm*100)
ggplot(final, aes(year, ratio, group = name)) + geom_point() + geom_line() + facet_wrap(~name) +
  ylab("Ratio (Mature females:Mature Males)")
