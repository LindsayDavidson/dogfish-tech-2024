# Length and catch composition differences between the DOG and HBLL surveys

# library -----------------------------------------------------------------
library(gfdata)
library(gfplot)
library(tidyverse)
library(here)
library(sdmTMB)
library(sf)
library(sp)


# load data ---------------------------------------------------------------

#samps <- readRDS("output/dogfish_samps.rds")
samps <- readRDS("data-raw/dogfish_samples_cleaned.rds")
hbllsamps <- readRDS("output/samples-hbll-dog.rds")


# filter data  -----------------------------------------------------------
#samps <- samps |> mutate(name = ifelse(year %in% c(1986, 1989), "DOGJhooks", survey_abbrev))
samps$julian
samps <- samps %>%
  mutate(dmy = lubridate::ymd(trip_start_date)) |>
  mutate(julian = lubridate::yday(dmy))



# length comp figure for each survey --------------------------------------

ggplot(samps, aes(length)) +
  geom_hist(aes(colour = as.factor(sex))) +
  facet_wrap(~survey_abbrev, ncol = 5)

gfsynopsis::plot_lengths




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
