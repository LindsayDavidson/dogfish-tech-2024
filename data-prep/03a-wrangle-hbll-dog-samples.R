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
# samps <- readRDS("output/dogfish_samps.rds")
hsamps <- readRDS("data-raw/samples-hbll-dog.rds") |>
  filter(!fishing_event_id %in% c(hbllrm$fishing_event_id))
range(hsamps$length)

dsamps <- readRDS("data-raw/dogfish_samples_cleaned.rds") |>
  filter(species_common_name == "NORTH PACIFIC SPINY DOGFISH") |>
  drop_na(total_length) |>
  filter(total_length > 0) |>
  mutate(total_length = total_length/10)
range(dsamps$total_length)


# set data (to remove fishing sets)
hbll <- readRDS("data-raw/hbllsets.rds")
final <- readRDS("data-generated/dogfish_sets_cleaned.rds") |>
  filter(species_code == "044") # pull just dogfish data not other species




# hbll clean up ------------------------------------------------------------

# remove two survey years that extended along the west coast VI
hbll1 <- filter(hbll, (latitude < 48.5 & longitude < -123.3)) # only two years have the sampling around the strait
hbll2 <- filter(hbll, (latitude < 48.75 & longitude < -124.25))
hbllrm <- bind_rows(hbll1, hbll2)
unique(hbllrm$fishing_event_id) # rm these

test <- filter(hbll, survey_abbrev == "HBLL INS S")
x <- ggplot(test) +
  geom_point(aes(longitude, latitude))
x + geom_point(data = hbllrm, aes(longitude, latitude), col = "red")


ggplot(test) +
  geom_point(aes(longitude, latitude)) + facet_wrap(~year)




# wrangle -----------------------------------------------------------------

hsamps <- hsamps |>
  filter(!fishing_event_id %in% c(hbllrm$fishing_event_id))

hsamps2 <- hsamps |> dplyr::select(
  survey_abbrev, year, month, species_common_name,
  sex, length, weight, fishing_event_id
)

dsamps <-
  dsamps |>
  mutate(
    date= as.Date(trip_start_date, format = "%Y-%m-%d"),
    julian = lubridate::yday(date),
    month = lubridate::month(date))


dsamps2 <- dsamps |>
  dplyr::select(
    survey, year, month, species_common_name,
    specimen_sex_code, total_length, round_weight, fishing_event_id
  ) |>
  rename(
    survey_abbrev = survey, sex = specimen_sex_code, length = total_length,
    weight = round_weight
  )

samps <- bind_rows(dsamps2, hsamps2)



# summary plot ------------------------------------------------------------
ggplot(samps, aes(year, length, col = survey_abbrev)) +
  geom_jitter()

ggplot(samps, aes(year, length, col = survey_abbrev)) +
  geom_jitter() +
  facet_wrap(~sex)







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

samps |>
  drop_na(weight) |>
  tally()
samps |> tally()
115952 - 6984 # lots of NAs for weight

# use maturity DFO of 55
# these legnths are from a length weight and length matuirty curve calculated in
# split-index_by_regionandmaturity.R
test <- samps |>
  filter(sex %in% c(1, 2)) |>
  mutate(lengthgroup = ifelse(length >= 77 & length < 95.5 & sex == 2, "maturingf",
    ifelse(length >= 65.1 & length < 76.7 & sex == 1, "maturingm",
      ifelse(length >= 95.5 & sex == 2, "mf",
        ifelse(length >= 76.7 & sex == 1, "mm",
          ifelse(length < 77 & sex == 2, "imm",
            ifelse(length < 65.1 & sex == 1, "imm",
              NA
            )
          )
        )
      )
    )
  )) |>
  group_by(year, lengthgroup, name) |>
  mutate(count = n())

ggplot(filter(test, name != "DOGJhooks"), aes(year, count, group = name, colour = name)) +
  geom_line() +
  facet_wrap(~lengthgroup, scales = "free") +
  theme_classic() +
  geom_point()


# Mature female ratio -----------------------------------------------------

test2 <- test |>
  group_by(year, fishing_event_id, name) |>
  filter(lengthgroup != "immatures") |>
  mutate(sumall = n())
test3 <- test |>
  group_by(year, name) |>
  filter(lengthgroup == "mf") |>
  summarize(summf = n())
testmm <- test |>
  group_by(year, name) |>
  filter(lengthgroup == "mm") |>
  summarize(summm = n())

final <- inner_join(test3, testmm, by = c("year", "name"))
final <- final |>
  mutate(ratio = summf / summm * 100)
ggplot(final, aes(year, ratio, group = name)) +
  geom_point() +
  geom_line() +
  facet_wrap(~name) +
  ylab("Ratio (Mature females:Mature Males)")
