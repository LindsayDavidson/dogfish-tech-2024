samps <- readRDS("data-raw/dogfish_samples_cleaned.rds")
glimpse(samps)
unique(samps$age_specimen_collected)
test <- filter(samps, age_specimen_collected == 1)
ggplot(test, aes(year, length, group = survey_abbrev, colour = survey_abbrev)) + geom_jitter()
unique(test$year)


d <- readRDS("data-raw/dogfish_samples_gfdata.rds")
unique(d$survey_abbrev)
unique(d$year)
unique(d$age_specimen_collected) #no dogfish ages in database despite having structures
d2 <- filter(d, age_specimen_collected == 1)
ggplot(d2, aes(year, age, group = survey_abbrev, colour = survey_abbrev)) + geom_jitter()
unique(test$year)