
# load data ---------------------------------------------------------------
samps <- readRDS("output/dogfish_samps.rds")
hbllsamps <- readRDS("output/samples-hbll-dog.rds")


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

