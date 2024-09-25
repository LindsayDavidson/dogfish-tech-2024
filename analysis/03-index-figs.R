cols <- c("#d7191c", "#fdae61", "#2c6184", "#2c7bb6")


i_hblldog <- readRDS(file = "output/ind-sog-hblldog_no2004.rds") |> mutate(type = "all_no2004")
i_hbllns <- readRDS("output/ind-sog-hbll-n-s.rds") |> mutate(type = "hbll-n-s")
i_hbllsdog <- readRDS("output/ind-sog-dog-circle.rds") |> mutate(type = "circle") # hbll s and dog


i_hblldog <- readRDS(file = "output/ind-sog-hblldog_no2004.rds") |> mutate(type = "all_no2004")
i_hblldogjul_int <- readRDS(file = "output/ind-sog-hblldog_no2004-depthbin-julian-int.rds") |> mutate(type = "all_no2004_julian_int")
#i_hblldogjul <- readRDS(file = "output/ind-sog-hblldog_no2004-julian.rds") |> mutate(type = "all_no2004_julian")
i_hblldogmonth_int <- readRDS(file = "output/ind-sog-hblldog_no2004-month-interaction.rds") |> mutate(type = "all_no2004_month_int")

index <- bind_rows(i_hblldog, i_hblldogjul_int, i_hblldogmonth_int)

index <- bind_rows(i_hblldog, i_hbllns, i_hbllsdog)

index <- index |>
  group_by(type) |>
  mutate(geomean = mean(est)) |>
  mutate(
    estc = est- geomean,
    lwrc = lwr-geomean,
    uprc = upr-geomean
  )

# figure code -------------------------------------------------------------
ggplot(data = filter(index, type == "hbll-n-s"), aes(year, est)) +
  geom_point(aes(colour = model)) +
  geom_line() +
  #geom_pointrange(aes(x = year - 0.25), size = 0.2, pch = 5, alpha = 0.6) +
  theme_classic() +
  scale_x_continuous(breaks = c(years)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5)) +
  scale_colour_manual(values = c("#d8b365", "#5ab4ac", "#d7191c"))

ggplot(data = filter(index, model == "yrs_surved"), aes(year, (est), colour = type, fill = model)) +
  geom_point() +
  geom_line() +
  #geom_pointrange(aes(x = year - 0.25), size = 0.2, pch = 5, alpha = 0.6) +
  theme_classic() +
  scale_x_continuous(breaks = c(years)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5)) +
  scale_colour_manual(values = c("#d8b365", "#5ab4ac", "#d7191c"))
ggsave("Figures/index_withyrsinterpreted.jpg", width = 5, height = 4)

ggplot(data =
       filter(index, model == "yrs_surved"),
       #index,
       aes(year, (est), ymin = (lwr), ymax = (upr), colour = type, fill = model)) +
  geom_pointrange(aes(x = year - 0.25), size = 0.2, pch = 5, alpha = 0.6) +
  #geom_ribbon(aes(ymin = log(lwr), ymax = log(upr)), alpha = 0.4) +
  theme_classic() +
  scale_x_continuous(breaks = c(years)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5)) +
  scale_colour_manual(values = c("#d8b365", "#5ab4ac", "#d7191c"))
ggsave("Figures/index_withyrsinterpreted.jpg", width = 5, height = 4)


test <- index |>
  filter(type %in% c("all", "all_no2004")) |>
  filter(model == "yrs_surved")

ggplot(data = test, aes(year, log(est), ymin = log(lwr), ymax = log(upr), colour = type, fill = type)) +
  # geom_line() +
  geom_pointrange(aes(x = year - 0.25), size = 0.2, pch = 5) +
  # geom_ribbon(aes(ymin = log(lwr), ymax = log(upr)), alpha = 0.4) +
  theme_classic() +
  scale_colour_manual(values = c("#d8b365", "#5ab4ac")) +
  scale_x_continuous(breaks = c(yearlabs)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5))
ggsave("Figures/index_without2004.jpg", width = 5, height = 4)

test <- index |>
  filter(type %in% c("all_no2004", "all_no2004_month")) |>
  filter(model == "yrs_surved")

ggplot(data = test, aes(year, log(est), ymin = log(lwr), ymax = log(upr), colour = type, fill = type)) +
  # geom_line() +
  geom_pointrange(aes(x = year - 0.25), size = 0.2, pch = 5) +
  # geom_ribbon(aes(ymin = log(lwr), ymax = log(upr)), alpha = 0.4) +
  theme_classic() +
  scale_colour_manual(values = c("#d8b365", "#5ab4ac")) +
  scale_x_continuous(breaks = c(yearlabs)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5))
ggsave("Figures/index_without2004_withmonth.jpg", width = 5, height = 4)

index |>
  filter(model == "yrs_surved") |>
  ggplot(aes(year, log(est), group = type, col = type, fill = type)) +
  geom_line() +
  geom_point() +
  theme_classic() +
  geom_ribbon(aes(ymin = log(lwr), ymax = log(upr)), alpha = 0.4) +
  theme_classic() +
  scale_x_continuous(breaks = c(years))

index |>
  filter(model == "yrs_surved") |>
  ggplot(aes(year, log(est), ymin = log(lwr), ymax = log(upr), colour = type, fill = type)) +
  geom_pointrange(aes(x = year - 0.25), size = 0.2, pch = 5, alpha = 0.6) +
  theme_classic() +
  scale_x_continuous(breaks = c(yearlabs)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(colour = guide_legend(title = "Survey")) +
  guides(fill = FALSE) +
  scale_color_manual(values = cols)

index |>
  filter(model == "yrs_surved") |>
  ggplot(aes(year, (est), ymin = (lwr), ymax = (upr), colour = type, fill = type)) +
  geom_pointrange(aes(x = year - 0.25), size = 0.2, pch = 5, alpha = 0.6) +
  theme_classic() +
  scale_x_continuous(breaks = c(yearlabs)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(colour = guide_legend(title = "Survey")) +
  guides(fill = FALSE) +
  scale_color_manual(values = cols)

index |>
  filter(model == "yrs_surved") |>
  ggplot(aes(year, (est), ymin = (lwr), ymax = (upr), colour = type, fill = type)) +
  geom_pointrange(aes(x = year - 0.25), size = 0.2, pch = 5, alpha = 0.6) +
  theme_classic() +
  scale_x_continuous(breaks = c(yearlabs)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~type, scales = "free_y") +
  guides(colour = guide_legend(title = "Survey")) +
  guides(fill = FALSE) +
  scale_color_manual(values = cols)
ggsave("Figures/index_comparison.jpg", width = 6, height = 3)
