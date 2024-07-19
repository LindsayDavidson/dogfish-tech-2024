cols <- c("#d7191c", "#fdae61", "#2c6184",  "#2c7bb6" )

i_hblldog <- readRDS("output/ind-sog-hblldog.rds")|> mutate(type = "all") #noth n and s hbll and dog
yearlabs <- as.list(i_hblldog |> filter(model == "yrs_surved") |> reframe(year = year))
yearlabs <- yearlabs$year

i_hblldogmonth <- readRDS(file = "output/ind-sog-hblldog-month.rds") |> mutate(type = "all_no2004_month")

i_hblldog_no2004 <- readRDS("output/ind-sog-hblldog_no2004.rds") |> mutate(type = "all_no2004")

i_hblln <- readRDS("output/ind-sog-hbll-n.rds") |> mutate(type = "hblln")

i_hblls<- readRDS("output/ind-sog-hbll-s.rds") |> mutate(type = "hblls")

i_hbllsdog <- readRDS("output/ind-sog-dog-hblls.rds") |> mutate(type = "hblls_dog") #hbll s and dog

index <- bind_rows(i_hblldog, i_hblldogmonth, i_hblldog_no2004, i_hblln, i_hblls, i_hbllsdog)

index <- index |>
  group_by(type) |>
  mutate(geomean = log(mean(log(index$est)))) |>
  mutate(est = est / geomean,
         lwr = lwr / geomean,
         upr = upr / geomean
  )

# figure code -------------------------------------------------------------

test <- index |>
  filter(type == "all")
ggplot(data = test, aes(year, log(est), ymin = log(lwr), ymax = log(upr), colour = model, fill = model)) +
  #geom_line() +
  geom_pointrange(aes(x = year - 0.25), size = 0.2, pch = 5, alpha = 0.6) +
    #geom_ribbon(aes(ymin = log(lwr), ymax = log(upr)), alpha = 0.4) +
  theme_classic() +
  scale_x_continuous(breaks = c(years)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
ggsave("Figures/index_withyrsinterpreted.jpg", width = 5, height = 4)


test <- index |>
  filter(type %in% c("all", "all_no2004")) |>
  filter(model == "yrs_surved")

ggplot(data = test, aes(year, log(est), ymin = log(lwr), ymax = log(upr), colour = type, fill = type)) +
  #geom_line() +
  geom_pointrange(aes(x = year - 0.25), size = 0.2, pch = 5) +
  #geom_ribbon(aes(ymin = log(lwr), ymax = log(upr)), alpha = 0.4) +
  theme_classic() +
  scale_colour_manual(values = c("#d8b365", "#5ab4ac")) +
  scale_x_continuous(breaks = c(yearlabs)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
ggsave("Figures/index_without2004.jpg", width = 5, height = 4)

test <- index |>
  filter(type %in% c("all_no2004", "all_no2004_month")) |>
  filter(model == "yrs_surved")

ggplot(data = test, aes(year, log(est), ymin = log(lwr), ymax = log(upr), colour = type, fill = type)) +
  #geom_line() +
  geom_pointrange(aes(x = year - 0.25), size = 0.2, pch = 5) +
  #geom_ribbon(aes(ymin = log(lwr), ymax = log(upr)), alpha = 0.4) +
  theme_classic() +
  scale_colour_manual(values = c("#d8b365", "#5ab4ac")) +
  scale_x_continuous(breaks = c(yearlabs)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
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
    guides(colour=guide_legend(title="Survey")) +
    guides(fill = FALSE) +
    scale_color_manual(values = cols)

index |>
  filter(model == "yrs_surved") |>
  ggplot(aes(year, (est), ymin = (lwr), ymax = (upr), colour = type, fill = type)) +
  geom_pointrange(aes(x = year - 0.25), size = 0.2, pch = 5, alpha = 0.6) +
  theme_classic() +
  scale_x_continuous(breaks = c(yearlabs)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(colour=guide_legend(title="Survey")) +
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
  guides(colour=guide_legend(title="Survey")) +
  guides(fill = FALSE) +
  scale_color_manual(values = cols)
ggsave("Figures/index_comparison.jpg", width = 6, height = 3)

