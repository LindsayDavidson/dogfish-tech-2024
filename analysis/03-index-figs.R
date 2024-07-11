i_hblldog <- readRDS("output/ind-sog-hblldog.rds")|> mutate(type = "all") #noth n and s hbll and dog
yearlabs <- as.list(i_hblldog |> filter(model == "yrs_surved") |> reframe(year = year))
yearlabs <- yearlabs$year

i_hblln <- readRDS("output/ind-sog-hbll-n.rds") |> mutate(type = "hblln")

i_hblls<- readRDS("output/ind-sog-hbll-s.rds") |> mutate(type = "hblls")

i_hbllsdog <- readRDS("output/ind-sog-dog-hblls.rds") |> mutate(type = "hblls_dog") #hbll s and dog

index <- bind_rows(i_hblldog, i_hblln, i_hblls, i_hbllsdog)
index <- index |>
  group_by(type) |>
  mutate(geomean = log(mean(log(index$est)))) |>
  mutate(est = est / geomean,
         lwr = lwr / geomean,
         upr = upr / geomean
  )

# figure code -------------------------------------------------------------


index |>
  filter(model == "yrs_surved") |>
  ggplot(aes(year, log(est), group = type, col = type, fill = type)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = log(lwr), ymax = log(upr)), alpha = 0.4) +
  theme_classic() +
  scale_x_continuous(breaks = c(years))

index |>
  filter(model == "yrs_surved") |>
  ggplot(aes(year, (est), ymin = (lwr), ymax = (upr), colour = type, fill = type)) +
  geom_pointrange(aes(x = year - 0.25), size = 0.2, pch = 5, alpha = 0.6) +
  theme_classic() +
  scale_x_continuous(breaks = c(yearlabs)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~type, scales = "free")

index |>
  filter(model == "yrs_surved") |>
  ggplot(aes(year, log(est), ymin = log(lwr), ymax = log(upr), colour = type, fill = type)) +
  geom_pointrange(aes(x = year - 0.25), size = 0.2, pch = 5, alpha = 0.6) +
  theme_classic() +
  scale_x_continuous(breaks = c(yearlabs)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

index |>
  filter(model == "yrs_surved") |>
  ggplot(aes(year, (est), ymin = (lwr), ymax = (upr), colour = type, fill = type)) +
  geom_pointrange(aes(x = year - 0.25), size = 0.2, pch = 5, alpha = 0.6) +
  theme_classic() +
  scale_x_continuous(breaks = c(yearlabs)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
