i_hblldog <- readRDS("output/ind-sog-hblldog.rds") #noth n and s hbll and dog
i_hblldog <- i_hblldog |> mutate(model = ifelse(year %in% unique(d$year), "yrs_surved", "yrs_interp"))
yearlabs <- as.list(i_hblldog |> filter(model == "yrs_surved") |> reframe(year = year))
yearlabs <- yearlabs$year

i_hblln <- readRDS("output/ind-sog-hbll-n.rds")
i_hblln <- i_hblln |> mutate(model = ifelse(year %in% unique(d$year), "yrs_surved", "yrs_interp"))
yearlabs <- as.list(i_hblln |> filter(model == "yrs_surved") |> reframe(year = year))
#yearlabs <- yearlabs$year

i_hblls<- readRDS("output/ind-sog-hbll-s.rds")
i_hblls <- i_hblls |> mutate(model = ifelse(year %in% unique(d$year), "yrs_surved", "yrs_interp"))
yearlabs <- as.list(i_hblls |> filter(model == "yrs_surved") |> reframe(year = year))
#yearlabs <- yearlabs$year

i_hbllsdog <- readRDS("output/ind-sog-dog-hblls.rds") #hbll s and dog
i_hbllsdog <- i_hbllsdog |> mutate(model = ifelse(year %in% unique(d$year), "yrs_surved", "yrs_interp"))
yearlabs <- as.list(i_hbllsdog |> filter(model == "yrs_surved") |> reframe(year = year))
#yearlabs <- yearlabs$year


# figure code -------------------------------------------------------------



ggplot(index, aes(year, est / 10000)) +
  geom_line(col = "#8D9999") +
  geom_point(col = "#8D9999") +
  geom_ribbon(aes(ymin = lwr / 10000, ymax = upr / 10000), alpha = 0.4, fill = "#8D9999") +
  theme_classic() +
  scale_x_continuous(breaks = c(years))

ggplot(index, aes(year, est, ymin = lwr, ymax = upr)) +
  # geom_pointrange(data = filter(index, model == "yrs_interp"), mapping = aes(x = year - 0.25), size = 0.2, pch = 5, colour = "grey40", alpha = 0.6) +
  geom_pointrange(data = filter(index, model == "yrs_surved"), mapping = aes(x = year - 0.25), size = 0.2, pch = 5, colour = "red", alpha = 0.6) +
  theme_classic() +
  scale_x_continuous(breaks = c(yearlabs)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

x <-
  ggplot(i_hblldog, aes(year, est, ymin = lwr, ymax = upr)) +
  # geom_pointrange(data = filter(index, model == "yrs_interp"), mapping = aes(x = year - 0.25), size = 0.2, pch = 5, colour = "grey40", alpha = 0.6) +
  geom_pointrange(data = filter(ind, model == "yrs_surved"), mapping = aes(x = year - 0.25), size = 0.2, pch = 5, colour = "red", alpha = 0.6) +
  theme_classic() +
  scale_x_continuous(breaks = c(yearlabs)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

y <- x + geom_pointrange(data = filter(i_hbllsdog, model == "yrs_surved"), mapping = aes(x = year - 0.25), size = 0.2, pch = 5, colour = "grey", alpha = 0.6) + theme_classic()
z <- y + geom_pointrange(data = filter(i_hblln, model == "yrs_surved"), mapping = aes(x = year - 0.25), size = 0.2, pch = 5, colour = "grey", alpha = 0.6) + theme_classic()
