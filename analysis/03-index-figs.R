# cols <- c("#d7191c", "#fdae61", "#2c6184", "#2c7bb6")

# first figure
index2 <- readRDS(file = paste0("output/ind-sog-depth", "hbll-n", ".rds"))
index3 <- readRDS(file = paste0("output/ind-sog-depth", "hbll-s", ".rds"))
index4 <- readRDS(file = paste0("output/ind-sog-depth", "dog", ".rds"))
ind <- bind_rows(index2, index3, index4)
ind$group <- paste0(ind$modelloc, ind$type)
ggplot(ind, aes(year, (est), ymin = (lwr), ymax = (upr), colour = model)) +
  geom_pointrange(data = ind, mapping = aes(x = year - 0.25), size = 0.2, pch = 5, alpha = 0.6, position = position_dodge(width = 0.5)) +
  theme_classic() +
  scale_colour_manual(values = c("grey", "black")) +
  facet_wrap(~group, scales = "free")

# second figure
index <- readRDS(file = paste0("output/ind-sog-depth", "hblldog_no2004", ".rds"))
index1 <- readRDS(file = paste0("output/ind-sog-depth", "hbll-n-s", ".rds"))
ind <- bind_rows(index, index1)
ind$group <- paste0(ind$modelloc, ind$type)
ggplot(ind, aes(year, (est), ymin = (lwr), ymax = (upr), colour = model)) +
  geom_pointrange(data = ind, mapping = aes(x = year - 0.25), size = 0.2, pch = 5, alpha = 0.6, position = position_dodge(width = 0.5)) +
  theme_classic() +
  scale_colour_manual(values = c("grey", "black")) +
  facet_wrap(~group, scales = "free_y") +
  coord_cartesian(ylim = c(0,800))
ggsave("Figures/stitched_index.jpg", width = 8, height = 5)

ggplot(ind, aes(year, log(est), ymin = log(lwr), ymax = log(upr), colour = group)) +
  geom_pointrange(data = ind, mapping = aes(x = year - 0.25), size = 0.2, pch = 5, alpha = 0.6, position = position_dodge(width = 0.75)) +
  theme_classic() +
  scale_colour_manual(values = c("black", "red"))
ggsave("Figures/stitched_index_combined.jpg", width = 8, height = 5)


# int and depth comp models
indint <- readRDS(file = paste0("output/ind-sog-intonly", "hblldog_no2004", ".rds"))
indint$type <- "int"
index <- readRDS(file = paste0("output/ind-sog-depth", "hblldog_no2004", ".rds"))
index$type <- "depth"
ind <- bind_rows(index, indint)
ggplot(ind, aes(year, log(est), ymin = log(lwr), ymax = log(upr), group = type, colour = type)) +
  geom_pointrange(data = filter(ind, model == "yrs_surved"), mapping = aes(x = year - 0.25), size = 0.2, pch = 5, alpha = 0.6, position = position_dodge(width = 0.5)) +
  theme_classic()

# int and depth comp models
indint <- readRDS(file = paste0("output/ind-sog-intonly", "hbll-s", ".rds"))
indint$type <- "int"
index <- readRDS(file = paste0("output/ind-sog-depth", "hbll-s", ".rds"))
index$type <- "depth"
ind <- bind_rows(index, indint)
ggplot(ind, aes(year, log(est), ymin = log(lwr), ymax = log(upr), group = type, colour = type)) +
  geom_pointrange(data = filter(ind, model == "yrs_surved"), mapping = aes(x = year - 0.25), size = 0.2, pch = 5, alpha = 0.6, position = position_dodge(width = 0.5)) +
  theme_classic()

# all int models
index <- readRDS(file = paste0("output/ind-sog-intonly", "hblldog_no2004", ".rds"))
index1 <- readRDS(file = paste0("output/ind-sog-intonly", "hbll-n-s", ".rds"))
index2 <- readRDS(file = paste0("output/ind-sog-intonly", "hbll-n", ".rds"))
index3 <- readRDS(file = paste0("output/ind-sog-intonly", "hbll-s", ".rds"))
index4 <- readRDS(file = paste0("output/ind-sog-intonly", "dog", ".rds"))
ind <- bind_rows(index, index1, index2, index3, index4)
ind$group <- paste0(ind$modelloc, ind$type)
ggplot(ind, aes(year, log(est), ymin = log(lwr), ymax = log(upr), group = group, colour = group)) +
  geom_pointrange(data = filter(ind, model == "yrs_surved"), mapping = aes(x = year - 0.25), size = 0.2, pch = 5, alpha = 0.6, position = position_dodge(width = 0.5)) +
  theme_classic() +
  scale_colour_viridis_d() +
  facet_wrap(~group)

ggplot(ind, aes(year, log(est), ymin = log(lwr), ymax = log(upr), group = group)) +
  geom_pointrange(data = filter(ind, model == "yrs_surved"), mapping = aes(x = year - 0.25), size = 0.2, pch = 5, alpha = 0.6, position = position_dodge(width = 0.5)) +
  theme_classic() +
  scale_colour_viridis_d() +
  facet_wrap(~group)


# all depth models
index <- readRDS(file = paste0("output/ind-sog-depth", "hblldog_no2004", ".rds"))
index1 <- readRDS(file = paste0("output/ind-sog-depth", "hbll-n-s", ".rds"))
index2 <- readRDS(file = paste0("output/ind-sog-depth", "hbll-n", ".rds"))
index3 <- readRDS(file = paste0("output/ind-sog-depth", "hbll-s", ".rds"))
index4 <- readRDS(file = paste0("output/ind-sog-depth", "dog", ".rds"))
ind <- bind_rows(index, index1, index2, index3, index4)
ind$group <- paste0(ind$modelloc, ind$type)
ggplot(ind, aes(year, log(est), ymin = log(lwr), ymax = log(upr), group = group, colour = group)) +
  geom_pointrange(data = filter(ind, model == "yrs_surved"), mapping = aes(x = year - 0.25), size = 0.2, pch = 5, alpha = 0.6, position = position_dodge(width = 0.5)) +
  theme_classic() +
  scale_colour_viridis_d() +
  facet_wrap(~group)
ggplot(ind, aes(year, log(est), ymin = log(lwr), ymax = log(upr), group = group)) +
  geom_pointrange(data = filter(ind, model == "yrs_surved"), mapping = aes(x = year - 0.25), size = 0.2, pch = 5, alpha = 0.6, position = position_dodge(width = 0.5)) +
  theme_classic() +
  scale_colour_viridis_d() +
  facet_wrap(~group)

ggplot(index3, aes(year, (est), ymin = (lwr), ymax = (upr))) +
  geom_pointrange(data = filter(ind, model == "yrs_surved"), mapping = aes(x = year - 0.25), size = 0.2, pch = 5, alpha = 0.6, position = position_dodge(width = 0.5)) +
  theme_classic()

ggplot(index1, aes(year, (est), ymin = (lwr), ymax = (upr))) +
  geom_pointrange(data = filter(index1, model == "yrs_surved"), mapping = aes(x = year - 0.25), size = 0.2, pch = 5, alpha = 0.6, position = position_dodge(width = 0.5)) +
  theme_classic()
ggplot(index1, aes(year, (est), ymin = (lwr), ymax = (upr), colour = model)) +
  geom_pointrange(data = index1, mapping = aes(x = year - 0.25), size = 0.2, pch = 5, alpha = 0.6, position = position_dodge(width = 0.5)) +
  theme_classic() +
  scale_colour_manual(values = c("grey", "black"))



# index <- bind_rows(i_hblldog, i_hbllns, i_hbllsdog)

index <- index |>
  group_by(type) |>
  mutate(geomean = est) |>
  mutate(
    estc = est,
    lwrc = lwr,
    uprc = upr
  )

# index <- index |>
#   group_by(type) |>
#   mutate(geomean = mean(est)) |>
#   mutate(
#     estc = est- geomean,
#     lwrc = lwr-geomean,
#     uprc = upr-geomean
#   )

# figure code -------------------------------------------------------------
ggplot(
  data = filter(index, type == "hbll-n-s"),
  aes(year, est, ymin = (lwr), ymax = (upr))
) +
  # ggplot(data = filter(index, type == "hbll-n-s"), aes(year, est)) +
  geom_point(aes(colour = model)) +
  geom_line() +
  geom_pointrange(aes(x = year - 0.25), size = 0.2, pch = 5, alpha = 0.6) +
  theme_classic() +
  scale_x_continuous(breaks = c(years)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5)) +
  scale_colour_manual(values = c("#d8b365", "#5ab4ac", "#d7191c"))

ggplot(data = filter(index, model == "yrs_surved"), aes(year, (est), colour = type, fill = model)) +
  geom_point() +
  geom_line() +
  # geom_pointrange(aes(x = year - 0.25), size = 0.2, pch = 5, alpha = 0.6) +
  theme_classic() +
  scale_x_continuous(breaks = c(years)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5)) +
  scale_colour_manual(values = c("#d8b365", "#5ab4ac", "#d7191c"))
ggsave("Figures/index_withyrsinterpreted.jpg", width = 5, height = 4)

ggplot(
  data =
    filter(index, model == "yrs_surved" & type %in% c("all_no2004", "hbll-n-s")),
  # index,
  aes(year, log(est), ymin = log(lwr), ymax = log(upr), colour = type, fill = model)
) +
  geom_pointrange(aes(x = year - 0.25),
    size = 0.2, pch = 5, alpha = 0.6,
    position = position_dodge(width = 1), size = 1.5
  ) +
  # geom_ribbon(aes(ymin = log(lwr), ymax = log(upr)), alpha = 0.4) +
  theme_classic() +
  scale_x_continuous(breaks = c(years)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5))

ggplot(
  data =
    filter(index, model == "yrs_surved"),
  # index,
  aes(year, (est), ymin = (lwr), ymax = (upr), colour = type, fill = model)
) +
  geom_pointrange(aes(x = year - 0.25),
    size = 0.2, pch = 5, alpha = 0.6,
    position = position_dodge(width = 1), size = 1.5
  ) +
  # geom_ribbon(aes(ymin = log(lwr), ymax = log(upr)), alpha = 0.4) +
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
