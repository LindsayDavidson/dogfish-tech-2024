# combined indicies and centered and scale indices figures


# index1 <- readRDS(file = paste0("output/ind-sog-intonly", "hblldog_no2004", ".rds")) |>
#   mutate(mean = mean(est), sd = sd(est)) |>
#   mutate(est_c = (est - mean) / sd, lwr_c = (lwr - mean) / sd, upr_c = (upr - mean) / sd)

index2 <- readRDS(file = paste0("output/ind-sog-intonly", "hbll-n-s", ".rds")) |>
  mutate(mean = mean(est), sd = sd(est)) |>
  mutate(est_c = (est - mean) / sd, lwr_c = (lwr - mean) / sd, upr_c = (upr - mean) / sd)

index3 <- readRDS(file = paste0("output/ind-sog-intonly", "dog-predict", ".rds")) |>
  mutate(mean = mean(est), sd = sd(est)) |>
  mutate(est_c = (est - mean) / sd, lwr_c = (lwr - mean) / sd, upr_c = (upr - mean) / sd)

ind <- index3 |> filter(year %in% c(2005, 2023))
ind |>
  mutate(diff = (ind[2,2] - ind[1,2])/ind[1,2] * 100)

index4 <- readRDS(file = paste0("output/ind-sog-intonly", "dog", ".rds")) |>
  mutate(mean = mean(est), sd = sd(est)) |>
  mutate(est_c = (est - mean) / sd, lwr_c = (lwr - mean) / sd, upr_c = (upr - mean) / sd)

ind <- index2 |> filter(year %in% c(2005, 2023))
ind |>
  mutate(diff = (ind[2,2] - ind[1,2])/ind[1,2] * 100)

ind <- index2 |> filter(year %in% c(1986, 2023))
ind |>
  mutate(diff = (ind[2,2] - ind[1,2])/ind[1,2] * 100)

ind <- bind_rows(index2, index3, index4)
ind$group <- paste0(ind$modelloc, ind$type)

unique(ind$modelloc)

annotations <- data.frame(
  modelloc = c("hbll-n-s" ,   "dog-predict", "dog" ),
  label = c("HBLL", "Dog circle", "Dog circle and J-hook"),
  x = 1995,
  y = 4
)

unique(ind$modelloc)
ind$modelloc = factor(ind$modelloc, levels = c("dog-predict", "dog",  "hbll-n-s"))

a <-
  ind |>
  #filter(model == "yrs_surved") |>
  ggplot(aes(year, (est_c), ymin = (lwr_c), ymax = (upr_c))) +
  geom_pointrange(data = filter(ind, model == "yrs_surved"), mapping = aes(x = year, fill = modelloc, colour = modelloc), size = 0.25, pch = 21, alpha = 0.6, position = position_dodge(width = 0.5)) +
  geom_pointrange(data = filter(ind, model == "yrs_interp"), mapping = aes(x = year), size = 0.25, pch = 21, alpha = 0.6, position = position_dodge(width = 0.5), fill = "grey80", colour = "grey80") +
  geom_line(mapping = aes(x = year, est_c, fill = modelloc, colour = modelloc), size = 0.5, alpha = 0.6) +
  geom_point(data = filter(ind, model == "yrs_surved"), aes(x = year, est_c, fill = modelloc, colour = modelloc), size = 1, pch = 21) +
  theme_classic() +
  theme(strip.text = element_blank() ) +
  facet_grid(rows = vars(modelloc), scales = "free") +
  labs(y = "Estimated abundance", x = "Year") +
  scale_colour_viridis_d(guide = NULL) +
  scale_fill_viridis_d(guide = NULL) +
  geom_text(
    data = annotations,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    color = "black",
    #fontface = "bold",
    size = 3
  )

a



annotations <- data.frame(
  modelloc = c("hbll-n-s" ,   "dog-predict" ),
  label = c("HBLL", "Dog circle"),
  x = 2005,
  y = -4
)

b <-
  ind |>
  filter(model == "yrs_surved") |>
  filter(modelloc %in% c("hbll-n-s", "dog-predict")) |>
  #filter(modelloc %in% c("hbll-n-s", "dog")) |>
  ggplot( ) +
  geom_line(aes(year, est_c, colour = group, fill = group), linewidth = 1, alpha = 0.4) +
  geom_ribbon(aes(year, est_c, ymin = lwr_c, ymax = upr_c, fill = group), alpha = 0.4, guides = NULL) +
  geom_point(aes(year, est_c, ymin = lwr_c, ymax = upr_c, colour = group), size = 1, alpha = 0.4, guides = NULL) +
  theme_classic() +
  theme(legend.position = c(0.2, 0.2),
        legend.key = element_rect(fill = NA),
        legend.background = element_rect(fill = NA, colour = NA)) +  # Remove legend box background) +
  labs(y = "Estimated abundance", x = "Year", fill = "", colour = " " , group = NULL) +
  scale_colour_viridis_d(labels = c("Dogfish circle", "HBLL")) +
  scale_fill_viridis_d(guide = NULL)

b

cv <- cowplot::plot_grid(
  b, NULL,
  ncol = 1,
  nrow = 2,
  #labels = c("(a)", "(b)"), # Labels for each plot
  # align = "hv",
  rel_heights = c(1, 1),
  rel_widths = rep(1)
)

cv2 <- cowplot::plot_grid(
  a, cv,
  ncol = 2,
  nrow = 1,
  labels = c("(a)", "(b)"), # Labels for each plot
  # align = "hv",
  rel_heights = c(2, 1),
  rel_widths = rep(1)
)

cv2
ggsave("figures/stitched_index_combined.jpg", width = 6, height = 6)


#
# # int and depth comp models
# indint <- readRDS(file = paste0("output/ind-sog-intonly", "hblldog_no2004", ".rds"))
# indint$type <- "int"
# index <- readRDS(file = paste0("output/ind-sog-depth", "hblldog_no2004", ".rds"))
# index$type <- "depth"
# ind <- bind_rows(index, indint)
# ggplot(ind, aes(year, log(est), ymin = log(lwr), ymax = log(upr), group = type, colour = type)) +
#   geom_pointrange(data = filter(ind, model == "yrs_surved"), mapping = aes(x = year - 0.25), size = 0.2, pch = 5, alpha = 0.6, position = position_dodge(width = 0.5)) +
#   theme_classic()
#
# # int and depth comp models
# indint <- readRDS(file = paste0("output/ind-sog-intonly", "hbll-s", ".rds"))
# indint$type <- "int"
# index <- readRDS(file = paste0("output/ind-sog-depth", "hbll-s", ".rds"))
# index$type <- "depth"
# ind <- bind_rows(index, indint)
# ggplot(ind, aes(year, log(est), ymin = log(lwr), ymax = log(upr), group = type, colour = type)) +
#   geom_pointrange(data = filter(ind, model == "yrs_surved"), mapping = aes(x = year - 0.25), size = 0.2, pch = 5, alpha = 0.6, position = position_dodge(width = 0.5)) +
#   theme_classic()
#
# # all int models
# index <- readRDS(file = paste0("output/ind-sog-intonly", "hblldog_no2004", ".rds"))
# index1 <- readRDS(file = paste0("output/ind-sog-intonly", "hbll-n-s", ".rds"))
#
# ggplot(index1, aes(year, (est), ymin = (lwr), ymax = (upr))) +
#   geom_pointrange(mapping = aes(x = year - 0.25), size = 0.2, pch = 5, alpha = 0.6, position = position_dodge(width = 0.5)) +
#   scale_colour_manual(values = c("#d8b365", "#5ab4ac")) +
#   theme_classic() +
#   ylab("Abundance estimate") +
#   xlab("Year") +
#   coord_cartesian(ylim = c(0, 375))
#
# ggplot(index1, aes(year, (est), ymin = (lwr), ymax = (upr), colour = model)) +
#   geom_pointrange(data = index1, mapping = aes(x = year - 0.25), size = 0.2, pch = 5, alpha = 0.6, position = position_dodge(width = 0.5)) +
#   theme_classic() +
#   scale_colour_manual(values = c("grey50", "black")) +
#   coord_cartesian(ylim = c(0, 375)) +
#   ylab("Abundance estimate") +
#   xlab("Year")
# ggsave("Figures/hbll-ins-n-s-figure.jpg", width = 4, height = 3)
#
#
# index2 <- readRDS(file = paste0("output/ind-sog-intonly", "hbll-n", ".rds"))
# index3 <- readRDS(file = paste0("output/ind-sog-intonly", "hbll-s", ".rds"))
# index4 <- readRDS(file = paste0("output/ind-sog-intonly", "dog", ".rds"))
# ind <- bind_rows(index, index1, index2, index3, index4)
# ind$group <- paste0(ind$modelloc, ind$type)
# ggplot(ind, aes(year, log(est), ymin = log(lwr), ymax = log(upr), group = group, colour = group)) +
#   geom_pointrange(data = filter(ind, model == "yrs_surved"), mapping = aes(x = year - 0.25), size = 0.2, pch = 5, alpha = 0.6, position = position_dodge(width = 0.5)) +
#   theme_classic() +
#   scale_colour_viridis_d() +
#   facet_wrap(~group)
#
# ggplot(ind, aes(year, log(est), ymin = log(lwr), ymax = log(upr), group = group)) +
#   geom_pointrange(data = filter(ind, model == "yrs_surved"), mapping = aes(x = year - 0.25), size = 0.2, pch = 5, alpha = 0.6, position = position_dodge(width = 0.5)) +
#   theme_classic() +
#   scale_colour_viridis_d() +
#   facet_wrap(~group)
#
#
# # all depth models
# index <- readRDS(file = paste0("output/ind-sog-depth", "hblldog_no2004", ".rds"))
# index1 <- readRDS(file = paste0("output/ind-sog-depth", "hbll-n-s", ".rds"))
# index2 <- readRDS(file = paste0("output/ind-sog-depth", "hbll-n", ".rds"))
# index3 <- readRDS(file = paste0("output/ind-sog-depth", "hbll-s", ".rds"))
# index4 <- readRDS(file = paste0("output/ind-sog-depth", "dog", ".rds"))
# ind <- bind_rows(index, index1, index2, index3, index4)
# ind$group <- paste0(ind$modelloc, ind$type)
# ggplot(ind, aes(year, log(est), ymin = log(lwr), ymax = log(upr), group = group, colour = group)) +
#   geom_pointrange(data = filter(ind, model == "yrs_surved"), mapping = aes(x = year - 0.25), size = 0.2, pch = 5, alpha = 0.6, position = position_dodge(width = 0.5)) +
#   theme_classic() +
#   scale_colour_viridis_d() +
#   facet_wrap(~group)
# ggplot(ind, aes(year, log(est), ymin = log(lwr), ymax = log(upr), group = group)) +
#   geom_pointrange(data = filter(ind, model == "yrs_surved"), mapping = aes(x = year - 0.25), size = 0.2, pch = 5, alpha = 0.6, position = position_dodge(width = 0.5)) +
#   theme_classic() +
#   scale_colour_viridis_d() +
#   facet_wrap(~group)
#
# ggplot(index3, aes(year, (est), ymin = (lwr), ymax = (upr))) +
#   geom_pointrange(data = filter(ind, model == "yrs_surved"), mapping = aes(x = year - 0.25), size = 0.2, pch = 5, alpha = 0.6, position = position_dodge(width = 0.5)) +
#   theme_classic()
#
# ggplot(index1, aes(year, (est), ymin = (lwr), ymax = (upr))) +
#   geom_pointrange(data = filter(index1, model == "yrs_surved"), mapping = aes(x = year - 0.25), size = 0.2, pch = 5, alpha = 0.6, position = position_dodge(width = 0.5)) +
#   theme_classic()
# ggplot(index1, aes(year, (est), ymin = (lwr), ymax = (upr), colour = model)) +
#   geom_pointrange(data = index1, mapping = aes(x = year - 0.25), size = 0.2, pch = 5, alpha = 0.6, position = position_dodge(width = 0.5)) +
#   theme_classic() +
#   scale_colour_manual(values = c("grey", "black"))
#
#
# # index <- bind_rows(i_hblldog, i_hbllns, i_hbllsdog)
#
# index <- index |>
#   group_by(type) |>
#   mutate(geomean = est) |>
#   mutate(
#     estc = est,
#     lwrc = lwr,
#     uprc = upr
#   )
#
# # index <- index |>
# #   group_by(type) |>
# #   mutate(geomean = mean(est)) |>
# #   mutate(
# #     estc = est- geomean,
# #     lwrc = lwr-geomean,
# #     uprc = upr-geomean
# #   )
#
# # figure code -------------------------------------------------------------
# ggplot(
#   data = filter(index, type == "hbll-n-s"),
#   aes(year, est, ymin = (lwr), ymax = (upr))
# ) +
#   # ggplot(data = filter(index, type == "hbll-n-s"), aes(year, est)) +
#   geom_point(aes(colour = model)) +
#   geom_line() +
#   geom_pointrange(aes(x = year - 0.25), size = 0.2, pch = 5, alpha = 0.6) +
#   theme_classic() +
#   scale_x_continuous(breaks = c(years)) +
#   theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5)) +
#   scale_colour_manual(values = c("#d8b365", "#5ab4ac", "#d7191c"))
#
# ggplot(data = filter(index, model == "yrs_surved"), aes(year, (est), colour = type, fill = model)) +
#   geom_point() +
#   geom_line() +
#   # geom_pointrange(aes(x = year - 0.25), size = 0.2, pch = 5, alpha = 0.6) +
#   theme_classic() +
#   scale_x_continuous(breaks = c(years)) +
#   theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5)) +
#   scale_colour_manual(values = c("#d8b365", "#5ab4ac", "#d7191c"))
# ggsave("Figures/index_withyrsinterpreted.jpg", width = 5, height = 4)
#
# ggplot(
#   data =
#     filter(index, model == "yrs_surved" & type %in% c("all_no2004", "hbll-n-s")),
#   # index,
#   aes(year, log(est), ymin = log(lwr), ymax = log(upr), colour = type, fill = model)
# ) +
#   geom_pointrange(aes(x = year - 0.25),
#     size = 0.2, pch = 5, alpha = 0.6,
#     position = position_dodge(width = 1), size = 1.5
#   ) +
#   # geom_ribbon(aes(ymin = log(lwr), ymax = log(upr)), alpha = 0.4) +
#   theme_classic() +
#   scale_x_continuous(breaks = c(years)) +
#   theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5))
#
# ggplot(
#   data =
#     filter(index, model == "yrs_surved"),
#   # index,
#   aes(year, (est), ymin = (lwr), ymax = (upr), colour = type, fill = model)
# ) +
#   geom_pointrange(aes(x = year - 0.25),
#     size = 0.2, pch = 5, alpha = 0.6,
#     position = position_dodge(width = 1), size = 1.5
#   ) +
#   # geom_ribbon(aes(ymin = log(lwr), ymax = log(upr)), alpha = 0.4) +
#   theme_classic() +
#   scale_x_continuous(breaks = c(years)) +
#   theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5)) +
#   scale_colour_manual(values = c("#d8b365", "#5ab4ac", "#d7191c"))
# ggsave("Figures/index_withyrsinterpreted.jpg", width = 5, height = 4)
#
#
# test <- index |>
#   filter(type %in% c("all", "all_no2004")) |>
#   filter(model == "yrs_surved")
#
# ggplot(data = test, aes(year, log(est), ymin = log(lwr), ymax = log(upr), colour = type, fill = type)) +
#   # geom_line() +
#   geom_pointrange(aes(x = year - 0.25), size = 0.2, pch = 5) +
#   # geom_ribbon(aes(ymin = log(lwr), ymax = log(upr)), alpha = 0.4) +
#   theme_classic() +
#   scale_colour_manual(values = c("#d8b365", "#5ab4ac")) +
#   scale_x_continuous(breaks = c(yearlabs)) +
#   theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5))
# ggsave("Figures/index_without2004.jpg", width = 5, height = 4)
#
# test <- index |>
#   filter(type %in% c("all_no2004", "all_no2004_month")) |>
#   filter(model == "yrs_surved")
#
# ggplot(data = test, aes(year, log(est), ymin = log(lwr), ymax = log(upr), colour = type, fill = type)) +
#   # geom_line() +
#   geom_pointrange(aes(x = year - 0.25), size = 0.2, pch = 5) +
#   # geom_ribbon(aes(ymin = log(lwr), ymax = log(upr)), alpha = 0.4) +
#   theme_classic() +
#   scale_colour_manual(values = c("#d8b365", "#5ab4ac")) +
#   scale_x_continuous(breaks = c(yearlabs)) +
#   theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5))
# ggsave("Figures/index_without2004_withmonth.jpg", width = 5, height = 4)
#
# index |>
#   filter(model == "yrs_surved") |>
#   ggplot(aes(year, log(est), group = type, col = type, fill = type)) +
#   geom_line() +
#   geom_point() +
#   theme_classic() +
#   geom_ribbon(aes(ymin = log(lwr), ymax = log(upr)), alpha = 0.4) +
#   theme_classic() +
#   scale_x_continuous(breaks = c(years))
#
# index |>
#   filter(model == "yrs_surved") |>
#   ggplot(aes(year, log(est), ymin = log(lwr), ymax = log(upr), colour = type, fill = type)) +
#   geom_pointrange(aes(x = year - 0.25), size = 0.2, pch = 5, alpha = 0.6) +
#   theme_classic() +
#   scale_x_continuous(breaks = c(yearlabs)) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   guides(colour = guide_legend(title = "Survey")) +
#   guides(fill = FALSE) +
#   scale_color_manual(values = cols)
#
# index |>
#   filter(model == "yrs_surved") |>
#   ggplot(aes(year, (est), ymin = (lwr), ymax = (upr), colour = type, fill = type)) +
#   geom_pointrange(aes(x = year - 0.25), size = 0.2, pch = 5, alpha = 0.6) +
#   theme_classic() +
#   scale_x_continuous(breaks = c(yearlabs)) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   guides(colour = guide_legend(title = "Survey")) +
#   guides(fill = FALSE) +
#   scale_color_manual(values = cols)
#
# index |>
#   filter(model == "yrs_surved") |>
#   ggplot(aes(year, (est), ymin = (lwr), ymax = (upr), colour = type, fill = type)) +
#   geom_pointrange(aes(x = year - 0.25), size = 0.2, pch = 5, alpha = 0.6) +
#   theme_classic() +
#   scale_x_continuous(breaks = c(yearlabs)) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   facet_wrap(~type, scales = "free_y") +
#   guides(colour = guide_legend(title = "Survey")) +
#   guides(fill = FALSE) +
#   scale_color_manual(values = cols)
# ggsave("Figures/index_comparison.jpg", width = 6, height = 3)
