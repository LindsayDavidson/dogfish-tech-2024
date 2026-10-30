# combined indices and centered and scale indices figures
library(tidyverse)
library(ggplot2)

index1 <- readRDS(file = paste0("output/ind-sog-intonly", "hblldog", ".rds")) |>
  mutate(mean = mean(est), sd = sd(est)) |>
  mutate(est_c = (est - mean) / sd, lwr_c = (lwr - mean) / sd, upr_c = (upr - mean) / sd)

index2 <- readRDS(file = paste0("output/ind-sog-intonly", "hbll-n-s", ".rds")) |>
  mutate(mean = mean(est), sd = sd(est)) |>
  mutate(est_c = (est - mean) / sd, lwr_c = (lwr - mean) / sd, upr_c = (upr - mean) / sd)

index3 <- readRDS(file = paste0("output/ind-sog-intonly", "dog-j-circle", ".rds")) |>
  mutate(mean = mean(est), sd = sd(est)) |>
  mutate(est_c = (est - mean) / sd, lwr_c = (lwr - mean) / sd, upr_c = (upr - mean) / sd)

ind <- bind_rows(index1, index2, index3)
ind$group <- paste0(ind$modelloc, ind$type)
ind$modelloc <- factor(ind$modelloc, levels = c("hblldog", "dog-j-circle", "hbll-n-s"))

x <- palette.colors(palette = "Okabe-Ito")

gg <- ind %>%
  # mutate(dyear = year %in% year_dogfish) %>%
  #ggplot(aes(year, est, ymin = lwr, ymax = upr, group = Survey, colour = Survey)) +
  ggplot(aes(year, est_c, ymin = lwr_c, ymax = upr_c, group = modelloc, colour = modelloc)) +
  geom_line(aes(group = modelloc, colour = modelloc), linewidth = 1) +
  geom_point(aes(group = modelloc, colour = modelloc), size = 2) +
  geom_ribbon(aes(year, est_c, ymin = lwr_c, ymax = upr_c, fill = modelloc), alpha = 0.5, guides = NULL) +
  #geom_linerange() +
  #facet_wrap(vars(Survey), ncol = 1, scales = "free_y") +
  expand_limits(y = 0) +
  scale_colour_manual(values = x[c(2,4,7)]) +
  scale_fill_manual(values = x[c(2,4,7)], labels=c("HBLL", "SoG dog circle/J-hook", "HBLL & SoG circle/J-hook")) +
  labs(x = "Year", y = "Abundance index") +
  theme_classic() +
  labs(fill = "Surveys incl.") +
  guides(colour = "none")

gg

ggsave("figures/stitched_indexes.jpg", gg, width = 5, height = 4)


gg2 <- ind %>%
  #filter(model == "yrs_surved") |>
  ggplot(aes(year, est_c, ymin = lwr_c, ymax = upr_c, group = modelloc, colour = modelloc)) +
  #geom_line(data = filter(ind, model == "yrs_surved"), aes(group = modelloc, colour = modelloc), linewidth = 1) +
  geom_point(data = filter(ind, model == "yrs_surved"), aes(group = modelloc, colour = modelloc), size = 2) +

  geom_point(data = filter(ind, model == "yrs_interp") , aes(group = modelloc), colour = "grey80", size = 2) +
  geom_pointrange(data = filter(ind, model == "yrs_interp"), position = position_dodge(width = 0.25), colour = "grey80") +

  geom_pointrange(data = filter(ind, model == "yrs_surved"), position = position_dodge(width = 0.25)) +
  theme_classic() +

  expand_limits(y = 0) +
  scale_fill_manual(values = x[c(2,4,7)]) +
  scale_colour_manual(values = x[c(2,4,7)]) +
  labs(x = "Year", y = "Abundance index") +
  facet_wrap(~modelloc,
             labeller = labeller(
               modelloc = c(
                       "hbll-n-s" = "HBLL",
                       "dog-j-circle" = "SoG dog circle/J-hook",
                       "hblldog" = "HBLL & SoG circle/J-hook"))) +
  geom_hline(yintercept = 0, colour = "grey90")+
  guides(colour = "none")

gg2

ggsave("figures/stitched_indexes_panel.jpg", gg2, width = 8, height = 4)
