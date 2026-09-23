#figure of calibrated indicies
library(ggplot2)
library(tidyverse)


index <- readRDS(file = "data-generated/index_dogfish_depth_int.rds")


# sensitivity to calibration params ---------------------------------------

gg <-
  index %>%
  filter(id == "depth") |>
  ggplot(aes(year, est, ymin = lwr, ymax = upr, group = id)) +
  geom_ribbon(aes(year, est, ymin = lwr, ymax = upr ), fill = "grey90", colour  = NA, alpha = 0.25) +
  geom_line(aes(group = id), colour = "grey70", linewidth = 1, alpha = 0.25) +
  geom_point(aes(group = id), colour = "grey70", size = 2, alpha = 0.25) +
  #geom_linerange() +
  #facet_wrap(vars(Survey), ncol = 1, scales = "free_y") +
  expand_limits(y = 0) +
  guides(fill = "none", colour = "none") +
  #scale_colour_viridis_d() +
  #scale_fill_viridis_d() +
  labs(x = "Year", y = "Index") +
  theme_classic()
gg2 <- gg +
  geom_ribbon(data = filter(index, id == "int"), aes(year, est, ymin = lwr, ymax = upr), fill = "red", alpha = 0.05) +
  geom_line(data = filter(index, id == "int"), aes(year, est), colour = "red") +
  geom_point(data = filter(index, id == "int"), aes(year, est), colour = "red")

gg2
ggsave("figures/sensitivity_calibrated_index_depth_int.jpg", gg2, width = 4, height =4)




# compare indices ---------------------------------------------------------

index_compare <- rbind(   #seasonally paired value and center data
  index_mean |> mutate(Survey = "Calibrated HBLL & SoG dogfish") |>
    mutate(est_c = scale(est, center = TRUE, scale = TRUE), lwr_c = (lwr - mean(est))/sd(est), upr_c = (upr - mean(est))/sd(est)),
  index_hbll |> mutate(Survey = "HBLL") |>
    mutate(est_c = scale(est, center = TRUE, scale = TRUE), lwr_c = (lwr - mean(est))/sd(est), upr_c = (upr - mean(est))/sd(est)),
  #index_dog |> mutate(Survey = "SoG dogfish (circle)") |>
  #  mutate(est_c = scale(est, center = TRUE, scale = TRUE), lwr_c = (lwr - mean(est))/sd(est), upr_c = (upr - mean(est))/sd(est)),
  index_dc |> mutate(Survey = "Calibrated SoG dogfish circle & Jhook") |>
    mutate(est_c = scale(est, center = TRUE, scale = TRUE), lwr_c = (lwr - mean(est))/sd(est), upr_c = (upr - mean(est))/sd(est))

)

x <- palette.colors(palette = "Okabe-Ito")

gg <- index_compare %>%
  # mutate(dyear = year %in% year_dogfish) %>%
  #ggplot(aes(year, est, ymin = lwr, ymax = upr, group = Survey, colour = Survey)) +
  ggplot(aes(year, est_c, ymin = lwr_c, ymax = upr_c, group = Survey, colour = Survey)) +
  geom_line(aes(group = Survey, colour = Survey), linewidth = 1) +
  geom_point(aes(group = Survey, colour = Survey), size = 2) +
  geom_ribbon(aes(year, est_c, ymin = lwr_c, ymax = upr_c, fill = Survey), alpha = 0.5, guides = NULL) +
  #geom_linerange() +
  #facet_wrap(vars(Survey), ncol = 1, scales = "free_y") +
  expand_limits(y = 0) +
  scale_colour_manual(values = x[c(2,4,7)]) +
  scale_fill_manual(values = x[c(2,4,7)]) +
  labs(x = "Year", y = "Index") +
  theme_classic()

ggsave("figures/calibrated_index.jpg", gg, width = 6, height =3)

