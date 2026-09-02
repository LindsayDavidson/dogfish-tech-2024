write_csv(index_dog, file = "analysis/calibration/index/index_dog.csv")
#write_csv(index_dog, file = "analysis/calibration/index/index_dog_julian.csv")
write_csv(index_hbll, file = "analysis/calibration/index/index_hbll.csv")
write_csv(index_dc, file = "analysis/calibration/index/index_dogfish_jcirclehook_calibrate.csv")
write_csv(index, file = "analysis/calibration/index/index_dogfish_calibrate.csv")



# compare indices ---------------------------------------------------------


index_compare <- rbind(   #seasonally paired value and center data
  index |> mutate(Survey = "Calibrated HBLL + SoG dogfish") |>
    mutate(est_c = scale(est, center = TRUE, scale = TRUE), lwr_c = (lwr - mean(est))/sd(est), upr_c = (upr - mean(est))/sd(est)),
  index_hbll |> mutate(Survey = "HBLL") |>
    mutate(est_c = scale(est, center = TRUE, scale = TRUE), lwr_c = (lwr - mean(est))/sd(est), upr_c = (upr - mean(est))/sd(est)),
  #index_dog |> mutate(Survey = "SoG dogfish (circle)") |>
  #  mutate(est_c = scale(est, center = TRUE, scale = TRUE), lwr_c = (lwr - mean(est))/sd(est), upr_c = (upr - mean(est))/sd(est)),
  index_dc |> mutate(Survey = "Calibrated SoG dogfish") |>
    mutate(est_c = scale(est, center = TRUE, scale = TRUE), lwr_c = (lwr - mean(est))/sd(est), upr_c = (upr - mean(est))/sd(est))

)

# index_compare2 <- index_compare <- rbind(
#   index |> mutate(Survey = "Calibrated HBLL + SoG dogfish (circle and j-hook)", value = "1.19"), #paired value
#   index_hbll |> mutate(Survey = "HBLL", value = "1.19"),
#   index_dog |> mutate(Survey = "SoG dogfish", value = "1.19")
# )

#index_compare <- rbind(index_compare, index_compare2)

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

# index_compare <- rbind(
#   read.csv(file = "analysis/calibration/index/index_dogfish_hbll_calibrate.csv") %>%
#     mutate(Survey = "Calibrated HBLL + SoG dogfish"),
#   read.csv(file = "analysis/calibration/index/index_dog.csv") %>%
#     mutate(Survey = "SoG dogfish"),
#   read.csv(file = "analysis/calibration/index/index_hbll.csv") %>%
#     mutate(Survey = "HBLL")
# )
#
# year_dogfish <- index_compare %>%
#   filter(Survey == "SoG dogfish") %>%
#   pull(year)
#
# gg <- index_compare %>%
#   mutate(dyear = year %in% year_dogfish) %>%
#   ggplot(aes(year, est, ymin = lwr, ymax = upr, colour = dyear)) +
#   geom_point() +
#   geom_line(aes(group = Survey), linewidth = 0.1) +
#   geom_linerange() +
#   facet_wrap(vars(Survey), ncol = 1, scales = "free_y") +
#   expand_limits(y = 0) +
#   labs(x = "Year", y = "Index", colour = "Year with \nSoG dogfish survey?")
# ggsave("analysis/index-calibration/index/compare_index.png", g, height = 6, width = 5)
