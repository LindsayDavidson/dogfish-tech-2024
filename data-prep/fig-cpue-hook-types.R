#calibration coefficient

final <- readRDS("data-raw/wrangled-hbll-dog-sets.rds")  |> filter(year %in% c(2019, 2022, 2023)) |> filter(survey_abbrev == "OTHER")
samps <- readRDS("output/samps_joined.rds")

# Remove sets that compared J hook vs circle hook
id_remove <- final %>% filter(hooksize_desc == "12/0") %>% pull(fishing_event_id)
final <- filter(final, !fishing_event_id %in% id_remove) %>%
  arrange(year, fishing_event_id, survey_abbrev)

glimpse(final)

final <- final |> mutate(survey_abbrev = ifelse(time_deployed > "2023-09-06 09:15:21", "dog", "hbll"))

paired <- final |> dplyr::select(fishing_event_id, catch_count, year, hooksize_desc, offset, survey_abbrev) |>
  filter(year != 2019) |>
  mutate(hook_name = ifelse(hooksize_desc == "13/0", "HBLL 13/0", "Dog 14/0")) |>
  mutate(cpue = catch_count/offset) |> dplyr::select(-c(catch_count, year, offset, hooksize_desc))


pairedw <- paired |>
  pivot_wider(
    names_from = hook_name,
    values_from = c(cpue)
  )


gg <- ggplot() +
  geom_point(data = pairedw, aes(`Dog 14/0`, `HBLL 13/0`), size = 2) + #, colour = survey_abbrev color is pretty confusing
  geom_abline() +
  scale_colour_viridis_d() +
  theme_classic() +
  xlab("CPUE of Dogfish catch (Dogfish hooks)") +
  ylab("CPUE of Dogfish catch (HBLL hooks)") +
  labs(colour = "Survey")

ggsave("figures/paired_sets_cpue_lengths.jpg", width = 4, height = 4)
