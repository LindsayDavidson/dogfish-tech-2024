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


cpue <- ggplot() +
  geom_point(data = pairedw, aes(`Dog 14/0`, `HBLL 13/0`, colour = survey_abbrev), size = 2) +
  geom_abline() +
  scale_colour_viridis_d() +
  theme_classic() +
  xlab("CPUE of Dogfish catch (Dogfish hooks)") +
  ylab("CPUE of Dogfish catch (HBLL hooks)") +
  labs(colour = "Survey")



# length catch rate plot --------------------------------------------------

samps <- readRDS("output/samps_joined.rds")
glimpse(samps)
samps <- samps |>
  filter(year != 2019) |>
  mutate(hook_name = ifelse(hooksize_desc == "13/0", "HBLL_13/0", "Dog_14/0")) |>
  dplyr::select(hook_name, sex, fishing_event_id, year, length) #add lglsp_hook_count

id_remove <- paired  |>   pull(fishing_event_id)
samps <- filter(samps, fishing_event_id %in% id_remove) %>%
  arrange(year, fishing_event_id)

sampsm <- filter(samps, sex == 1)
sampsf <- filter(samps, sex == 2)

min <- min(sampsf$length, na.rm = TRUE)
max <- max(sampsf$length, na.rm = TRUE)

breaksf <- seq(min, max, by = 10)

test <- sampsf  |>  drop_na(length) |> group_by(hook_name, fishing_event_id) |> mutate(new_bin = cut(length, breaksf))
test2 <- test |> group_by(new_bin, hook_name, fishing_event_id) |> reframe(count = n())

min <- min(sampsm$length, na.rm = TRUE)
max <- max(sampsm$length, na.rm = TRUE)

breaksm <- seq(min, max, by = 10)

testm <- sampsm |>  group_by(hook_name, fishing_event_id) |> mutate(new_bin = cut(length, breaksm))
testm2 <- testm |> group_by(new_bin, hook_name, fishing_event_id) |> reframe(count = n())

testm3 <- testm2 |>
  pivot_wider(
    names_from = hook_name,
    values_from = count
  ) |>
  mutate(ratioDH = `Dog_14/0`/`HBLL_13/0`, sex = "1") |>
  drop_na(new_bin) |>
  ungroup()

testf3 <- test2 |>
  pivot_wider(
    names_from = hook_name,
    values_from = count
  ) |>
  mutate(ratioDH = `Dog_14/0`/`HBLL_13/0`, sex = "2") |>
  drop_na(new_bin) |>
  ungroup()

testfm <- rbind(testf3, testm3)

lengthbins <- ggplot(data = testfm, aes(new_bin, ratioDH)) +
  geom_jitter(size = 3) +
  geom_boxplot() +
  #geom_line()+
  theme_classic() +
  facet_wrap(~sex, scale = "free_x") +
  geom_hline(yintercept = 1) +
  xlab("Length bin") +
  ylab("Ratio of Dogfish/HBLL hook catches (paired)")


cv <- cowplot::plot_grid(
  cpue, lengthbins,
  ncol = 1,
  nrow = 2,
  labels = c("(a)", "(b)"),
  #align = "hv",
  rel_heights = c(1,1.5),
  rel_widths = rep(1)
)

cv

ggsave(paste0("figures/paired_sets_cpue_lengths.png"), cv, height = 7, width = 6, dpi = 200)

#pairedw <- pairedw |>  mutate(ratio = pairedw$`Dog 14/0` / pairedw$`HBLL 13/0`)


#########length and catch ratio plot across season######

#should I pair with sites and depths across HBLL and DOG??

samps <- readRDS("output/samps_joined.rds")
glimpse(samps)
samps <- samps |>
  filter(year != 2019) |>
  mutate(hook_name = ifelse(hooksize_desc == "13/0", "HBLL_13/0", "Dog_14/0")) |>
  dplyr::select(hook_name, sex, fishing_event_id, year, length, time_deployed, lglsp_hook_count) #add lglsp_hook_count

id_remove <- paired  |>   pull(fishing_event_id)

samps <- filter(samps, fishing_event_id %in% id_remove) %>%
  arrange(year, fishing_event_id)

samps <- samps |> mutate(survey_abbrev = ifelse(time_deployed > "2023-09-06 09:15:21", "dog", "hbll"))

#compare hbll hook catches on hbll survey to dog hook catch on dog survey
sampsdog <- samps |> filter(hook_name == "Dog_14/0" & survey_abbrev == "dog")
sampshbll <- samps |> filter(hook_name == "HBLL_13/0" & survey_abbrev == "hbll")

samps <- rbind(sampsdog, sampshbll)

sampsm <- filter(samps, sex == 1)
sampsf <- filter(samps, sex == 2)

min <- min(sampsf$length, na.rm = TRUE)
max <- max(sampsf$length, na.rm = TRUE)

breaksf <- seq(min, max, by = 10)

# test <- sampsf  |>  drop_na(length) |> group_by(hook_name, fishing_event_id) |> mutate(new_bin = cut(length, breaksf))
# test2 <- test |> group_by(new_bin, hook_name) |> reframe(count = n())

testf <- sampsf |>  group_by(hook_name) |> mutate(new_bin = cut(length, breaksf))
testf2 <- testf |> group_by(new_bin, hook_name) |> reframe(count = n(), meanhooks = mean(lglsp_hook_count)) |> mutate(cpue = count/meanhooks) |>
  dplyr::select(-count, -meanhooks)

min <- min(sampsm$length, na.rm = TRUE)
max <- max(sampsm$length, na.rm = TRUE)

breaksm <- seq(min, max, by = 10)

testm <- sampsm |>  group_by(hook_name) |> mutate(new_bin = cut(length, breaksm))
testm2 <- testm |> group_by(new_bin, hook_name) |> reframe(count = n(), meanhooks = mean(lglsp_hook_count)) |> mutate(cpue = count/meanhooks) |>
  dplyr::select(-count, -meanhooks)

testm3 <- testm2 |>
  pivot_wider(
    names_from = hook_name,
    values_from = cpue
  ) |>
  #dplyr::select(-fishing_event_id) |>
  mutate(ratioDH = `Dog_14/0`/`HBLL_13/0`, sex = "1") |>
  drop_na(new_bin) |>
  ungroup()

testf3 <- testf2 |>
  pivot_wider(
    names_from = hook_name,
    values_from = cpue
  ) |>
  mutate(ratioDH = `Dog_14/0`/`HBLL_13/0`, sex = "2") |>
  drop_na(new_bin) |>
  ungroup()

testfm <- rbind(testf3, testm3)

x <- ggplot(data = testfm, aes(new_bin, ratioDH)) +
  geom_jitter(size = 3) +
  #geom_line()+
  theme_classic() +
  facet_wrap(~sex, scale = "free_x") +
  geom_hline(yintercept = 1) +
  xlab("Length bin") +
  ylab("Ratio of Dogfish/HBLL cpue in paired comparative sets")

ggsave(paste0("figures/season_sets_cpue_lengths.png"), x, height = 4, width = 5, dpi = 200)

#pairedw <- pairedw |>  mutate(ratio = pairedw$`Dog 14/0` / pairedw$`HBLL 13/0`)

