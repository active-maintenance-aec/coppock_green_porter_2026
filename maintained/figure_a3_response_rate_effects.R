# coppock_green_porter_2026/maintained/figure_a3_response_rate_effects.R
# Output: output/figure_a3_response_rate_effects.{pdf,png}, output/estimates_response_rate_effects.csv
# Depends on: original/clean_survey_experiments.rds, helpers.R
# Description: The effect of each number of treatment videos, against the study's control group, on Wave 2
#   and Wave 3 response in Studies 1 to 5. One lm_robust per study and wave, HC2 standard errors.

source(here::here("maintained", "helpers.R"))

five_studies <- read_rds(here::here("original", "clean_survey_experiments.rds")) |>
  filter(!is.na(dosage_continuous)) |>
  mutate(dose = as.factor(dosage_continuous))

# Estimates ----
# Everyone in Study 4 responded at Wave 2, so both of its Wave 2 effects are exactly zero with an NA standard
# error; under older estimatr the standard error was zero to rounding error, which drew the same nothing.
estimates <- c(`Wave 2` = "responded_post", `Wave 3` = "responded_post_3") |>
  map(\(y) five_studies |>
        group_by(study) |>
        reframe(tidy(lm_robust(reformulate("dose", y), data = pick(everything()), se_type = "HC2")))) |>
  list_rbind(names_to = "wave") |>
  filter(term != "(Intercept)") |>
  mutate(dosage_continuous = as.numeric(str_remove(term, "^dose"))) |>
  select(wave, study, dosage_continuous, outcome, estimate, std.error, statistic, p.value, conf.low, conf.high, df)
write_csv(estimates, here::here("maintained", "output", "estimates_response_rate_effects.csv"))

# Figure ----
gg_df <- estimates |>
  mutate(label = str_c("Number of treatment videos: ", dosage_continuous))

g <- ggplot(gg_df, aes(estimate, label)) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  geom_point() +
  geom_text(aes(label = round(estimate, 3)), nudge_y = 0.2) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high)) +
  scale_y_discrete(limits = rev) +
  facet_grid(study ~ wave, scales = "free_y") +
  theme_bw() +
  theme(axis.title.y = element_blank()) +
  labs(x = "ATE of treatment on response rate")

save_figure(g, "figure_a3_response_rate_effects", width = 6.5, height = 6.5)
