# coppock_green_porter_2026/maintained/figure_c9_probabilities_study_2.R
# Output: output/figure_c9_probabilities_study_2.{pdf,png}, output/estimates_probabilities_study_2.csv
# Depends on: original/clean_hypotheticals_study_2.rds, helpers.R
# Description: Study 2's mean judged probability that each scenario comes true, by treatment arm and
#   pre-treatment party ID. Each mean is an intercept-only lm_robust with HC2 standard errors.

source(here::here("maintained", "helpers.R"))

study_2 <- read_rds(here::here("original", "clean_hypotheticals_study_2.rds"))

# Estimates ----
estimates <- study_2 |>
  pivot_longer(c(prob_healthcare_2, prob_guns_2, prob_ukraine_2, prob_violence_2, prob_economy_2, prob_camps_2),
               names_to = "scenario") |>
  mutate(scenario = str_to_title(str_remove_all(scenario, "^prob_|_2$")),
         treatment = factor(treatment, levels = treatment_levels, labels = treatment_labels)) |>
  group_by(pid_7_factor_pre, treatment, scenario) |>
  reframe(tidy(lm_robust(value ~ 1, data = pick(everything()), se_type = "HC2"))) |>
  select(pid_7_factor_pre, treatment, scenario, estimate, std.error, conf.low, conf.high, df)
write_csv(estimates, here::here("maintained", "output", "estimates_probabilities_study_2.csv"))

# Figure ----
# Corrected: the deposit's axis title never closes the bracket it opens.
gg_df <- estimates |>
  mutate(scenario = factor(scenario, levels = scenario_levels))

g <- ggplot(gg_df, aes(pid_7_factor_pre, estimate, color = pid_7_factor_pre)) +
  geom_point() +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high)) +
  facet_grid(scenario ~ treatment) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_color_manual(values = palette7) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(), strip.background = element_blank(), legend.position = "none") +
  labs(y = str_c("Nobody can know for sure, but some observers have anticipated that the scenario described ",
                 "could become reality.\nHow likely do you think it is that... ",
                 "[7-pt scale: Extremely unlikely (0%) to Extremely likely (100%)]"))

save_figure(g, "figure_c9_probabilities_study_2", width = 6.5, height = 9)
