# coppock_green_porter_2026/maintained/figure_a2_response_rates.R
# Output: output/figure_a2_response_rates.{pdf,png}, output/estimates_response_rates.csv
# Depends on: original/clean_survey_experiments.rds, helpers.R
# Description: Wave 2 and Wave 3 response rates in Studies 1 to 5, by number of treatment videos, each an
#   intercept-only lm_robust with HC2 standard errors.

source(here::here("maintained", "helpers.R"))

five_studies <- read_rds(here::here("original", "clean_survey_experiments.rds")) |>
  filter(!is.na(dosage_continuous))

# Estimates ----
# Where everyone in a group responded, the standard error is NA and the interval is not drawn; under older
# estimatr it was zero to rounding error, which drew the same nothing.
estimates <- c(`Wave 2` = "responded_post", `Wave 3` = "responded_post_3") |>
  map(\(y) five_studies |>
        group_by(study, dosage_continuous) |>
        reframe(tidy(lm_robust(reformulate("1", y), data = pick(everything()), se_type = "HC2")))) |>
  list_rbind(names_to = "wave") |>
  select(wave, study, dosage_continuous, outcome, estimate, std.error, conf.low, conf.high, df)
write_csv(estimates, here::here("maintained", "output", "estimates_response_rates.csv"))

# Figure ----
gg_df <- estimates |>
  mutate(label = str_c("Number of treatment videos: ", dosage_continuous))

g <- ggplot(gg_df, aes(estimate, label)) +
  geom_point() +
  geom_text(aes(label = round(estimate, 2)), nudge_y = 0.2) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high)) +
  scale_y_discrete(limits = rev) +
  facet_grid(study ~ wave, scales = "free_y") +
  theme_bw() +
  theme(axis.title.y = element_blank()) +
  labs(x = "Response rate")

save_figure(g, "figure_a2_response_rates", width = 6.5, height = 6.5)
