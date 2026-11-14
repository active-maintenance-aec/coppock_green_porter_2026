# coppock_green_porter_2026/maintained/figure_a4_trimming_bounds.R
# Output: output/figure_a4_trimming_bounds.{pdf,png}, output/estimates_trimming_bounds.csv
# Depends on: original/clean_survey_experiments.rds, helpers.R
# Description: Trimming bounds on the effect of the ads on Wave 3 party ID among always-reporters, for each
#   ad-versus-control contrast in Studies 1 to 5, beside the naive difference in means among responders.
#   Bounds are computed within each pre-treatment party ID group and averaged with weights equal to the
#   number of treated responders; standard errors come from a simple bootstrap of respondents.

source(here::here("maintained", "helpers.R"))

five_studies <- read_rds(here::here("original", "clean_survey_experiments.rds"))
sims <- 1000

# Trimming bounds within one stratum ----
# Only the control group is ever trimmed: when the treatment group responds at the higher rate (Q < 0), the
# deposit reports the naive difference as both bounds rather than trimming the treated, and so does this.
trimming_bounds <- function(Y, Z) {
  control_R <- !is.na(Y[Z == 0])
  treat_R <- !is.na(Y[Z == 1])
  control_Y <- Y[Z == 0][control_R]
  treat_Y <- Y[Z == 1][treat_R]
  Q <- (mean(control_R) - mean(treat_R)) / mean(control_R)
  naive_est <- mean(treat_Y) - mean(control_Y)
  if (Q >= 0) {
    n <- length(control_Y)
    number_to_trim <- ceiling(Q * n)
    low_est <- mean(treat_Y) - mean(sort(control_Y)[(number_to_trim + 1):n])
    high_est <- mean(treat_Y) - mean(sort(control_Y)[1:(n - number_to_trim)])
  } else {
    number_to_trim <- NA
    low_est <- high_est <- naive_est
  }
  tibble(low_est, naive_est, high_est, control_RR = mean(control_R), treat_RR = mean(treat_R), Q,
         n_treat = length(treat_Y), number_to_trim)
}

stratified_bounds <- function(data) {
  data |> reframe(trimming_bounds(pid_7_rev_post_3, Z), .by = pid_7_rev_pre)
}

# Contrasts ----
# Each multi-dose study contributes one contrast per dose, each against its own control group.
contrasts <- tribble(
  ~label, ~study_id, ~dose,
  "Study 1: 1 Ad", "study_1_mturk", 1,
  "Study 2: 1 Ad", "study_2_mturk", 1,
  "Study 3: 1 Ad", "study_3_dosage", 1,
  "Study 3: 2 Ads", "study_3_dosage", 2,
  "Study 3: 3 Ads", "study_3_dosage", 3,
  "Study 4: 3 Ads", "study_4_dosage", 3,
  "Study 4: 6 Ads", "study_4_dosage", 6,
  "Study 5: 3 Ads", "study_5_dosage", 3,
  "Study 5: 6 Ads", "study_5_dosage", 6
) |>
  mutate(data = map2(study_id, dose, \(s, k) five_studies |>
                       filter(study == s, dosage_continuous %in% c(0, k)) |>
                       mutate(Z = as.integer(dosage_continuous == k))))

# Estimates ----
# The point estimate averages over strata without dropping missing values; the replicates drop them, so a
# stratum whose resampled control group has no responders falls out of that replicate's average.
# Replicates in which some stratum lacks an arm are dropped. The deposit dropped the same replicates by
# catching the error that trimming_bounds() raises on them.
# Corrected: the deposit set the naive estimate's standard error to sd(low_est), so the published naive CIs
# carry the lower bound's spread. naive_est_se below is sd(naive_est); naive_est_se_deposit keeps the slip
# from the same draws, to measure the move.
set.seed(343)
estimates <- contrasts |>
  mutate(result = map(data, \(d) {
    point <- stratified_bounds(d) |>
      summarise(across(c(low_est, naive_est, high_est), \(x) weighted.mean(x, n_treat)))
    replicates <- bootstraps(d, times = sims)$splits |>
      map(analysis) |>
      keep(\(b) b |> summarise(both = n_distinct(Z) == 2, .by = pid_7_rev_pre) |> pull(both) |> all()) |>
      map(\(b) stratified_bounds(b) |>
            summarise(across(c(low_est, naive_est, high_est), \(x) weighted.mean(x, n_treat, na.rm = TRUE)))) |>
      list_rbind()
    point |>
      mutate(low_est_se = sd(replicates$low_est),
             naive_est_se = sd(replicates$naive_est),
             high_est_se = sd(replicates$high_est),
             naive_est_se_deposit = sd(replicates$low_est),
             n_replicates = nrow(replicates))
  })) |>
  select(label, result) |>
  unnest(result) |>
  mutate(bounds_conf.low = low_est - 1.96 * low_est_se,
         bounds_conf.high = high_est + 1.96 * high_est_se,
         naive_conf.low = naive_est - 1.96 * naive_est_se,
         naive_conf.high = naive_est + 1.96 * naive_est_se)
write_csv(estimates, here::here("maintained", "output", "estimates_trimming_bounds.csv"))

# Figure ----
gg_df <- estimates |>
  mutate(label = fct_rev(fct_inorder(label)))

g <- ggplot(gg_df, aes(y = label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_point(aes(x = low_est), position = position_nudge(y = -0.2), shape = 25) +
  geom_point(aes(x = high_est), position = position_nudge(y = -0.2), shape = 24) +
  geom_errorbar(aes(xmin = bounds_conf.low, xmax = bounds_conf.high), orientation = "y",
                position = position_nudge(y = -0.2), width = 0.1) +
  geom_point(aes(x = naive_est), position = position_nudge(y = 0.2)) +
  geom_errorbar(aes(xmin = naive_conf.low, xmax = naive_conf.high), orientation = "y",
                position = position_nudge(y = 0.2), width = 0.1) +
  annotate("text", y = 9 + 0.2, x = 0.2, label = "Naive estimate (95% CI)", hjust = 0) +
  annotate("text", y = 9 - 0.2, x = 0.2, label = "Trimming Bounds (95% CI)", hjust = 0) +
  theme_bw() +
  theme(axis.title.y = element_blank()) +
  labs(x = "Estimated ATE of ads on Wave 3 partisanship among Always-Reporters")

save_figure(g, "figure_a4_trimming_bounds", width = 6.5, height = 6.5)
