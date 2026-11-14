# coppock_green_porter_2026/maintained/estimates_post_treatment_outcomes.R
# Output: output/estimates_post_treatment_outcomes.csv
# Depends on: original/clean_hypotheticals_study_{1,2}.rds, helpers.R
# Description: Effects of the hypotheticals on the three post-treatment outcomes in each study, overall and
#   by pre-treatment party ID, with the one-sided test the paper reports. Replaces the deposit's two
#   analyze_outcomes scripts, which differ only in the study they read.

source(here::here("maintained", "helpers.R"))

outcomes <- c("pid_7_post", "partisan_evaluation_index_post", "partisan_identification_index_post")
studies <- list(study_1 = read_rds(here::here("original", "clean_hypotheticals_study_1.rds")),
                study_2 = read_rds(here::here("original", "clean_hypotheticals_study_2.rds")))

# One-sided tests ----
# The hypothesis is that pro-Democratic scenarios move respondents toward the Democrats and pro-Republican
# ones toward the Republicans. On party ID (1 = Strong Democrat) toward the Democrats is a negative effect;
# on the two 0-1 indexes, where higher values are more Democratic, it is a positive one. The deposit tests
# both indexes in the party ID direction, and its Study 1 test returns NA on every pro-Democratic row
# because it matches a label already renamed (errata entry 5). Here the direction follows the outcome.
estimates <- studies |>
  map(\(d) map(outcomes, \(y) estimate_by_pid(d, y, covariates = c("pid_7_factor_pre", "batch_pre"))) |> list_rbind()) |>
  list_rbind(names_to = "study") |>
  mutate(toward_democrats = if_else(outcome == "pid_7_post", -1, 1),
         hypothesized_sign = if_else(term == "Pro-Democratic hypotheticals", toward_democrats, -toward_democrats),
         p_one_sided = pt(hypothesized_sign * statistic, df = df, lower.tail = FALSE)) |>
  select(-toward_democrats, -hypothesized_sign)

write_csv(estimates, here::here("maintained", "output", "estimates_post_treatment_outcomes.csv"))
print(filter(estimates, pid_7_factor_pre == "Overall"), width = Inf)
