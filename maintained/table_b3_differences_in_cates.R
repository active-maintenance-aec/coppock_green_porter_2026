# coppock_green_porter_2026/maintained/table_b3_differences_in_cates.R
# Output: output/table_b3_differences_in_cates.{tex,csv}
# Depends on: original/clean_field_experiments.rds, helpers.R
# Description: Differences in the field experiment's conditional effects between pairs of parties and
#   between the two partner organizations, from treatment-by-subgroup interactions.

source(here::here("maintained", "helpers.R"))

field <- read_rds(here::here("original", "clean_field_experiments.rds"))
outcomes <- c("pid_7_rev_post", "partisan_evaluation_index_post", "partisan_identification_index_post")

# Each contrast is one interaction term: a party pair fit on those two parties only, adjusting for
# organization, and the organization contrast fit on everyone, adjusting for party. Household-clustered
# standard errors with the Stata small-sample correction, as the deposit computes them.
contrasts <- tribble(
  ~contrast, ~parties, ~formula_rhs, ~term,
  "Independent vs Democrat", c("Democrat", "Independent"), "Z + org + Z * pid_3_pre_nona", "Z:pid_3_pre_nonaIndependent",
  "Republican vs Democrat", c("Democrat", "Republican"), "Z + org + Z * pid_3_pre_nona", "Z:pid_3_pre_nonaRepublican",
  "Republican vs Independent", c("Independent", "Republican"), "Z + org + Z * pid_3_pre_nona", "Z:pid_3_pre_nonaRepublican",
  # The deposit labels this row "Org 2 vs Org 2" (errata entry 9); the term is Z:orgorg2.
  "Org 2 vs Org 1", c("Democrat", "Independent", "Republican", "Unknown"), "Z + pid_3_pre_nona + org + org * Z", "Z:orgorg2"
)
stopifnot(all(field$pid_3_pre_nona %in% contrasts$parties[[4]]))

estimates <- expand_grid(outcome = outcomes, contrasts) |>
  mutate(fit = pmap(list(outcome, parties, formula_rhs, term), \(y, p, rhs, t) {
    lm_robust(as.formula(str_c(y, " ~ ", rhs)), data = filter(field, pid_3_pre_nona %in% p),
              clusters = hhid_anon, se_type = "stata") |>
      tidy() |>
      filter(term == t) |>
      select(estimate, std.error, p.value)
  })) |>
  unnest(fit) |>
  arrange(outcome)

tab_differences <- estimates |>
  transmute(outcome = str_replace_all(outcome, "_", " "), contrast,
            entry = sprintf("%.3f (%.3f)", estimate, std.error), p_value = sprintf("%.3f", p.value))
write_csv(tab_differences, here::here("maintained", "output", "table_b3_differences_in_cates.csv"))
tab_differences |>
  kable(format = "latex", booktabs = TRUE, col.names = c("Outcome", "Contrast", "Estimate (SE)", "p-value")) |>
  write_lines(here::here("maintained", "output", "table_b3_differences_in_cates.tex"))
print(tab_differences, n = Inf)
