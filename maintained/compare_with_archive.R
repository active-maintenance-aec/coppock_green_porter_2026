# coppock_green_porter_2026/maintained/compare_with_archive.R
# Output: output/comparison_with_archive.csv
# Depends on: the rewrite's estimates in maintained/output/, and the five estimate files that the stripped
#   rerun of the deposit rebuilt in maintained/archive_run/stripped/replication_archive/
# Description: How far the rewrite's estimates sit from the deposit's own code run on the same data. Point
#   estimates and analytic standard errors should agree to rounding error. The macropartisanship standard
#   errors are bootstrapped, so they agree only up to Monte Carlo noise, and the ratio is reported instead.

source(here::here("maintained", "helpers.R"))

archive_file <- \(file) read_rds(here::here("maintained", "archive_run", "stripped", "replication_archive", file))
estimates_file <- \(file) read_csv(here::here("maintained", "output", file), show_col_types = FALSE)

compare <- function(rewrite, archive, keys) {
  joined <- inner_join(rewrite, archive, by = keys, suffix = c("_rewrite", "_archive"), relationship = "one-to-one")
  tibble(rows_rewrite = nrow(rewrite), rows_archive = nrow(archive), rows_matched = nrow(joined),
         max_abs_diff_estimate = max(abs(joined$estimate_rewrite - joined$estimate_archive), na.rm = TRUE),
         max_abs_diff_std.error = max(abs(joined$std.error_rewrite - joined$std.error_archive), na.rm = TRUE))
}

archive_estimates <- function(files) {
  map(files, archive_file) |>
    list_rbind(names_to = "study") |>
    mutate(pid_7_factor_pre = as.character(pid_7_factor_pre)) |>
    select(study, outcome, pid_7_factor_pre, term, estimate, std.error)
}

# Hypotheticals and post-treatment outcomes ----
hypotheticals <- compare(
  bind_rows(study_1 = estimates_file("estimates_hypotheticals_study_1.csv"),
            study_2 = estimates_file("estimates_hypotheticals_study_2.csv"), .id = "study") |>
    select(study, outcome, pid_7_factor_pre, term, estimate, std.error),
  archive_estimates(c(study_1 = "hypotheticals_study_1.rds", study_2 = "hypotheticals_study_2.rds")),
  c("study", "outcome", "pid_7_factor_pre", "term"))

post_treatment <- compare(
  estimates_file("estimates_post_treatment_outcomes.csv") |>
    select(study, outcome, pid_7_factor_pre, term, estimate, std.error),
  archive_estimates(c(study_1 = "ates_study_1.rds", study_2 = "ates_study_2.rds")),
  c("study", "outcome", "pid_7_factor_pre", "term"))

# Macropartisanship ----
# Both parties' effects are stacked so one comparison covers them.
stack_mp <- \(d) d |>
  select(study, hypothetical_label, D_estimate = MP_D, R_estimate = MP_R, D_std.error = MP_D_SE, R_std.error = MP_R_SE) |>
  pivot_longer(-c(study, hypothetical_label), names_to = c("party", ".value"), names_sep = "_")
mp_rewrite <- estimates_file("estimates_macropartisanship.csv") |> stack_mp()
mp_archive <- archive_file("macropartisanship_estimates.rds") |> stack_mp()
mp_joined <- inner_join(mp_rewrite, mp_archive, by = join_by(study, hypothetical_label, party),
                        suffix = c("_rewrite", "_archive"), relationship = "one-to-one")
macropartisanship <- compare(mp_rewrite, mp_archive, c("study", "hypothetical_label", "party")) |>
  mutate(min_ratio_std.error = min(mp_joined$std.error_rewrite / mp_joined$std.error_archive),
         max_ratio_std.error = max(mp_joined$std.error_rewrite / mp_joined$std.error_archive))

comparison <- bind_rows(`Hypothetical effects (Figures 3 and 4)` = hypotheticals,
                        `Post-treatment outcomes` = post_treatment,
                        `Macropartisanship (Table C.5)` = macropartisanship, .id = "estimates")
write_csv(comparison, here::here("maintained", "output", "comparison_with_archive.csv"))
print(comparison, width = Inf)
