# coppock_green_porter_2026/maintained/run_archive.R
# Output: maintained/archive_run/ (gitignored), maintained/output/archive_run_status.csv
# Depends on: original_extracted/ (rebuilt by download_original.R)
# Description: Run the archive's scripts twice in scratch copies, each script in a fresh R process: as
#   shipped, and stripped of the five precomputed estimate files. The archive comments out most of the
#   write_rds() calls that build those files, so the stripped copy restores those lines; a stripped run
#   without them could not reach the figures. Data files stay, because the archive ships only cleaned
#   data (maintained/raw_to_clean.R tests those). Set CGP_REUSE_ARCHIVE_RUN=TRUE to reuse a finished run.

library(tidyverse)
library(here)
here::i_am("maintained/run_archive.R")

run_dir <- here::here("maintained", "archive_run")
status_path <- here::here("maintained", "output", "archive_run_status.csv")
root <- here::here("original_extracted", "replication_archive")

scripts <- c("analyze_outcomes_study_1.R", "analyze_outcomes_study_2.R",
             "figure_3_hypotheticals_study_1.R", "figure_4_hypotheticals_study_2.R",
             "table_C5_macropartisanship_comparison.R",
             "figure_1_figure_B5_table_B3_field_experiment_nofe.R", "figure_2_consort_calculations.R",
             "figure_5_party_switching.R", "figure_6_hypotheticals_order.R",
             "figure_7_meta_analysis_outcomes.R", "figure_A4_trimming_analysis.R",
             "figure_B6_field_experiment_yesfe.R", "figure_C7_theoffice.R",
             "figure_C8_meta_analysis_hypotheticals.R", "figure_C9_probabilities_study_2.R",
             "figure_C10-C11_party_switching.R", "figures_A2_and_A3_response_rates.R",
             "table_A1_survey_experiments_1_and_2_ns.R", "table_A2_survey_experiments_345_ns.R",
             "in_text_claims.R")
stopifnot(all(file.exists(file.path(root, scripts))))
precomputed <- c("ates_study_1.rds", "ates_study_2.rds", "hypotheticals_study_1.rds",
                 "hypotheticals_study_2.rds", "macropartisanship_estimates.rds")

restore_writes <- function(path) {
  lines <- read_lines(path)
  str_replace(lines, "^#\\s*(write_rds\\()", "\\1") |> write_lines(path)
}

run_pass <- function(pass) {
  scratch <- file.path(run_dir, pass)
  unlink(scratch, recursive = TRUE)
  dir.create(scratch, recursive = TRUE)
  file.copy(list.files(root, full.names = TRUE), scratch)
  dir.create(file.path(scratch, "logs"))
  if (pass == "stripped") {
    unlink(file.path(scratch, precomputed))
    walk(file.path(scratch, scripts), restore_writes)
  }
  map_dfr(scripts, \(script) {
    log_lines <- withr::with_dir(scratch, suppressWarnings(
      system2("Rscript", shQuote(script), stdout = TRUE, stderr = TRUE)))
    write_lines(log_lines, file.path(scratch, "logs", str_c(script, ".log")))
    errors <- str_subset(log_lines, "^Error|Execution halted")
    tibble(pass = pass, script = script, exit = attr(log_lines, "status") %||% 0L,
           error = str_trunc(first(errors) %||% "", 150))
  })
}

if (!(Sys.getenv("CGP_REUSE_ARCHIVE_RUN") == "TRUE" && file.exists(status_path))) {
  dir.create(run_dir, showWarnings = FALSE)
  write_csv(map_dfr(c("stripped", "as_shipped"), run_pass), status_path)
}
print(read_csv(status_path, show_col_types = FALSE), n = Inf)
