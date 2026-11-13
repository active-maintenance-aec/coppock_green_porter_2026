# coppock_green_porter_2026/maintained/run_archive.R
# Output: maintained/archive_run/ (gitignored), ground_truth/archive_run_status.csv,
#   ground_truth/archive_overwrites.csv
# Depends on: original_extracted/ (rebuilt by download_original.R)
# Description: Run the archive's scripts twice in scratch copies, each script in a fresh R process: as
#   shipped, and stripped of the five precomputed estimate files. The archive comments out most of the
#   write_rds() calls that build those files, so the stripped copy restores those lines; a stripped run
#   without them could not reach the figures. Data files stay, because the archive ships only cleaned
#   data (maintained/raw_to_clean.R tests those). Set CGP_REUSE_ARCHIVE_RUN=TRUE to reuse a finished run.
#   Two conditions were measured, not assumed. table_C5 writes to ../replication_archive/, so each scratch
#   copy sits in a directory of that name. figure_5, figure_7, and figure_C8 call format_num or
#   make_se_entry, exported by rdss, without attaching rdss, so they fail as shipped; the stripped pass
#   attaches it in those three scripts, and the as_shipped pass leaves them failing. figure_C8 also calls
#   ggstance's position_dodgev without attaching it (figure_7 gets it through coefplot), so the stripped pass
#   attaches that as well. helpers.R is only ever sourced by the other scripts, so it gets no row.

library(tidyverse)
library(here)
here::i_am("maintained/run_archive.R")

run_dir <- here::here("maintained", "archive_run")
status_path <- here::here("ground_truth", "archive_run_status.csv")
overwrites_path <- here::here("ground_truth", "archive_overwrites.csv")
root <- here::here("original_extracted", "replication_archive")

# Every script is analysis: the archive ships cleaned data, and the two analyze_outcomes scripts
# write the estimate files the figure scripts read.
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

unattached_rdss <- c("figure_5_party_switching.R", "figure_7_meta_analysis_outcomes.R",
                     "figure_C8_meta_analysis_hypotheticals.R")
attach_rdss <- \(path) write_lines(c("library(rdss)", "library(ggstance)", read_lines(path)), path)

restore_writes <- function(path) {
  lines <- read_lines(path)
  str_replace(lines, "^#\\s*(write_rds\\()", "\\1") |> write_lines(path)
}

# Checksums of the scratch copy, leaving out the logs this harness writes.
checksums <- function(scratch) {
  files <- list.files(scratch, recursive = TRUE) |> str_subset("^logs/", negate = TRUE)
  set_names(unname(tools::md5sum(file.path(scratch, files))), files)
}

# Whether a file the run wrote carries the deposit's contents. An rds is compared as an object at a
# tolerance that absorbs the last bits of a double; a CSV as a table; anything else byte for byte.
same_contents <- function(file, path) {
  deposited <- file.path(root, file)
  if (str_ends(file, "\\.rds")) {
    isTRUE(all.equal(read_rds(path), read_rds(deposited), tolerance = 1e-12))
  } else if (str_ends(file, "\\.csv")) {
    identical(read_csv(path, col_types = cols(.default = col_character())),
              read_csv(deposited, col_types = cols(.default = col_character())))
  } else {
    unname(tools::md5sum(path)) == unname(tools::md5sum(deposited))
  }
}

run_pass <- function(pass) {
  unlink(file.path(run_dir, pass), recursive = TRUE)
  scratch <- file.path(run_dir, pass, "replication_archive")
  dir.create(scratch, recursive = TRUE)
  file.copy(list.files(root, full.names = TRUE), scratch)
  dir.create(file.path(scratch, "logs"))
  if (pass == "stripped") {
    unlink(file.path(scratch, precomputed))
    walk(file.path(scratch, scripts), restore_writes)
    walk(file.path(scratch, unattached_rdss), attach_rdss)
  }
  before <- checksums(scratch)
  status <- map(scripts, \(script) {
    log_lines <- withr::with_dir(scratch, suppressWarnings(
      system2("Rscript", shQuote(script), stdout = TRUE, stderr = TRUE)))
    write_lines(log_lines, file.path(scratch, "logs", str_c(script, ".log")))
    exit_status <- attr(log_lines, "status") %||% 0L
    errors <- str_subset(log_lines, "^Error|Execution halted")
    tibble(pass = pass, script = script, role = "analysis", exit_status = exit_status,
           outcome = if_else(exit_status == 0, "clean", "error"),
           stopped_at = if_else(exit_status == 0, NA_character_,
                                str_trunc(first(errors) %||% NA_character_, 150)))
  }) |>
    list_rbind()
  after <- checksums(scratch)

  # A shipped file the run wrote over is overwritten, and one the stripped pass withheld is rebuilt
  # or stays withheld; a file no deposit member names is added.
  withheld <- if (pass == "stripped") precomputed else character()
  overwrites <- bind_rows(
    tibble(file = withheld, effect = if_else(file %in% names(after), "rebuilt", "withheld")),
    tibble(file = names(before)) |>
      filter(after[file] != before[file]) |>
      mutate(effect = "overwritten"),
    tibble(file = setdiff(names(after), c(names(before), withheld)), effect = "added")
  ) |>
    mutate(pass = pass,
           matches_deposit = map2_lgl(file, effect, \(f, e) {
             if (e %in% c("rebuilt", "overwritten")) same_contents(f, file.path(scratch, f)) else NA
           })) |>
    select(pass, file, effect, matches_deposit)
  list(status = status, overwrites = overwrites)
}

if (!(Sys.getenv("CGP_REUSE_ARCHIVE_RUN") == "TRUE" && file.exists(status_path) && file.exists(overwrites_path))) {
  dir.create(run_dir, showWarnings = FALSE)
  passes <- map(c("stripped", "as_shipped"), run_pass)
  write_csv(map(passes, "status") |> list_rbind(), status_path)
  write_csv(map(passes, "overwrites") |> list_rbind(), overwrites_path)
}
print(read_csv(status_path, show_col_types = FALSE), n = Inf)
print(read_csv(overwrites_path, show_col_types = FALSE), n = Inf)
