# coppock_green_porter_2026/maintained/raw_to_clean.R
# Output: maintained/output/raw_to_clean.csv, maintained/output/fielding_months.csv
# Depends on: Alex's local project folder (archived_projects/coppock_green_porter_2026), never copied here
# Description: Run the project's two cleaning scripts on the raw exports in a scratch copy outside this
#   repo, compare their output with the archive's cleaned files, and count fielding months from the raw
#   StartDate. The raw exports hold IP addresses and coordinates, so only column-free counts leave the
#   scratch copy. A stranger cannot run this script.

library(tidyverse)
library(here)
here::i_am("maintained/raw_to_clean.R")

project <- file.path(Sys.getenv("HOME"), "Library/CloudStorage/Dropbox/archived_projects/coppock_green_porter_2026/coppock_green_porter_hypotheticals")
stopifnot(dir.exists(project))
scratch <- tempfile("raw_to_clean")
dir.create(file.path(scratch, "code"), recursive = TRUE)
file.symlink(file.path(project, "data"), file.path(scratch, "raw"))
dir.create(file.path(scratch, "data"))
walk(list.files(file.path(project, "data"), full.names = TRUE, pattern = "\\.csv$|\\.xls$"), \(f) file.symlink(f, file.path(scratch, "data", basename(f))))
scripts <- c("clean_hypotheticals_study_1.R", "clean_hypotheticals_study_2.R")
file.copy(file.path(project, "code", scripts), file.path(scratch, "code"))
file.create(file.path(scratch, ".here"))
log_status <- map_int(scripts, \(s) withr::with_dir(scratch, suppressWarnings(system2("Rscript", shQuote(file.path("code", s)), stdout = FALSE, stderr = FALSE))))

archive <- here::here("original_extracted", "replication_archive")
compare <- function(file) {
  rebuilt_path <- file.path(scratch, "data", file)
  if (!file.exists(rebuilt_path)) return(tibble(file, rebuilt = FALSE, rows_rebuilt = NA_integer_, rows_archive = NA_integer_, shared_columns = NA_integer_, columns_differing = NA_integer_))
  rebuilt <- read_rds(rebuilt_path); shipped <- read_rds(file.path(archive, file))
  shared <- intersect(names(rebuilt), names(shipped))
  differing <- if (nrow(rebuilt) == nrow(shipped)) sum(map_lgl(shared, \(v) !isTRUE(all.equal(as.vector(rebuilt[[v]]), as.vector(shipped[[v]]), check.attributes = FALSE)))) else NA_integer_
  tibble(file, rebuilt = TRUE, rows_rebuilt = nrow(rebuilt), rows_archive = nrow(shipped), shared_columns = length(shared), columns_differing = differing)
}
results <- map_dfr(c("clean_hypotheticals_study_1.rds", "clean_hypotheticals_study_2.rds", "clean_hypotheticals_study_1_long.rds", "clean_hypotheticals_study_2_long.rds"), compare) |>
  mutate(script_exits = str_flatten(log_status, ","))
write_csv(results, here::here("maintained", "output", "raw_to_clean.csv"))
print(results)

months <- map_dfr(c("hyp_orig_d.csv", "hyp_orig_i.csv", "hyp_orig_r.csv", "hyp_rep_d.csv", "hyp_rep_i.csv", "hyp_rep_r.csv"), \(f) {
  start <- read_csv(file.path(project, "data", f), col_select = "StartDate", show_col_types = FALSE, col_types = cols(.default = "c"))$StartDate
  dates <- suppressWarnings(mdy_hm(start))
  tibble(file = f, month = format(dates, "%Y-%m")) |> filter(!is.na(month)) |> count(file, month)
})
write_csv(months, here::here("maintained", "output", "fielding_months.csv"))
print(months)
unlink(scratch, recursive = TRUE)
