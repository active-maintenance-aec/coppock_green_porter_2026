# coppock_green_porter_2026/run_all.R
# Runs in order: verify the archive, rerun it twice in scratch copies (set CGP_REUSE_ARCHIVE_RUN=TRUE to reuse
# a finished run), compute the page's values, render the errata, build and test the ground truth, and score
# the prose claims. It reads only the Dataverse deposit. maintained/raw_to_clean.R reads the authors' raw
# exports, which are not deposited, so it is run by hand and not from here.

library(here)
here::i_am("run_all.R")

source(here::here("download_original.R"))
source(here::here("maintained", "run_archive.R"))
source(here::here("maintained", "page_values.R"))
quarto::quarto_render(here::here("errata.qmd"))
source(here::here("ground_truth", "build_ground_truth.R"))
source(here::here("ground_truth", "negative_controls.R"))
source(here::here("maintained", "in_text_claims.R"))
