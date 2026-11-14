# coppock_green_porter_2026/run_all.R
# Runs in order: verify the archive, rerun it twice in scratch copies (set CGP_REUSE_ARCHIVE_RUN=TRUE to reuse
# a finished run), run the maintained rewrite, compute the page's values, compare the rewrite with the
# archive, render the errata, build and test the ground truth, and score the prose claims. It reads only the
# Dataverse deposit.

library(here)
here::i_am("run_all.R")

source(here::here("download_original.R"))
source(here::here("maintained", "run_archive.R"))

# The maintained rewrite ----
# Each script runs in its own R process, so none can lean on what an earlier one attached or defined.
# Figure 7 reads the post-treatment estimates and Figure C.8 the hypothetical estimates, so those come first.
rewrite_scripts <- c(
  "estimates_post_treatment_outcomes.R",
  "figure_1_field_experiment.R",
  "table_b3_differences_in_cates.R",
  "figure_b6_field_experiment_block_fe.R",
  "figure_2_design_counts.R",
  "figure_3_hypotheticals_study_1.R",
  "figure_4_hypotheticals_study_2.R",
  "figure_5_party_switching.R",
  "figure_6_hypothetical_order.R",
  "figure_7_meta_analysis_outcomes.R",
  "figure_a2_response_rates.R",
  "figure_a3_response_rate_effects.R",
  "figure_a4_trimming_bounds.R",
  "table_a1_assignment_studies_1_2.R",
  "table_a2_assignment_studies_3_5.R",
  "figure_c7_office_partisanship_guesses.R",
  "figure_c8_meta_analysis_hypotheticals.R",
  "figure_c9_probabilities_study_2.R",
  "figure_c10_party_switching_study_1.R",
  "figure_c11_party_switching_study_2.R",
  "table_c5_macropartisanship.R"
)
for (script in rewrite_scripts) callr::rscript(here::here("maintained", script), show = FALSE)

source(here::here("maintained", "page_values.R"))
callr::rscript(here::here("maintained", "compare_with_archive.R"), show = FALSE)
# lualatex stamps the PDF with the wall clock unless the source date is fixed.
Sys.setenv(SOURCE_DATE_EPOCH = "0", FORCE_SOURCE_DATE = "1")
quarto::quarto_render(here::here("errata.qmd"))
source(here::here("ground_truth", "build_ground_truth.R"))
source(here::here("ground_truth", "negative_controls.R"))
source(here::here("maintained", "in_text_claims.R"))
