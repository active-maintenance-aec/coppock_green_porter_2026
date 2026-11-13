# coppock_green_porter_2026/ground_truth/build_ground_truth.R
# Output: ground_truth/coppock_green_porter_2026_ground_truth.csv, ground_truth/float_coverage.csv
# Depends on: ground_truth/published_claims.csv, errata_entries.csv, maintained/output/page_values.csv
# Description: Compare the extracted prose values with the rerun at the page's precision, then halt on a
#   mismatch the spine does not report, a spine id with no row, or a spine id on a claim that holds. The
#   values come from the deposit's cleaned data, so a defect both inherit would pass; raw_to_clean.R tests
#   the two hypothetical studies' cleaning only. No figure or table cell is in this gate. The archive's own
#   printed values were not read back, so value_script and match are NA on every row.

library(tidyverse)
library(here)
here::i_am("ground_truth/build_ground_truth.R")

claims <- read_csv(here::here("ground_truth", "published_claims.csv"), col_types = cols(.default = col_character(), digits = col_integer()))
values <- read_csv(here::here("maintained", "output", "page_values.csv"), show_col_types = FALSE)
spine <- read_csv(here::here("errata_entries.csv"), show_col_types = FALSE)
spine_ids <- spine$claim_ids |> na.omit() |> str_split(";") |> unlist() |> discard(\(x) x == "")

# Comparison ----
ground_truth <- claims |>
  left_join(values, by = "claim_id", relationship = "one-to-one") |>
  mutate(printed = sprintf(str_c("%.", digits, "f"), value_rewrite),
         match_rewrite = printed == value_paper,
         defect_locus = case_when(!match_rewrite ~ "paper_internal", .default = NA_character_),
         paper_id = "coppock_green_porter_2026")

stopifnot(all(!is.na(ground_truth$value_rewrite)),
          all(spine_ids %in% ground_truth$claim_id),
          all(ground_truth$claim_id[!ground_truth$match_rewrite] %in% spine_ids),
          all(!ground_truth$match_rewrite[ground_truth$claim_id %in% spine_ids]),
          !anyNA(ground_truth$claim_type))

# Disposition ----
# corrected_here is computed from the errata spine and never typed. No other judgment has been recorded
# for this paper, so any other defect stays NA until someone looks.
ground_truth <- ground_truth |>
  mutate(disposition = case_when(is.na(defect_locus) ~ NA_character_,
                                 claim_id %in% spine_ids ~ "corrected_here",
                                 .default = NA_character_),
         disposition_reason = NA_character_)

undisposed <- ground_truth |> filter(defect_locus == "paper_internal", is.na(disposition))
if (nrow(undisposed) > 0) {
  print(select(undisposed, claim_id, location, claim))
  stop(nrow(undisposed), " paper_internal rows carry no disposition.")
}
print(ground_truth |> filter(!is.na(defect_locus)) |> count(defect_locus, disposition))

# Float coverage ----
# The article and its online appendix print 24 floats. published_numbers counts the numbers on each
# float's face, leaving out axis ticks and the Study and Dosage label columns of Table A.2; an estimate
# printed with its standard error counts as two. plotted counts a figure's plotted quantities: a
# coefficient, a bubble, or a point each count one, and a trimming bound counts one per end. Both were
# read off the published PDFs on 2026-11-13. The pipeline rebuilds no float, so plotted_reproduced and
# reproduced_by_archive are NA, and covered counts only the prose claims that state a number the float
# prints.
floats <- tribble(
  ~float, ~published_numbers, ~plotted, ~note,
  "Figure 1", 90L, 45L, NA,
  "Table 1", 0L, NA, "Prints the text of the scenarios; carries no data.",
  "Figure 2", 24L, NA, "A design diagram; its numbers are the sample sizes and scenario counts in its boxes.",
  "Figure 3", 160L, 80L, NA,
  "Figure 4", 192L, 96L, NA,
  "Figure 5", 8L, 294L, NA,
  "Figure 6", 0L, 33L, NA,
  "Figure 7", 288L, 144L, NA,
  "Figure A.1", 0L, NA, "Screenshots of the twelve ads; carries no data.",
  "Table A.1", 17L, NA, NA,
  "Table A.2", 70L, NA, "Not compared cell by cell; each row's printed cells were summed against dose times N in maintained/output/table_a2_sixth_video.csv.",
  "Figure A.2", 28L, 28L, NA,
  "Figure A.3", 18L, 18L, NA,
  "Figure A.4", 0L, 27L, NA,
  "Figure B.5", 90L, 45L, NA,
  "Table B.3", 36L, NA, NA,
  "Figure B.6", 90L, 45L, NA,
  "Figure C.7", 0L, 140L, NA,
  "Figure C.8", 192L, 96L, NA,
  "Table C.4", 0L, NA, "Prints the text of the consolidation exercise; carries no data.",
  "Figure C.9", 0L, 126L, NA,
  "Table C.5", 52L, NA, NA,
  "Figure C.10", 20L, 735L, NA,
  "Figure C.11", 24L, 882L, NA
)

# The float each prose claim restates. location comes from the extraction and names the page, so this
# key is kept here for coverage only and never written to the ground truth.
float_key <- tribble(
  ~claim_id, ~float,
  "field_ate", "Figure 1", "field_se", "Figure 1",
  "fig_2a_enrolled", "Figure 2",
  "s1_d_guns", "Figure 3", "s1_d_health", "Figure 3", "s1_r_health", "Figure 3", "s1_r_economy", "Figure 3",
  "s2_d_guns", "Figure 4", "s2_d_health", "Figure 4", "s2_r_guns", "Figure 4", "s2_r_camps", "Figure 4",
  "box_r_1", "Figure 5", "box_r_2", "Figure 5", "box_d_1", "Figure 5", "box_d_2", "Figure 5",
  "status_quo_low", "Figure 5", "status_quo_high", "Figure 5",
  "post_d", "Figure 7", "post_d_se", "Figure 7", "post_r", "Figure 7", "post_r_se", "Figure 7",
  "s1_index_largest", "Figure 7"
)
stopifnot(all(float_key$claim_id %in% ground_truth$claim_id), all(float_key$float %in% floats$float))

float_claims <- ground_truth |>
  inner_join(float_key, by = "claim_id") |>
  summarise(covered = n(), reproduced_by_rewrite = sum(match_rewrite), .by = float)

float_coverage <- floats |>
  left_join(float_claims, by = "float") |>
  mutate(covered = replace_na(covered, 0L),
         reproduced_by_rewrite = replace_na(reproduced_by_rewrite, 0L),
         reproduced_by_archive = NA_integer_,
         plotted_reproduced = NA_integer_,
         note = case_when(!is.na(note) ~ note,
                          covered > 0 ~ "Not compared against a rerun; covered counts the prose claims that restate a number on its face.",
                          .default = "Not compared against a rerun.")) |>
  select(float, published_numbers, covered, reproduced_by_rewrite, reproduced_by_archive,
         plotted, plotted_reproduced, note)
stopifnot(nrow(float_coverage) == 24, !anyDuplicated(float_coverage$float))
write_csv(float_coverage, here::here("ground_truth", "float_coverage.csv"))
print(float_coverage, n = Inf)

# The ground truth, in the canonical column order ----
ground_truth <- ground_truth |>
  mutate(value_script = NA_real_, match = NA, holds = NA) |>
  select(paper_id, claim_id, location, claim_type, claim, value_paper, digits, value_script, match,
         value_rewrite, match_rewrite, holds, defect_locus, disposition, disposition_reason, notes)
write_csv(ground_truth, here::here("ground_truth", "coppock_green_porter_2026_ground_truth.csv"))
print(count(ground_truth, match_rewrite))
print(filter(ground_truth, !match_rewrite) |> select(claim_id, value_paper, value_rewrite, disposition), n = 30)
