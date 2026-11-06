# coppock_green_porter_2026/ground_truth/build_ground_truth.R
# Output: ground_truth/coppock_green_porter_2026_ground_truth.csv
# Depends on: ground_truth/published_claims.csv, errata_entries.csv, maintained/output/page_values.csv
# Description: Compare the extracted prose values with the rerun at the page's precision, then halt on a
#   mismatch the spine does not report, a spine id with no row, or a spine id on a claim that holds. The
#   values come from the deposit's cleaned data, so a defect both inherit would pass; raw_to_clean.R tests
#   the two hypothetical studies' cleaning only. No figure or table cell is in this gate.

library(tidyverse)
library(here)
here::i_am("ground_truth/build_ground_truth.R")

claims <- read_csv(here::here("ground_truth", "published_claims.csv"), col_types = cols(.default = col_character(), digits = col_integer()))
values <- read_csv(here::here("maintained", "output", "page_values.csv"), show_col_types = FALSE)
spine <- read_csv(here::here("errata_entries.csv"), show_col_types = FALSE)
spine_ids <- spine$claim_ids |> na.omit() |> str_split(";") |> unlist() |> discard(\(x) x == "")

ground_truth <- claims |>
  left_join(values, by = "claim_id", relationship = "one-to-one") |>
  mutate(printed = sprintf(str_c("%.", digits, "f"), value_rewrite),
         match_rewrite = as.numeric(printed == value_paper),
         defect_locus = case_when(match_rewrite == 0 ~ "paper_internal", .default = NA_character_),
         paper_id = "coppock_green_porter_2026") |>
  select(paper_id, claim_id, location, claim, value_paper, value_rewrite, match_rewrite, defect_locus)

stopifnot(all(!is.na(ground_truth$value_rewrite)),
          all(spine_ids %in% ground_truth$claim_id),
          all(ground_truth$claim_id[ground_truth$match_rewrite == 0] %in% spine_ids),
          all(ground_truth$match_rewrite[ground_truth$claim_id %in% spine_ids] == 0))
write_csv(ground_truth, here::here("ground_truth", "coppock_green_porter_2026_ground_truth.csv"))
print(count(ground_truth, match_rewrite))
print(filter(ground_truth, match_rewrite == 0) |> select(claim_id, value_paper, value_rewrite), n = 30)
