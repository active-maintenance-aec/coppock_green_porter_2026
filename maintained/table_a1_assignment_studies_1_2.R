# coppock_green_porter_2026/maintained/table_a1_assignment_studies_1_2.R
# Output: output/table_a1_assignment_studies_1_2.{csv,tex}
# Depends on: original/clean_survey_experiments.rds, helpers.R
# Description: Wave 2 respondents in Studies 1 and 2 by assigned video.

source(here::here("maintained", "helpers.R"))

five_studies <- read_rds(here::here("original", "clean_survey_experiments.rds"))

# Table ----
tab_assignment <- five_studies |>
  filter(study %in% c("study_1_mturk", "study_2_mturk"), responded_post == 1) |>
  count(Z_fac, study) |>
  pivot_wider(names_from = study, values_from = n)
write_csv(tab_assignment, here::here("maintained", "output", "table_a1_assignment_studies_1_2.csv"))

tab_assignment |>
  mutate(across(starts_with("study"), \(x) replace_na(as.character(x), ""))) |>
  kable(format = "latex", booktabs = TRUE, col.names = c("Video", "Study 1", "Study 2")) |>
  write_lines(here::here("maintained", "output", "table_a1_assignment_studies_1_2.tex"))
print(tab_assignment, n = Inf)
