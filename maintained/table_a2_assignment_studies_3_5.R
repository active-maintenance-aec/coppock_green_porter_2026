# coppock_green_porter_2026/maintained/table_a2_assignment_studies_3_5.R
# Output: output/table_a2_assignment_studies_3_5.{csv,tex}
# Depends on: original/clean_survey_experiments.rds, helpers.R
# Description: Respondents in Studies 3 to 5 by number of ads, and how many of them saw each ad. The column
#   order follows the published table, whose headers were typed into the paper rather than written here.

source(here::here("maintained", "helpers.R"))

five_studies <- read_rds(here::here("original", "clean_survey_experiments.rds"))

# Table ----
# Corrected: the deposit summed five of the six ad indicators, leaving out notaparty (Negative Partisanship),
# so no row's cells reached dose times N (errata entry 10). The column is added before Placebo.
tab_assignment <- five_studies |>
  filter(study %in% c("study_3_dosage", "study_4_dosage", "study_5_dosage"), !is.na(dosage_factor)) |>
  summarise(n = n(), across(c(presidents, share_america, facts, guns, weare, notaparty, placebo), sum),
            .by = c(study, dosage_continuous)) |>
  arrange(study, dosage_continuous) |>
  mutate(study = str_replace(study, "^study_(\\d)_dosage$", "Study \\1"))
write_csv(tab_assignment, here::here("maintained", "output", "table_a2_assignment_studies_3_5.csv"))

tab_assignment |>
  kable(format = "latex", booktabs = TRUE,
        col.names = c("Study", "Ads", "N", "Charisma", "Social Identity", "Performance", "Issues", "Generic",
                      "Negative Partisanship", "Placebo")) |>
  write_lines(here::here("maintained", "output", "table_a2_assignment_studies_3_5.tex"))
print(tab_assignment, n = Inf)
