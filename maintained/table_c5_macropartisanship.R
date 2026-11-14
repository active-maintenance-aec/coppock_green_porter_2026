# coppock_green_porter_2026/maintained/table_c5_macropartisanship.R
# Output: output/table_c5_macropartisanship.{tex,csv}, output/estimates_macropartisanship.csv
# Depends on: original/clean_hypotheticals_study_{1,2}_long.rds, helpers.R
# Description: Effects of the pro-Democratic and pro-Republican scenarios on macropartisanship (the
#   Democratic share of partisans, counting 1-2 as Democrats and 6-7 as Republicans), by scenario and
#   overall, with standard errors from a respondent-clustered bootstrap.

source(here::here("maintained", "helpers.R"))
set.seed(343)

long <- list(study_1 = read_rds(here::here("original", "clean_hypotheticals_study_1_long.rds")),
             study_2 = read_rds(here::here("original", "clean_hypotheticals_study_2_long.rds")))

macropartisanship <- function(x) sum(x %in% 1:2) / sum(x %in% c(1:2, 6:7))

# Effects against the status quo, by scenario and pooled over scenarios.
mp_effects <- function(data) {
  effects <- \(d) summarise(d,
    MP_D = macropartisanship(value[treatment == "Pro-Democrat hypotheticals"]) -
      macropartisanship(value[treatment == "Status quo hypotheticals"]),
    MP_R = macropartisanship(value[treatment == "Pro-Republican hypotheticals"]) -
      macropartisanship(value[treatment == "Status quo hypotheticals"]))
  bind_rows(data |> group_by(hypothetical_label) |> effects(),
            data |> effects() |> mutate(hypothetical_label = "Overall"))
}

# Clustered bootstrap ----
# Resamples respondents, keeping all of a respondent's scenarios together. The deposit drew 988 respondents
# with replacement in Study 1, the article's sample size, where the data hold 995 (errata entry 2); every
# replicate here draws as many respondents as the study has. Nesting each respondent's rows and
# bootstrapping the nested table is the same resampling scheme as group_bootstraps(), which builds its
# splits about a hundred times slower.
sims <- 1000
estimates <- long |>
  map(\(d) {
    replicates <- bootstraps(nest(d, .by = resp_id), times = sims)$splits |>
      map(\(s) mp_effects(unnest(analysis(s), data))) |>
      list_rbind() |>
      summarise(MP_D_SE = sd(MP_D), MP_R_SE = sd(MP_R), .by = hypothetical_label)
    mp_effects(d) |> left_join(replicates, by = join_by(hypothetical_label))
  }) |>
  list_rbind(names_to = "study")
write_csv(estimates, here::here("maintained", "output", "estimates_macropartisanship.csv"))

# Table ----
tab_macropartisanship <- estimates |>
  transmute(study = if_else(study == "study_1", "Study 1", "Study 2"),
            hypothetical = hypothetical_label,
            pro_dem = sprintf("%.2f (%.2f)", MP_D, MP_D_SE),
            pro_rep = sprintf("%.2f (%.2f)", MP_R, MP_R_SE))
write_csv(tab_macropartisanship, here::here("maintained", "output", "table_c5_macropartisanship.csv"))

tab_macropartisanship |>
  select(-study) |>
  kable(format = "latex", booktabs = TRUE,
        col.names = c("Hypothetical", "Effect of pro-Dem hypotheticals", "Effect of pro-Rep hypotheticals")) |>
  pack_rows(index = table(fct_inorder(tab_macropartisanship$study))) |>
  write_lines(here::here("maintained", "output", "table_c5_macropartisanship.tex"))
print(tab_macropartisanship, n = Inf)
