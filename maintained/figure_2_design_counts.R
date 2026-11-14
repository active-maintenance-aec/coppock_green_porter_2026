# coppock_green_porter_2026/maintained/figure_2_design_counts.R
# Output: output/figure_2_design_counts.csv
# Depends on: original/clean_hypotheticals_study_{1,2}.rds, helpers.R
# Description: The sample sizes in Figure 2's design diagram: respondents by recruitment batch, by assigned
#   condition, and by condition among those who answered the post-treatment party ID question. The figure
#   itself is a drawing and is not rebuilt.

source(here::here("maintained", "helpers.R"))

studies <- list(`Study 1` = read_rds(here::here("original", "clean_hypotheticals_study_1.rds")),
                `Study 2` = read_rds(here::here("original", "clean_hypotheticals_study_2.rds"))) |>
  map(\(d) d |> mutate(batch_pre = as.character(batch_pre)))

# Batch labels ----
# Corrected: the deposit's cleaning scripts bind hyp_orig_i as the "Republican sample" and hyp_orig_r as the
# "Independent sample", and pre-treatment party ID shows the reverse. The check below confirms the swap
# before the labels are exchanged. The other scripts use batch_pre only as a factor covariate, where its
# labels do not matter.
republican_share <- studies |>
  map_dbl(\(d) mean(d$pid_7_pre[d$batch_pre == "Independent sample"] %in% 5:7))
print(republican_share)
stopifnot(republican_share > 0.9)

studies <- studies |>
  map(\(d) d |> mutate(batch_pre = case_when(batch_pre == "Republican sample" ~ "Independent sample",
                                             batch_pre == "Independent sample" ~ "Republican sample",
                                             .default = batch_pre)))

# Counts ----
design_counts <- studies |>
  map(\(d) bind_rows(
    count(d, group = as.character(batch_pre)) |> mutate(count = "enrolled by batch"),
    count(d, group = as.character(treatment)) |> mutate(count = "assigned"),
    d |> filter(!is.na(pid_7_post)) |> count(group = as.character(treatment)) |> mutate(count = "measured")
  )) |>
  list_rbind(names_to = "study") |>
  select(study, count, group, n)

write_csv(design_counts, here::here("maintained", "output", "figure_2_design_counts.csv"))
print(design_counts, n = Inf)
