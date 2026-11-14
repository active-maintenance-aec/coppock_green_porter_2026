# coppock_green_porter_2026/maintained/page_values.R
# Output: maintained/output/page_values.csv, maintained/output/one_sided_tests.csv,
#   maintained/output/table_a2_sixth_video.csv
# Depends on: original/ (the deposit's data), the rewrite's estimates in maintained/output/, and
#   maintained/archive_run/stripped/replication_archive/ for the deposit's shipped one-sided p only
# Description: Compute the numbers the article's prose states and the errata needs, from the deposit's data
#   and the rewrite's estimates. It also repairs the deposit's one-sided test. The deposit's Study 1
#   column is NA on every Pro-Democratic row, because it matches a label already renamed, and it tests the
#   0-1 indexes in the wrong direction, because a higher index is more Democratic. That repair is mine.

library(tidyverse)
library(estimatr)
library(metafor)
library(slider)
library(broom)
library(here)
here::i_am("maintained/page_values.R")

archive <- here::here("maintained", "archive_run", "stripped", "replication_archive")
data_file <- \(file) read_rds(here::here("original", file))
estimates_file <- \(file) read_csv(here::here("maintained", "output", file), show_col_types = FALSE)
study_1 <- data_file("clean_hypotheticals_study_1.rds")
study_2 <- data_file("clean_hypotheticals_study_2.rds")
long_1 <- data_file("clean_hypotheticals_study_1_long.rds")
long_2 <- data_file("clean_hypotheticals_study_2_long.rds")
survey <- data_file("clean_survey_experiments.rds")
field <- data_file("clean_field_experiments.rds")
macro <- read_csv(here::here("original", "macro_data.csv"), show_col_types = FALSE)
values <- list()
put <- function(...) values <<- c(values, list(...))

# Samples and the evaluation index ----
put(n_study_1 = nrow(study_1), n_study_2 = nrow(study_2),
    fig_2a_enrolled = sum(count(study_1, batch_pre)$n),
    fig_2b_hypotheticals = sum(str_detect(names(study_2), "_value_hyp$")),
    sd_pid = (sd(study_1$pid_7_pre) + sd(study_2$pid_7_pre)) / 2)
index_items <- c("therm_harris_s_post", "therm_obama_s_post", "therm_biden_s_post", "therm_aoc_s_post",
                 "therm_trump_s_post", "therm_mtg_s_post", "therm_desantis_s_post", "therm_romney_s_post",
                 "therm_pence_s_post", "therm_dems_s_post", "therm_reps_s_post", "vote_dem_2024_s_post",
                 "rep_favorability_s_post", "dem_favorability_s_post")
index_gap <- \(d) max(abs(rowMeans(d[index_items], na.rm = TRUE) - d$partisan_evaluation_index_post), na.rm = TRUE)
stopifnot(index_gap(study_1) < 1e-12, index_gap(study_2) < 1e-12)
put(index_items = length(index_items))
listed_items <- setdiff(index_items, c("therm_dems_s_post", "therm_reps_s_post"))
listed_fit <- study_2 |>
  mutate(listed = rowMeans(pick(all_of(listed_items)), na.rm = TRUE)) |>
  lm_robust(listed ~ treatment + pid_7_factor_pre + batch_pre, data = _) |> tidy() |>
  filter(term == "treatmentPro-Democrat hypotheticals")
put(listed_index_estimate = listed_fit$estimate, listed_index_se = listed_fit$std.error)

# Field identification index: three items where the third was asked (organization 2), so p. 32 holds ----
field_gap <- field |>
  mutate(three = rowMeans(pick(pid_7_s_post, rep_self_description_s_post, dem_self_description_s_post), na.rm = TRUE)) |>
  summarise(gap = max(abs(three - partisan_identification_index_post), na.rm = TRUE)) |> pull(gap)
stopifnot(field_gap < 1e-9)

# Switching: the deposit's four contrasts and random-effects pool ----
switch_fit <- function(data, items, party) {
  data |> pivot_longer(all_of(items)) |> filter(pid_3_pre == party) |>
    lm_lin(value ~ treatment, covariates = ~ pid_7_factor_pre + batch_pre, clusters = resp_id, data = _) |> tidy()
}
hyps <- c("violence", "ukraine", "economy", "medical", "guns")
fits <- list(switch_fit(study_1, str_c(hyps, "_hyp_dem"), "Republican"),
             switch_fit(study_1, str_c(hyps, "_hyp_rep"), "Democrat"),
             switch_fit(study_2, str_c(c(hyps, "camps"), "_hyp_dem"), "Republican"),
             switch_fit(study_2, str_c(c(hyps, "camps"), "_hyp_rep"), "Democrat"))
terms <- c("treatmentPro-Democrat hypotheticals", "treatmentPro-Republican hypotheticals")
gap <- map2_dfr(fits, terms[c(1, 2, 1, 2)], \(f, t) filter(f, term == t))
status_quo <- map_dfr(fits, \(f) filter(f, term == "(Intercept)"))
gap_fit <- rma.uni(estimate ~ 1, sei = std.error, data = gap)
status_quo_fit <- rma.uni(estimate ~ 1, sei = std.error, data = status_quo)
put(switch_gap = 100 * coef(gap_fit), switch_gap_se = 100 * gap_fit$se, switch_status_quo = 100 * coef(status_quo_fit),
    switch_treated = 100 * (coef(gap_fit) + coef(status_quo_fit)))

# Hypothetical effects ----
hyp <- bind_rows(s1 = estimates_file("estimates_hypotheticals_study_1.csv"),
                 s2 = estimates_file("estimates_hypotheticals_study_2.csv"), .id = "study") |>
  filter(pid_7_factor_pre == "Overall") |> mutate(term = str_extract(term, "Pro-[A-Z]"))
effect <- \(s, t, f) hyp$estimate[hyp$study == s & hyp$term == t & hyp$scenario == f]
pooled <- \(t) { d <- filter(hyp, term == t); coef(rma.uni(estimate ~ 1, sei = std.error, data = d)) }
put(preview_rep = pooled("Pro-R"), preview_dem = abs(pooled("Pro-D")),
    s1_d_guns = effect("s1", "Pro-D", "Guns"), s1_d_health = effect("s1", "Pro-D", "Healthcare"),
    s1_r_health = effect("s1", "Pro-R", "Healthcare"), s1_r_economy = effect("s1", "Pro-R", "Economy"),
    s2_d_guns = effect("s2", "Pro-D", "Guns"), s2_d_health = effect("s2", "Pro-D", "Healthcare"),
    s2_r_guns = effect("s2", "Pro-R", "Guns"), s2_r_camps = effect("s2", "Pro-R", "Camps"))
extreme_holds <- hyp |> filter(!is.na(estimate)) |> group_by(study, term) |>
  summarise(lo = scenario[which.min(abs(estimate))], hi = scenario[which.max(abs(estimate))], .groups = "drop")
stopifnot(extreme_holds$lo == c("Guns", "Healthcare", "Guns", "Guns"), extreme_holds$hi == c("Healthcare", "Economy", "Healthcare", "Camps"))

first_cor <- \(d) d |> filter(treatment == "Status quo hypotheticals") |> summarise(r = cor(pid_7_pre, first_value_hyp, use = "complete.obs")) |> pull(r)
put(cor_1 = first_cor(study_1), cor_2 = first_cor(study_2))

# Figure 5's boxes ----
boxes <- bind_rows(s1 = long_1, s2 = long_2, .id = "study") |> group_by(study, treatment) |>
  summarise(r_to_d = 100 * mean(value[pid_7_pre %in% 5:7] %in% 1:3), d_to_r = 100 * mean(value[pid_7_pre %in% 1:3] %in% 5:7), .groups = "drop")
box <- \(s, t, v) boxes[[v]][boxes$study == s & boxes$treatment == t]
out_party <- c(box("s1", "Pro-Democrat hypotheticals", "r_to_d"), box("s2", "Pro-Democrat hypotheticals", "r_to_d"),
               box("s1", "Pro-Republican hypotheticals", "d_to_r"), box("s2", "Pro-Republican hypotheticals", "d_to_r"))
status_quo_rates <- boxes |> filter(treatment == "Status quo hypotheticals") |> select(r_to_d, d_to_r) |> unlist()
put(box_r_1 = out_party[1], box_r_2 = out_party[2], box_d_1 = out_party[3], box_d_2 = out_party[4],
    status_quo_low = min(status_quo_rates), status_quo_high = max(status_quo_rates))

# Single-video experiments and the field experiment ----
single <- \(s) tidy(lm_robust(pid_7_post ~ any_treat, data = filter(survey, study == s))) |> filter(term == "any_treat")
s1_ad <- single("study_1_mturk"); s2_ad <- single("study_2_mturk")
put(ad_1 = s1_ad$estimate, ad_1_se = s1_ad$std.error, ad_2 = s2_ad$estimate, ad_2_se = s2_ad$std.error)
field_fit <- lm_robust(pid_7_rev_post ~ Z + pid_3_pre_nona + org, clusters = hhid_anon, data = field)
field_row <- tidy(field_fit) |> filter(term == "Z")
put(field_ate = field_row$estimate, field_se = field_row$std.error)
videos <- c("Issues (Firm A)", "Performance (Firm A)", "Charisma (Firm A)", "Social Identity (Firm A)", "Generic (DNC)",
            "Performance (Firm B)", "Charisma (Firm B)", "Social Identity (Firm B)", "Issues (Firm B, version 1)",
            "Issues (Firm B, version 2)", "Issues (Firm B, version 3)", "Negative Partisanship")
stopifnot(all(setdiff(videos, "Negative Partisanship") %in% survey$Z_fac),
          "Negative Partisanship" %in% survey$video_1)
put(ads_tested = length(videos))

# Post-treatment effects, the one-sided tests, and what the page calls null ----
# p.value_alt is the deposit's shipped one-sided p, which only the archive's rerun carries.
shipped_p <- bind_rows(s1 = read_rds(file.path(archive, "ates_study_1.rds")),
                       s2 = read_rds(file.path(archive, "ates_study_2.rds")), .id = "study") |>
  transmute(study, outcome, pid_7_factor_pre = as.character(pid_7_factor_pre), term, p.value_alt)
ates <- estimates_file("estimates_post_treatment_outcomes.csv") |>
  mutate(study = str_replace(study, "^study_", "s")) |>
  left_join(shipped_p, by = join_by(study, outcome, pid_7_factor_pre, term), relationship = "one-to-one") |>
  mutate(is_index = outcome != "pid_7_post", pro_dem = str_starts(term, "Pro-Dem"),
         direction = case_when(pro_dem == is_index ~ 1, .default = -1),
         p_repaired = pt(direction * statistic, df = df, lower.tail = FALSE),
         p_bh = p.adjust(p_repaired, "BH"))
write_csv(select(ates, study, pid_7_factor_pre, outcome, term, estimate, std.error, p.value, p.value_alt, p_repaired, p_bh),
          here::here("maintained", "output", "one_sided_tests.csv"))
overall <- \(s, y, d) ates |> filter(study == s, outcome == y, pro_dem == d, pid_7_factor_pre == "Overall")
pid_d <- overall("s1", "pid_7_post", TRUE); pid_r <- overall("s1", "pid_7_post", FALSE)
eval_d <- overall("s2", "partisan_evaluation_index_post", TRUE); id_d <- overall("s2", "partisan_identification_index_post", TRUE)
subgroup <- ates |> filter(pid_7_factor_pre != "Overall")
lone <- subgroup |> filter(p_repaired <= 0.025)
stopifnot(nrow(lone) == 1, lone$study == "s1", as.character(lone$pid_7_factor_pre) == "Not very strong Republican")
s1_indexes <- ates |> filter(study == "s1", is_index, pid_7_factor_pre == "Overall")
se_pid_2 <- overall("s2", "pid_7_post", TRUE)$std.error; se_pid_2r <- overall("s2", "pid_7_post", FALSE)$std.error
power_at <- \(se) pnorm(0.05 / se - qnorm(0.975)) + pnorm(-0.05 / se - qnorm(0.975))
put(post_d = pid_d$estimate, post_d_se = pid_d$std.error, post_r = pid_r$estimate, post_r_se = pid_r$std.error,
    subgroup_significant = nrow(lone), subgroup_p = lone$p_repaired, subgroup_bh = lone$p_bh,
    s1_index_largest = 100 * max(abs(s1_indexes$estimate)),
    s2_eval_d = eval_d$estimate, s2_eval_d_se = eval_d$std.error, s2_eval_d_p = eval_d$p_repaired, s2_eval_d_shipped = eval_d$p.value_alt, s2_eval_d_bh = eval_d$p_bh,
    s2_id_d = id_d$estimate, s2_id_d_se = id_d$std.error, s2_id_d_p = id_d$p_repaired, s2_id_d_shipped = id_d$p.value_alt, s2_id_d_bh = id_d$p_bh,
    s2_overall_below = sum(ates$p_repaired[ates$study == "s2" & ates$pid_7_factor_pre == "Overall"] <= 0.025),
    s1_pro_dem_na = sum(is.na(ates$p.value_alt[ates$study == "s1" & ates$pro_dem])),
    power_d = power_at(se_pid_2), power_r = power_at(se_pid_2r), se_pid_d = se_pid_2, se_pid_r = se_pid_2r)

# Macropartisanship ----
mp <- estimates_file("estimates_macropartisanship.csv") |> filter(hypothetical_label == "Overall")
put(mp_dem = 100 * coef(rma.uni(mp$MP_D, sei = mp$MP_D_SE)), mp_rep = abs(100 * coef(rma.uni(mp$MP_R, sei = mp$MP_R_SE))))
series <- macro |> transmute(year, date, value = 100 * macropartisanship) |> arrange(year, date) |> mutate(index = row_number())
swing <- \(from, to, sign) {
  window <- series |> filter(year >= from, year <= to)
  sign * max(sign * outer(window$value, window$value, \(a, b) b - a)[outer(window$index, window$index, \(a, b) b > a & b - a <= 40)])
}
put(mp_high = max(series$value), mp_low = min(series$value), mp_range = max(series$value) - min(series$value),
    high_year = series$year[which.max(series$value)], low_year = series$year[which.min(series$value)],
    swing_68_77 = swing(1968, 1977, 1), swing_56_65 = swing(1956, 1965, 1), swing_03_09 = swing(2003, 2009, 1),
    swing_77_85 = swing(1977, 1985, -1), swing_65_68 = swing(1965, 1968, -1),
    swing_93_02_printed = series$value[series$date == "2002q1"] - series$value[series$date == "1993q1"],
    swing_third_republican = swing(1990, 2003, -1), third_from = series$year[series$date == "1997q3"], third_to = series$year[series$date == "2003q4"])
third <- filter(series, year >= 1990, year <= 2003)
stopifnot(abs((series$value[series$date == "2003q4"] - series$value[series$date == "1997q3"]) - swing(1990, 2003, -1)) < 1e-9)

# Status quo probabilities ----
probabilities <- study_2 |>
  pivot_longer(c(prob_camps_2, prob_economy_2, prob_guns_2, prob_healthcare_2, prob_ukraine_2, prob_violence_2)) |>
  filter(treatment == "Status quo hypotheticals") |> group_by(pid_7_factor_pre, name) |> summarise(m = mean(value, na.rm = TRUE), .groups = "drop")
put(status_quo_cells = nrow(probabilities), status_quo_above_half = sum(probabilities$m >= 0.5))

# Table A.2's sixth video: dose times N less the five printed columns ----
sixth <- survey |> filter(study %in% c("study_3_dosage", "study_4_dosage", "study_5_dosage"), !is.na(dosage_factor)) |>
  group_by(study, dosage_continuous) |>
  summarise(n = n(), printed = sum(presidents, share_america, facts, guns, weare), sixth = dosage_continuous[1] * n - printed,
            notaparty = sum(str_detect(video_all, "notaparty"), na.rm = TRUE), .groups = "drop")
write_csv(sixth, here::here("maintained", "output", "table_a2_sixth_video.csv"))
put(table_a2_gap_d1 = sixth$sixth[sixth$study == "study_3_dosage" & sixth$dosage_continuous == 1])

page_values <- enframe(values |> unlist(), name = "claim_id", value = "value_rewrite") |>
  mutate(claim_id = str_remove(claim_id, "\\.intrcpt$"))
write_csv(page_values, here::here("maintained", "output", "page_values.csv"))
