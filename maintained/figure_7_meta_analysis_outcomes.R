# coppock_green_porter_2026/maintained/figure_7_meta_analysis_outcomes.R
# Output: output/figure_7_meta_analysis_outcomes.{pdf,png}, output/estimates_meta_analysis_outcomes.csv
# Depends on: output/estimates_post_treatment_outcomes.csv, helpers.R
# Description: The post-treatment effects in each study and their random-effects pool across the two,
#   overall and by pre-treatment party ID.

source(here::here("maintained", "helpers.R"))

estimates <- read_csv(here::here("maintained", "output", "estimates_post_treatment_outcomes.csv"), show_col_types = FALSE)

# Random-effects pool (REML, rma.uni's default, as the deposit uses) of the two studies' estimates.
# tidy() returns its own term column, so the treatment contrast is grouped under another name.
pooled <- estimates |>
  rename(contrast = term) |>
  group_by(outcome, pid_7_factor_pre, contrast) |>
  reframe(tidy(rma.uni(estimate ~ 1, sei = std.error, data = pick(everything()), method = "REML"), conf.int = TRUE)) |>
  mutate(study = "meta", term = contrast) |>
  select(study, outcome, pid_7_factor_pre, term, estimate, std.error, conf.low, conf.high)
write_csv(pooled, here::here("maintained", "output", "estimates_meta_analysis_outcomes.csv"))

# Figure ----
outcome_labels <- c(
  pid_7_post = "Party identification (1 - 7)\n(1 = Strong Democrat, 7 = Strong Republican)",
  partisan_evaluation_index_post = "Partisan Evaluation Index (0 - 1)\nhigher values higher evaluation of Ds",
  partisan_identification_index_post = "Partisan Identification Index (0 - 1)\nhigher values stronger identification with Ds"
)
gg_df <- bind_rows(estimates, pooled) |>
  mutate(pid_7_factor_pre = factor(pid_7_factor_pre, levels = pid_7_levels),
         facet_label = outcome_labels[outcome],
         study_label = factor(study, levels = c("study_1", "study_2", "meta"),
                              labels = c("Study 1", "Study 2", "Meta-analysis")),
         pro_dem = term == "Pro-Democratic hypotheticals",
         label_x = if_else(pro_dem == (outcome == "pid_7_post"), -0.06, 0.06),
         hjust = if_else(label_x < 0, 1, 0),
         entry = sprintf("%.3f (%.3f)", estimate, std.error))

g <- ggplot(gg_df, aes(estimate, pid_7_factor_pre, color = term, shape = term)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.25) +
  geom_point(position = position_dodge(width = 0.25)) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high), position = position_dodge(width = 0.25)) +
  geom_text(aes(x = label_x, label = entry, hjust = hjust), nudge_y = 0.25, size = 2, show.legend = FALSE) +
  scale_color_manual(values = bpr_colors[c(1, 3)]) +
  coord_cartesian(xlim = c(-0.25, 0.25)) +
  facet_grid(cols = vars(study_label), rows = vars(facet_label)) +
  theme_bw() +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(), legend.title = element_blank(),
        strip.text = element_text(size = 7), axis.title.y = element_blank()) +
  labs(x = "Estimated average effect on main outcomes")

save_figure(g, "figure_7_meta_analysis_outcomes", width = 6.5, height = 7.5)
