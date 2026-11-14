# coppock_green_porter_2026/maintained/figure_c8_meta_analysis_hypotheticals.R
# Output: output/figure_c8_meta_analysis_hypotheticals.{pdf,png}, output/estimates_meta_analysis_hypotheticals.csv
# Depends on: output/estimates_hypotheticals_study_{1,2}.csv, helpers.R
# Description: The random-effects pool of the two studies' effects on hypothetical party ID, by scenario,
#   overall and by pre-treatment party ID. Camps was asked only in Study 2, so its pool is Study 2 alone.

source(here::here("maintained", "helpers.R"))

estimates <- bind_rows(
  study_1 = read_csv(here::here("maintained", "output", "estimates_hypotheticals_study_1.csv"), show_col_types = FALSE),
  study_2 = read_csv(here::here("maintained", "output", "estimates_hypotheticals_study_2.csv"), show_col_types = FALSE),
  .id = "study"
)

# Random-effects pool (REML, rma.uni's default, as the deposit uses).
# tidy() returns its own term column, so the treatment contrast is grouped under another name.
pooled <- estimates |>
  rename(contrast = term) |>
  group_by(scenario, pid_7_factor_pre, contrast) |>
  reframe(tidy(rma.uni(estimate ~ 1, sei = std.error, data = pick(everything()), method = "REML"), conf.int = TRUE)) |>
  mutate(term = contrast) |>
  select(scenario, pid_7_factor_pre, term, estimate, std.error, conf.low, conf.high)
write_csv(pooled, here::here("maintained", "output", "estimates_meta_analysis_hypotheticals.csv"))

# Figure ----
gg_df <- pooled |>
  mutate(pid_7_factor_pre = factor(pid_7_factor_pre, levels = pid_7_levels),
         scenario = factor(scenario, levels = scenario_levels),
         label_x = if_else(term == "Pro-Democratic hypotheticals", -1, 1),
         hjust = if_else(term == "Pro-Democratic hypotheticals", 1, 0),
         entry = sprintf("%.2f (%.2f)", estimate, std.error))

g <- ggplot(gg_df, aes(estimate, pid_7_factor_pre, color = term, shape = term)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.25)) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high), position = position_dodge(width = 0.25)) +
  geom_text(aes(x = label_x, label = entry, hjust = hjust), nudge_y = 0.25, size = 2, show.legend = FALSE) +
  scale_color_manual(values = bpr_colors[c(1, 3)]) +
  coord_cartesian(xlim = c(-2, 2)) +
  facet_wrap(~scenario, nrow = 3) +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank(), axis.title.y = element_blank()) +
  labs(x = "Estimated average effect on hypothetical party id\n(1 = Strong Democrat, 7 = Strong Republican)")

save_figure(g, "figure_c8_meta_analysis_hypotheticals", width = 6.5, height = 6.5)
