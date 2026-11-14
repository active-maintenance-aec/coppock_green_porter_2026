# coppock_green_porter_2026/maintained/figure_3_hypotheticals_study_1.R
# Output: output/figure_3_hypotheticals_study_1.{pdf,png}, output/estimates_hypotheticals_study_1.csv
# Depends on: original/clean_hypotheticals_study_1.rds, helpers.R
# Description: Study 1's effects of pro-Democratic and pro-Republican scenarios on hypothetical party ID,
#   by scenario, overall and by pre-treatment party ID. Figure C.8 pools these estimates with Study 2's.

source(here::here("maintained", "helpers.R"))

study_1 <- read_rds(here::here("original", "clean_hypotheticals_study_1.rds"))
scenarios <- c(violence_hyp = "Violence", ukraine_hyp = "Ukraine", economy_hyp = "Economy",
               medical_hyp = "Healthcare", guns_hyp = "Guns")

estimates <- names(scenarios) |>
  map(\(y) estimate_by_pid(study_1, y, covariates = c("pid_7_factor_pre", "batch_pre"))) |>
  list_rbind() |>
  mutate(scenario = scenarios[outcome])
write_csv(estimates, here::here("maintained", "output", "estimates_hypotheticals_study_1.csv"))

# Figure ----
gg_df <- estimates |>
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
  theme(legend.position = "inside", legend.position.inside = c(0.75, 0.2),
        legend.title = element_blank(), axis.title.y = element_blank()) +
  labs(x = "Estimated average effect on hypothetical party id\n(1 = Strong Democrat, 7 = Strong Republican)")

save_figure(g, "figure_3_hypotheticals_study_1", width = 6.5, height = 6.5)
