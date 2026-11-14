# coppock_green_porter_2026/maintained/figure_b6_field_experiment_block_fe.R
# Output: output/figure_b6_field_experiment_block_fe.{pdf,png}, output/estimates_field_experiment_block_fe.csv
# Depends on: original/clean_field_experiments.rds, helpers.R
# Description: Figure 1's estimates again, with block fixed effects in place of the party and organization
#   covariates.

source(here::here("maintained", "helpers.R"))

field <- read_rds(here::here("original", "clean_field_experiments.rds"))
outcomes <- c("pid_7_rev_post", "partisan_evaluation_index_post", "partisan_identification_index_post")

# Estimates ----
# Fixed effects for the blocks as they stood after attrition; household-clustered standard errors with the
# Stata small-sample correction, as the deposit computes them.
fit_z <- function(data, outcome) {
  lm_robust(reformulate("Z", outcome), data = data, fixed_effects = ~blocks_after_attrition,
            clusters = hhid_anon, se_type = "stata") |>
    tidy() |>
    filter(term == "Z")
}

estimates <- map(outcomes, \(y) bind_rows(
  fit_z(field, y),
  field |> group_by(org) |> reframe(fit_z(pick(everything()), y)),
  field |> group_by(pid_3_pre_nona) |> reframe(fit_z(pick(everything()), y)),
  field |> group_by(pid_3_pre_nona, org) |> reframe(fit_z(pick(everything()), y))
)) |>
  list_rbind() |>
  mutate(party = replace_na(pid_3_pre_nona, "Overall"), org = replace_na(org, "Pooled")) |>
  select(outcome, org, party, estimate, std.error, statistic, p.value, conf.low, conf.high, df)
write_csv(estimates, here::here("maintained", "output", "estimates_field_experiment_block_fe.csv"))

# Figure ----
gg_df <- estimates |>
  mutate(org = factor(org, levels = c("org1", "org2", "Pooled"),
                      labels = c("Organization 1", "Organization 2", "Pooled")),
         party = factor(party, levels = c("Overall", "Democrat", "Independent", "Republican", "Unknown")),
         entry = str_c(party, ":\n", sprintf("%.3f (%.3f)", estimate, std.error)),
         outcome_label = factor(outcome, levels = outcomes,
                                labels = c("Party ID\n(reversed\n1: SR, 7: SD)",
                                           "Partisan\nEvaluation\nScale (0-1)",
                                           "Partisan\nIdentification\nScale (0-1)")))

g <- ggplot(gg_df, aes(estimate, outcome_label, group = party, color = party)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.2) +
  geom_point(position = position_dodge(width = 0.85)) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high), position = position_dodge(width = 0.85), alpha = 0.25) +
  geom_text(aes(x = -0.05, label = entry), position = position_dodge(width = 0.85), hjust = 1, size = 2) +
  scale_color_manual(values = c("black", "blue", "purple", "red", "darkgrey")) +
  scale_x_continuous(breaks = c(-0.10, 0, 0.10)) +
  coord_cartesian(xlim = c(-0.15, 0.15)) +
  facet_wrap(~org) +
  theme_bw() +
  theme(legend.position = "none", axis.title.y = element_blank(), axis.text.x = element_text(size = 5),
        panel.grid.major = element_line(linewidth = 0.2), panel.grid.minor = element_blank()) +
  labs(x = "Average causal effect estimate and 95% CI")

save_figure(g, "figure_b6_field_experiment_block_fe", width = 6.5, height = 4.5)
