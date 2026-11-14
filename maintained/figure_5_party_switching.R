# coppock_green_porter_2026/maintained/figure_5_party_switching.R
# Output: output/figure_5_party_switching.{pdf,png}, output/figure_5_switching_rates.csv
# Depends on: original/clean_hypotheticals_study_{1,2}_long.rds, helpers.R
# Description: Pre-treatment against hypothetical party ID, one point per respondent and scenario, by study
#   and condition, with the share of each party's identifiers who imagine joining the other.

source(here::here("maintained", "helpers.R"))

gg_df <- switching_points()
squares_df <- switching_boxes(gg_df)
write_csv(select(squares_df, study, treatment, direction, estimate),
          here::here("maintained", "output", "figure_5_switching_rates.csv"))

# The sunflower jitter is deterministic, so the figure needs no seed.
g <- ggplot(gg_df, aes(pid_7_pre, value, color = avg_color)) +
  geom_point(position = position_sunflower(density = 10), size = 0.40, alpha = 0.6, stroke = 0) +
  geom_rect(data = squares_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            inherit.aes = FALSE, color = "black", fill = NA, linewidth = 0.25) +
  geom_text(data = squares_df, aes(x = xlabel, y = ylabel, label = label), inherit.aes = FALSE) +
  facet_grid(cols = vars(treatment), rows = vars(study)) +
  coord_equal() +
  scale_color_identity() +
  scale_x_continuous(breaks = 1:7) +
  scale_y_continuous(breaks = 1:7) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(linewidth = 0.25)) +
  labs(x = "Pre-treatment 7-point party identification\n(1 = Strong Democrat, 7 = Strong Republican)",
       y = "Hypothetical 7-point party identification\n(1 = Strong Democrat, 7 = Strong Republican)")

save_figure(g, "figure_5_party_switching", width = 6.5, height = 5)
print(select(squares_df, study, treatment, direction, estimate))
