# coppock_green_porter_2026/maintained/figure_c11_party_switching_study_2.R
# Output: output/figure_c11_party_switching_study_2.{pdf,png}
# Depends on: original/clean_hypotheticals_study_2_long.rds, helpers.R
# Description: Figure 5's Study 2 row, broken out by scenario.

source(here::here("maintained", "helpers.R"))

gg_df <- switching_points() |> filter(study == "Study 2")
squares_df <- switching_boxes(gg_df, hypothetical_label, label_x = c(2.25, 5.75), drop_pro_dem_box = FALSE)

g <- ggplot(gg_df, aes(pid_7_pre, value, color = avg_color)) +
  geom_point(position = position_sunflower(density = 8), size = 0.80, alpha = 0.6, stroke = 0) +
  geom_rect(data = squares_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            inherit.aes = FALSE, color = "black", fill = NA, linewidth = 0.25) +
  geom_text(data = squares_df, aes(x = xlabel, y = ylabel, label = label), inherit.aes = FALSE) +
  facet_grid(cols = vars(treatment), rows = vars(hypothetical_label)) +
  coord_equal() +
  scale_color_identity() +
  scale_x_continuous(breaks = 1:7) +
  scale_y_continuous(breaks = 1:7) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(linewidth = 0.25)) +
  labs(x = "Pre-treatment 7-point party identification\n(1 = Strong Democrat, 7 = Strong Republican)",
       y = "Hypothetical 7-point party identification\n(1 = Strong Democrat, 7 = Strong Republican)")

save_figure(g, "figure_c11_party_switching_study_2", width = 6.5, height = 6 * 2.1)
