# coppock_green_porter_2026/maintained/figure_6_hypothetical_order.R
# Output: output/figure_6_hypothetical_order.{pdf,png}, output/estimates_hypothetical_order.csv
# Depends on: original/clean_hypotheticals_study_{1,2}.rds, helpers.R
# Description: Average hypothetical party ID by the position at which a scenario was shown, by condition.

source(here::here("maintained", "helpers.R"))

positions <- c("first", "second", "third", "fourth", "fifth", "sixth")
studies <- list(`Study 1` = read_rds(here::here("original", "clean_hypotheticals_study_1.rds")),
                `Study 2` = read_rds(here::here("original", "clean_hypotheticals_study_2.rds")))

estimates <- studies |>
  map(\(d) d |>
        pivot_longer(any_of(str_c(positions, "_value_hyp")), names_to = "position") |>
        mutate(hypothetical_number = match(str_remove(position, "_value_hyp"), positions)) |>
        group_by(hypothetical_number, treatment) |>
        reframe(tidy(lm_robust(value ~ 1, data = pick(everything()), se_type = "HC2")))) |>
  list_rbind(names_to = "study") |>
  mutate(treatment = as.character(factor(treatment, levels = treatment_levels, labels = treatment_labels))) |>
  select(study, hypothetical_number, treatment, estimate, std.error, conf.low, conf.high)
write_csv(estimates, here::here("maintained", "output", "estimates_hypothetical_order.csv"))

# Figure ----
gg_df <- estimates |>
  mutate(treatment = factor(treatment, levels = treatment_labels))
label_df <- tibble(treatment = factor(treatment_labels, levels = treatment_labels), hypothetical_number = 3.5,
                   estimate = c(3.1, 4.1, 4.8), study = "Study 2")

g <- ggplot(gg_df, aes(hypothetical_number, estimate, group = treatment, color = treatment,
                       fill = treatment, shape = treatment)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), linetype = "blank", alpha = 0.1) +
  geom_point() +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high)) +
  geom_line() +
  geom_text(data = label_df, aes(label = treatment)) +
  scale_x_continuous(breaks = 1:6) +
  scale_color_manual(values = bpr_colors) +
  scale_fill_manual(values = bpr_colors) +
  facet_grid(cols = vars(study)) +
  theme_bw() +
  theme(strip.background = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(y = "Average hypothetical 7-pt party identification", x = "Hypothetical scenario number")

save_figure(g, "figure_6_hypothetical_order", width = 6.5, height = 3.5)
