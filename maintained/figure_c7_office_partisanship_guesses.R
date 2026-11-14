# coppock_green_porter_2026/maintained/figure_c7_office_partisanship_guesses.R
# Output: output/figure_c7_office_partisanship_guesses.{pdf,png}, output/estimates_office_guesses.csv
# Depends on: original/clean_hypotheticals_study_{1,2}.rds, original/{darryl,pam,michael,dwight,angela}.jpg,
#   helpers.R
# Description: The average guess of five characters' party ID from The Office, by the respondent's own
#   party ID and whether the respondent has watched the show, in Studies 1 and 2. Each mean is an
#   intercept-only lm_robust with HC2 standard errors.

source(here::here("maintained", "helpers.R"))

characters <- c("darryl", "pam", "michael", "dwight", "angela")

# Estimates ----
estimates <- bind_rows(`Study 1` = read_rds(here::here("original", "clean_hypotheticals_study_1.rds")),
                       `Study 2` = read_rds(here::here("original", "clean_hypotheticals_study_2.rds")),
                       .id = "study") |>
  select(study, resp_id, pid_7_pre, office_intro, all_of(str_c("office_", characters))) |>
  pivot_longer(starts_with("office_") & !office_intro, names_to = "name", names_prefix = "office_") |>
  group_by(study, pid_7_pre, name, office_intro) |>
  reframe(tidy(lm_robust(value ~ 1, data = pick(everything()), se_type = "HC2"))) |>
  select(study, name, office_intro, pid_7_pre, estimate, std.error, conf.low, conf.high, df)
write_csv(estimates, here::here("maintained", "output", "estimates_office_guesses.csv"))

# Figure ----
# Each character's photo sits in the corner of its row away from the data: top left for the two the
# respondents place on the Democratic side, bottom left for the other three.
image_df <- tibble(name = factor(characters, levels = characters),
                   image = here::here("original", str_c(characters, ".jpg")),
                   x = 2, y = c(6, 6, 2, 2, 2))

gg_df <- estimates |>
  mutate(name = factor(name, levels = characters))

pos <- position_dodge(width = 0.4)

g <- ggplot(gg_df, aes(pid_7_pre, estimate, group = office_intro, color = office_intro)) +
  geom_abline(slope = 1, intercept = 0, alpha = 0.5, linetype = "dashed") +
  geom_point(position = pos) +
  geom_line(position = pos) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), position = pos) +
  geom_image(data = image_df, aes(x = x, y = y, image = image, group = NULL, color = NULL), size = 0.5) +
  scale_color_manual(name = "Do you watch The Office or\nhave you ever watched The Office?",
                     values = c("grey", "black")) +
  scale_x_continuous(breaks = 1:7) +
  scale_y_continuous(breaks = 1:7, limits = c(1, 7)) +
  coord_equal() +
  facet_grid(name ~ study) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom") +
  labs(x = "Respondent partisanship\n1 = Strong Democrat, 7 = Strong Republican",
       y = "Average partisanship guess")

save_figure(g, "figure_c7_office_partisanship_guesses", width = 6.5, height = 9)
