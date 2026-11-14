# coppock_green_porter_2026/maintained/helpers.R
# Output: none
# Depends on: nothing
# Description: Loads every package the rewrite uses and defines the shared pieces: the deposit's
#   palette, the label orders the figures share, the hypothetical-studies estimator that Figures 3 and 4
#   and the post-treatment estimates all run, the switching data behind Figures 5, C.10, and C.11, and
#   the PDF timestamp blanker that makes the pipeline's output byte-stable.

library(here)
library(tidyverse)
library(estimatr)
library(broom)
library(metafor)
library(rsample)
library(vayr)
library(ggimage)
library(knitr)
library(kableExtra)

here::i_am("maintained/helpers.R")

# Palette ----
# The deposit's three anchors, interpolated to seven: one per point of the party ID scale.
bpr_colors <- c("#1F3A93", "#7C2C55", "#D91E18")
palette7 <- colorRampPalette(bpr_colors)(7)

# Label orders ----
pid_7_levels <- c("Strong Democrat", "Not very strong Democrat", "Lean Democrat", "Neither",
                  "Lean Republican", "Not very strong Republican", "Strong Republican", "Overall")
treatment_levels <- c("Pro-Democrat hypotheticals", "Status quo hypotheticals", "Pro-Republican hypotheticals")
treatment_labels <- c("Pro-Democratic hypotheticals", "Status quo hypotheticals", "Pro-Republican hypotheticals")
scenario_levels <- c("Healthcare", "Guns", "Ukraine", "Violence", "Economy", "Camps")

# Hypothetical studies: the ATE and the CATEs by pre-treatment party ID ----
# The ATE adjusts for the covariates named at the call site; each CATE is the unadjusted difference in
# means within one party ID group. Both are lm_robust with HC2. The rows returned are the two treatment
# contrasts against the status quo, with pid_7_factor_pre = "Overall" for the ATE.
estimate_by_pid <- function(data, outcome, covariates) {
  ate <- lm_robust(reformulate(c("treatment", covariates), outcome), data = data, se_type = "HC2") |>
    tidy()
  cates <- data |>
    group_by(pid_7_factor_pre) |>
    reframe(tidy(lm_robust(reformulate("treatment", outcome), data = pick(everything()), se_type = "HC2")))
  bind_rows(cates, ate) |>
    filter(term %in% c("treatmentPro-Democrat hypotheticals", "treatmentPro-Republican hypotheticals")) |>
    mutate(term = if_else(term == "treatmentPro-Democrat hypotheticals",
                          "Pro-Democratic hypotheticals", "Pro-Republican hypotheticals"),
           pid_7_factor_pre = as.character(pid_7_factor_pre) |> replace_na("Overall")) |>
    select(outcome, pid_7_factor_pre, term, estimate, std.error, statistic, p.value, conf.low, conf.high, df)
}

# Party switching: one point per respondent and scenario, colored by origin and destination ----
# Each point's color averages the palette colors of its pre-treatment and hypothetical party ID.
average_colors <- function(hex1, hex2) {
  avg_rgb <- round((col2rgb(hex1) + col2rgb(hex2)) / 2)
  rgb(avg_rgb[1, ], avg_rgb[2, ], avg_rgb[3, ], maxColorValue = 255)
}

switching_points <- function() {
  color_map <- expand_grid(pid_7_pre = 1:7, value = 1:7) |>
    mutate(avg_color = map2_chr(palette7[pid_7_pre], palette7[value], average_colors))
  bind_rows(`Study 1` = read_rds(here::here("original", "clean_hypotheticals_study_1_long.rds")),
            `Study 2` = read_rds(here::here("original", "clean_hypotheticals_study_2_long.rds")),
            .id = "study") |>
    left_join(color_map, by = join_by(pid_7_pre, value)) |>
    mutate(treatment = factor(treatment, levels = treatment_levels, labels = treatment_labels))
}

# The two boxes on each panel: the share of Democrats (1-3) imagining themselves Republicans (5-7), and
# the reverse. A panel carries only the out-party box its treatment could move, plus both under the status
# quo: Democrats under pro-Democratic scenarios and Republicans under pro-Republican ones get none.
switching_boxes <- function(points, ..., label_x = c(2, 6)) {
  offset <- 4 / 3
  points |>
    group_by(treatment, study, ...) |>
    summarise(d_to_r = 100 * mean(value[pid_7_pre %in% 1:3] %in% 5:7),
              r_to_d = 100 * mean(value[pid_7_pre %in% 5:7] %in% 1:3), .groups = "drop") |>
    pivot_longer(c(d_to_r, r_to_d), names_to = "direction", values_to = "estimate") |>
    filter(!(direction == "d_to_r" & treatment == "Pro-Democratic hypotheticals"),
           !(direction == "r_to_d" & treatment == "Pro-Republican hypotheticals")) |>
    mutate(center_x = if_else(direction == "d_to_r", 2, 6),
           center_y = if_else(direction == "d_to_r", 6, 2),
           xlabel = if_else(direction == "d_to_r", label_x[1], label_x[2]),
           ylabel = if_else(direction == "d_to_r", 7.75, 0.25),
           label = str_c(sprintf("%.1f", estimate), if_else(direction == "d_to_r", "% of Ds", "% of Rs")),
           xmin = center_x - offset, xmax = center_x + offset,
           ymin = center_y - offset, ymax = center_y + offset)
}

# Figure timestamps ----
# R's pdf() device stamps /CreationDate and /ModDate with the wall clock. The epoch string is the same
# width as what it replaces, which keeps the cross-reference byte offsets valid.
blank_pdf_timestamps <- function(path) {
  epoch <- charToRaw("D:19700101000000")
  raw_pdf <- readBin(path, "raw", file.size(path))
  hits <- grepRaw("D:[0-9]{14}", raw_pdf, all = TRUE)
  for (h in hits) raw_pdf[h:(h + length(epoch) - 1L)] <- epoch
  writeBin(raw_pdf, path)
  invisible(path)
}

save_figure <- function(g, name, width, height) {
  pdf_path <- here::here("maintained", "output", str_c(name, ".pdf"))
  ggsave(pdf_path, plot = g, width = width, height = height)
  blank_pdf_timestamps(pdf_path)
  ggsave(here::here("maintained", "output", str_c(name, ".png")), plot = g, width = width, height = height, dpi = 300)
}
