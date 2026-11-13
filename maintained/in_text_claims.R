# coppock_green_porter_2026/maintained/in_text_claims.R
# Output: printed CLAIM lines and a CLAIM SUMMARY line; no files
# Depends on: maintained/output/page_values.csv, ground_truth/published_claims.csv, errata_entries.csv, excheckr
# Description: One claim() per row of the extraction, each scoring the value the rerun computes against the
#   number the article prints. The expectation is derived, never typed here: published_claims.csv says what
#   the article prints, and errata_entries.csv says which of those a quantity erratum corrects and to what.
#   A claim an erratum names must disagree with the article and equal the corrected value; every other
#   claim must agree with the article at its printed precision. Every value comes from page_values.csv,
#   which build_ground_truth.R reads too, so this file checks the errata against the rerun rather than
#   reaching each number by a second path.

library(tidyverse)
library(excheckr)
library(here)
here::i_am("maintained/in_text_claims.R")

page <- read_csv(here::here("maintained", "output", "page_values.csv"), show_col_types = FALSE)
value <- function(id) {
  hit <- page$value_rewrite[page$claim_id == id]
  stopifnot(length(hit) == 1)
  hit
}

# Every column is read as character, so a printed 7.0 keeps the decimal its comparison depends on.
published <- read_csv(here::here("ground_truth", "published_claims.csv"), col_types = cols(.default = col_character()))
errata <- read_csv(here::here("errata_entries.csv"), col_types = cols(.default = col_character()))
claim_start(published = published, errata = errata, format = "id")

# Abstract ----
claim("switch_treated", value("switch_treated"), "percent imagining a switch under pro-out-party scenarios")
claim("switch_status_quo", value("switch_status_quo"), "percent imagining a switch under status quo")

# Pages 26, 28, and 40 ----
claim("switch_gap", value("switch_gap"), "difference in percentage points")

# Pages 26 and 40 ----
claim("switch_gap_se", value("switch_gap_se"), "standard error of the difference")

# Page 28 ----
claim("preview_rep", value("preview_rep"), "average pro-Republican effect")
claim("preview_dem", value("preview_dem"), "average pro-Democratic effect")

# Page 29 ----
claim("sd_pid", value("sd_pid"), "standard deviation of the seven-point scale")

# Page 32 ----
claim("ad_1", value("ad_1"), "first single-video estimate")
claim("ad_1_se", value("ad_1_se"), "its robust standard error")
claim("ad_2", value("ad_2"), "second single-video estimate")
claim("ad_2_se", value("ad_2_se"), "its robust standard error")
claim("ads_tested", value("ads_tested"), "ads tested")
claim("field_ate", value("field_ate"), "field estimate on party ID")
claim("field_se", value("field_se"), "its cluster-robust standard error")

# Page 33 ----
claim("n_study_1", value("n_study_1"), "Study 1 respondents")
claim("n_study_2", value("n_study_2"), "Study 2 subjects")

# Page 37, Figure 2(a) ----
claim("fig_2a_enrolled", value("fig_2a_enrolled"), "enrolled in Study 1 (334 + 334 + 327)")

# Page 34 ----
claim("cor_1", value("cor_1"), "Study 1 correlation")
claim("cor_2", value("cor_2"), "Study 2 correlation")

# Page 38 ----
claim("index_items", value("index_items"), "items in the partisan evaluation index")

# Page 39 ----
claim("s1_d_guns", value("s1_d_guns"), "Study 1 smallest pro-Democratic")
claim("s1_d_health", value("s1_d_health"), "Study 1 largest pro-Democratic")
claim("s1_r_health", value("s1_r_health"), "Study 1 smallest pro-Republican")
claim("s1_r_economy", value("s1_r_economy"), "Study 1 largest pro-Republican")
claim("s2_d_guns", value("s2_d_guns"), "Study 2 smallest pro-Democratic")
claim("s2_d_health", value("s2_d_health"), "Study 2 largest pro-Democratic")
claim("s2_r_guns", value("s2_r_guns"), "Study 2 smallest pro-Republican")
claim("s2_r_camps", value("s2_r_camps"), "Study 2 Camps")

# Page 40 ----
claim("box_r_1", value("box_r_1"), "Study 1 Republicans who would be Democrats")
claim("box_r_2", value("box_r_2"), "Study 2 Republicans who would be Democrats")
claim("box_d_1", value("box_d_1"), "Study 1 Democrats who would be Republicans")
claim("box_d_2", value("box_d_2"), "Study 2 Democrats who would be Republicans")
claim("status_quo_low", value("status_quo_low"), "lowest status quo switching rate")
claim("status_quo_high", value("status_quo_high"), "highest status quo switching rate")

# Page 42 ----
claim("post_d", value("post_d"), "Study 1 pro-Democratic on party ID")
claim("post_d_se", value("post_d_se"), "its standard error")
claim("post_r", value("post_r"), "Study 1 pro-Republican on party ID")
claim("post_r_se", value("post_r_se"), "its standard error")
claim("s1_index_largest", value("s1_index_largest"), "largest Study 1 index estimate in percentage points")

# Page 43 ----
claim("subgroup_significant", value("subgroup_significant"), "significant subgroup tests")
claim("subgroup_p", value("subgroup_p"), "one-sided p")
claim("status_quo_above_half", value("status_quo_above_half"), "status quo cells exceeding 50 percent of 42")

# Page 44 ----
claim("mp_dem", value("mp_dem"), "pro-Democratic macropartisanship")
claim("mp_rep", value("mp_rep"), "pro-Republican macropartisanship")
claim("high_year", value("high_year"), "year of the high")
claim("mp_high", value("mp_high"), "high")
claim("low_year", value("low_year"), "year of the low")
claim("mp_low", value("mp_low"), "low")
claim("mp_range", value("mp_range"), "difference")
claim("swing_68_77", value("swing_68_77"), "1968 to 1977")
claim("swing_56_65", value("swing_56_65"), "1956 to 1965")
claim("swing_03_09", value("swing_03_09"), "2003 to 2009")
claim("swing_77_85", value("swing_77_85"), "1977 to 1985")
claim("swing_65_68", value("swing_65_68"), "1965 to 1968")
claim("swing_93_02_printed", value("swing_93_02_printed"), "1993 to 2002 as an endpoint difference")
claim("swing_third_republican", value("swing_third_republican"), "third listed pro-Republican swing")

# Gates ----
# assert_claims() fails on any MISMATCH, STALE, DRIFT, or MISSING, on a quantity erratum naming a claim
# this file does not print, and on any difference between the claims printed and those the extraction
# marks needs_block. A DRIFT means the rerun has moved off the value the errata print, or errata.qmd has
# not been re-rendered since it last moved; re-render before investigating.
assert_claims()
invisible(claim_summary())
