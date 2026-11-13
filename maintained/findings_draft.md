**SUPERSEDED 2026-11-06 by `errata.qmd`.** Kept because `rm` is denied in the unattended run. Candidate 5 was dropped: the field identification index has three items where the third was asked. Candidate 7 became entry 7.

# Draft findings, coppock_green_porter_2026 (2026-11-06)

Working notes, not a spine. Nothing here is filed. Printed pages are the journal's (pp. 26-48); no quote below has been checked on a rendered page image, and each must be before filing.

## Sources

- Published PDF and appendix: catalog `original_materials/`.
- `original/`: the authors' final replication archive, the 2025-12-29 zip, md5 d0e8c4aa, 39 members, cleaned data only. No public deposit was found: Harvard Dataverse searched by title phrase and by author keywords, and the catalog's `replication_archive` is empty. The OSF node nvmza is the Study 2 PAP (registered 2024-02-05), with no data seen at its top level.
- Project folder: `archived_projects/coppock_green_porter_2026/coppock_green_porter_hypotheticals` holds raw exports (`hyp_orig_*.csv`, `hyp_rep_*.csv`) and the cleaning scripts. `raw_data.csv` has IPAddress and LocationLatitude and Longitude columns. Never copy raw files into this repo.
- `submissions/qjps_final_take_2/replication_report/`: the journal replicator's memo of 2025-10-31 and the authors' `responses.txt`. The replicator already moved the 0.56 and 0.46 preview, the -0.36 and -0.67, the 15 and 17 macropartisanship claim, and Table A.1, so those were checked once by someone else.

## Rerun state (`ground_truth/archive_run_status.csv`)

As shipped, exit 1 on four of 20 scripts: `table_C5` (`write_rds`: cannot open the connection), `figure_5` (`format_num` in `summarise`), `figure_7` and `figure_C8` (`make_se_entry`). `figure_A4` exits 0 with an `Error` in its log (`trimming_bounds`). Stripped, `in_text_claims.R` also fails, on a missing rds downstream of those. **None of these has been chased.** Each is a hypothesis about package versions until it is: the archive records rdss 1.0.14 and estimatr 1.0.6, and this machine has rdss 1.0.16 and estimatr 2.0.1.9000. As-shipped `in_text_claims.R` runs to its end: log in `maintained/archive_run/as_shipped/logs/` (gitignored), 332 lines, not yet compared with the page beyond the two correlations.

`maintained/raw_to_clean.R` is named in `run_archive.R`'s header and is NOT WRITTEN. It should run the project folder's two clean scripts in a tempdir and compare with the archive's rds. An earlier run in this session ran those scripts without error and compared nothing.

## Candidates, in rough order of weight

1. **p. 38-39 vs p. 41, the evaluation index.** p. 38 says the index "averages together the following 12 outcomes" and lists 12; p. 41 says "an index of 14 partisan evaluation questions". The cleaning code (`clean_hypotheticals_study_1.R`, about line 528) averages 14: the listed 12 plus `therm_dems` and `therm_reps`. Study 2 appeared the same on a partial diff. So 14 is right and the p. 38 list omits the two party thermometers. Check whether the PAP's index lists 12 or 14 before writing the note.
2. **p. 43, "near 100% power".** Study 2's realized SEs on party ID are 0.024 and 0.023 (`ates_study_2.rds`, rerun). A two-sided 0.05 test, equivalent to the one-sided 0.025, has about 53% and 59% power against 0.05, the size of the Study 1 estimates (-0.04, 0.05). The PAP's simulation assumed its own R squared and the sentence may mean the design-stage figure. Study 1's 0.05 is also quoted at "near 100% ... 0.05" on p. 33-34. **Coauthored claim: file in note form and queue it.** State exactly what was computed, and that design-stage power is a different quantity.
3. **p. 34 vs appendix C.1, ANES year.** The article says subjects saw "the distribution of party ID as measured in the 2016 ANES"; appendix C.1's quoted instrument says "In a 2022 survey" and "in 2022". Settle from `experimental_materials/` or the raw csv's question-text row (my first attempt to read it failed on the date filter, not on the text).
4. **Table B.3, appendix.** Four rows read "Org 2 vs Org 2". Should presumably read "Org 2 vs Org 1"; confirm from `figure_1_figure_B5_table_B3_field_experiment_nofe.R`.
5. **Appendix B.1 vs article p. 32.** The article says the field experiment's scales "are the same as the outcome measures described in the next section" (three-item identification index). B.1 says the identification scale uses party ID and a Republican self-description, two items. Read the field script.
6. **Table A.2.** Dosage cells do not sum to dose times N: Study 4 at dose 6 gives 5 x 1500 = 7500 against 9000; Study 3 at dose 1 gives 698 of 846. The table's script summarises five variables (`presidents`, `share_america`, `facts`, `guns`, `weare`) and the table header has five treatment columns, so a sixth video type seems absent. Figure A.1 has 12 panels and Table A.1 has 11 video rows (no Negative Partisanship). Read `clean_survey_experiments.rds`.
7. **Macropartisanship.** The Discussion's 15 and 17 come from `rma.uni` over the two studies' Overall rows (0.179, 0.142 and -0.082, -0.247, per `macropartisanship_estimates.rds`). They reproduce on this reading but are random-effects averages of two strongly different pro-Republican estimates (-0.08 and -0.25), and the sentence says "on average". Verify the printed values and consider a note only if something is wrong; do not manufacture one. The Gallup swings: reread the `macro_data` window logic, since "within a 10-year period" lists 1965-1968 (3 years) and overlapping windows.
8. **p. 42 and methods: estimator wording.** Confirm that "OLS ... HC2" matches the scripts; the 7.0 (1.5) comes from `lm_lin` with clusters plus a random-effects pool over four contrasts, and the page calls it "cluster-robust SE".
9. **Dates.** p. 27 says "In the spring of 2024" for scenarios; the PAP (2024-02-05) says Study 1 was already complete. Check the raw `StartDate` values.

## Wording and typesetting, to verify on page images

- "Marjorie Taylor Green" (twice, p. 38), for Greene. "National Guuard" in Table 1 (p. 35): check whether it was fielded so. "very precisely estimated zero" (p. 29). "ffox nnews" in the Broockman and Kalla reference, and "partisan-ship" and "sensi-tive" in reference titles (p. 46): publisher artifacts, check the proofs in `submissions/page_proofs/`. "α = 0:025" (p. 39) may be only a text-layer artifact: read the image.
- Appendix: "by save and condition" (wave), "treatment views" (videos), "Outcomes were assess", "results are shows", "patten", "TableC.5", "a placebo-controlled designs", "demographic variable".

## Not looked at yet

Figures 1 and 3 to 7 against their reruns, every printed figure label, the field numbers (0.007, 0.028, 11,199, 11,005, 19,026, 18,424, adjusted R squared 0.76 and 0.48), survey experiment ATEs, 0.009 one-sided p, 13.4, 11.1, 11.8, 13.0, 3% to 8%, 7.0, 19.4%, sample sizes 988 and 3,050, equal-probability assignment, reference list against Crossref, abstract as printed vs catalog abstract.
