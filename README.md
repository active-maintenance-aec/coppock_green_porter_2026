# Active Maintenance Report: coppock_green_porter_2026


- [Summary](#summary)
- [What was run](#what-was-run)
- [Checks and what they do not
  cover](#checks-and-what-they-do-not-cover)
- [Not done](#not-done)

## Summary

`errata.qmd` lists 14 entries. Entries 1 and 2 are notes on coauthored
claims that await the authors. The other 12 are corrections to the text,
labels, one table, and the references. No public deposit was located, so
the rerun rests on the authors’ final replication archive, the
2025-12-29 zip in the local project folder. **A stranger cannot rerun
this repository**, because `download_original.R` and
`maintained/raw_to_clean.R` read that local folder. Raw exports hold IP
addresses and coordinates and are never copied here. This repository has
no independent rewrite, which falls short of Stage 3: every computed
value comes from the deposit’s own code and estimators, apart from the
repaired one-sided test.

## What was run

`maintained/run_archive.R` runs 20 scripts twice in scratch copies.
Causes of the as-shipped failures, each shown by its log:

- `table_C5` writes to `../replication_archive/`. The first harness’s
  scratch name caused that failure, so it was not an archive defect, and
  the scratch copy now has that name.
- `figure_5`, `figure_7`, and `figure_C8` call `format_num` or
  `make_se_entry`, exported by `rdss`, without attaching `rdss`. They
  fail as shipped. This is an archive defect. The stripped pass attaches
  it, and `ggstance` too because `figure_C8` calls `position_dodgev`.
- `figure_A4` logs an error and exits 0. The script’s comment says such
  errors are caught and handled, and the error is inside its `try()`
  around bootstrap draws.

The five shipped estimate files reproduce on the stripped pass, with a
largest numeric difference below 6e-13. `maintained/raw_to_clean.R`
rebuilds the four cleaned hypothetical files from the raw exports with
no differing shared column (`maintained/output/raw_to_clean.csv`). That
test does not reach the survey experiments or the field experiment,
whose cleaned files are inputs here.

## Checks and what they do not cover

- **Prose values.** `ground_truth/build_ground_truth.R` compares 56
  claims with the rerun at the page’s precision. The mismatches are the
  spine’s. These compare the page with the deposit’s own data, so a
  defect both inherit would pass.
- **Negative controls.** `ground_truth/negative_controls.R` tests that
  the gate fires on those faults only.
- **Page checks.** Every quote was read on a rendered crop of its key
  clause, and Figure 2 on the page. The rest of each sentence-long quote
  rests on the text layer.
- **Crossref.** Family names and years were compared for most article
  references. The service rate-limited 13 of 48 and 11 were not parsed,
  mostly books. Only Goren and Chapp was rechecked singly, so the
  reference list is not fully checked.

## Not done

- No figure other than Figure 2 was read against its rerun, so the
  labels of Figures 1 and 3 to 7 are unchecked, though their prose
  values are in the gate. No table cell outside Table A.2 and the Table
  B.3 label was compared.
- The appendix prose was searched by known phrases and not read whole.
  The Table A.1 change made for the journal’s replicator was not
  rechecked.
- Study 4’s added Table A.2 cells rest on a subtraction, since its
  `video_all` does not name the video.
- The page proofs in `QJPS page proofs/` were not compared, so entry
  13’s hyphenation may be the typesetter’s rather than the authors’.
- Queue rows 13 to 15 are open for Alex: deposit status and the two
  notes.
