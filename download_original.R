# coppock_green_porter_2026/download_original.R
# Output: original/ (the deposited replication archive, gitignored, not redistributed in this repo)
# Depends on: original_manifest.csv
# Description: Fetch the deposited archive from Harvard Dataverse, doi:10.7910/DVN/LBPTEK (released
#   2026-11-12), and verify every file. Run this once before anything in maintained/. Re-running is free:
#   files already present with the right checksum are not downloaded again.
#
#   The manifest carries two checksums per file. md5_served is the MD5 of the bytes Dataverse returns for
#   `?format=original`, which is what this code was written against; md5_published is the checksum
#   Dataverse displays. All 39 agree. None of the files was ingested as tabular data, so the bytes served
#   are the deposited bytes.
#
#   The deposit is the authors' final replication archive of 2025-12-29 with six files changed: two cleaned
#   data files lose identifying columns no script reads, three scripts gain the library() calls they
#   lacked, and README.txt records those changes. The README names the six.

library(tidyverse)
library(here)

here::i_am("download_original.R")

dataset_doi <- "doi:10.7910/DVN/LBPTEK"
base_url <- "https://dataverse.harvard.edu/api/access/datafile"

# Manifest ----
manifest <- read_csv(here::here("original_manifest.csv"), show_col_types = FALSE)

dir.create(here::here("original"), showWarnings = FALSE)

# Download what is missing or wrong ----
planned <- manifest |>
  mutate(
    path = here::here("original", file),
    url = str_glue("{base_url}/{dataverse_file_id}?format=original"),
    md5_local = unname(tools::md5sum(path)),
    needs_download = is.na(md5_local) | md5_local != md5_served
  )

walk2(
  planned$url[planned$needs_download],
  planned$path[planned$needs_download],
  function(url, path) download.file(url, destfile = path, mode = "wb", quiet = TRUE)
)

print(str_glue("Downloaded {sum(planned$needs_download)} of {nrow(planned)} files; ",
               "{sum(!planned$needs_download)} already present and verified."))

# Verify ----
verified <- planned |>
  mutate(
    md5_downloaded = unname(tools::md5sum(path)),
    bytes_local = file.size(path),
    match = md5_downloaded == md5_served,
    published_agrees = md5_served == md5_published,
    size_agrees = bytes_local == bytes
  ) |>
  select(file, bytes, bytes_local, md5_served, md5_downloaded, match, published_agrees, size_agrees)

print(verified, n = nrow(verified))

if (!all(verified$match)) {
  stop("Checksum mismatch: the downloaded archive does not match what Dataverse served when this code was written.")
}

if (!all(verified$size_agrees)) {
  stop("Byte size mismatch against original_manifest.csv.")
}

# original/ must hold the deposit and nothing else ----
# A stray file is usually output a run dropped beside its inputs, and one can overwrite a deposited file,
# so this is a hard stop. all.files = TRUE catches the .Rhistory and .RData R drops beside what it runs.
present <- list.files(here::here("original"), recursive = TRUE, all.files = TRUE, no.. = TRUE)
unlisted <- setdiff(present, manifest$file)

if (length(unlisted) > 0) {
  stop(str_glue("original/ holds {length(unlisted)} file(s) the manifest does not ",
                "list: {str_flatten_comma(unlisted)}"))
}

print(str_glue("All {nrow(verified)} files match on MD5 and byte size, and original/ holds nothing else. ",
               "{sum(!verified$published_agrees)} carry a published checksum that disagrees."))
print(str_glue("Archive: {dataset_doi}"))
