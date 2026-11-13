# coppock_green_porter_2026/download_original.R
# Output: original/ and original_extracted/ (both gitignored), original_manifest.csv, original_members_manifest.csv
# Depends on: Alex's local project archive, archived_projects/coppock_green_porter_2026/
# Description: No public deposit of this article could be located (searched 2026-11-06: Harvard Dataverse,
#   and the catalog lists none), so nothing is downloaded. Copy the authors' final replication archive
#   (the 2025-12-29 zip made after the journal's replication memo), gate it on the md5 and byte size in the
#   project archive's manifest of 2026-08-11, unpack it, and gate the members. Halts on a mismatch or a
#   stray file. Requires Alex's machine, which is why the report states what a stranger cannot rerun.

library(tidyverse)
library(tools)
library(here)
here::i_am("download_original.R")

archive <- "~/Library/CloudStorage/Dropbox/archived_projects/coppock_green_porter_2026"
zip_path <- "submissions/qjps_final_take_2/replication_archive.zip"
manifest <- read_csv(file.path(archive, "_ARCHIVE", "coppock_green_porter_2026_hypotheticals_manifest.csv"),
                     show_col_types = FALSE) |>
  filter(path == zip_path) |>
  transmute(file = basename(path), directory_label = NA_character_, bytes, md5)
stopifnot(nrow(manifest) == 1)

local_zip <- here::here("original", manifest$file)
if (!file.exists(local_zip)) {
  unlink(here::here("original"), recursive = TRUE)
  dir.create(here::here("original"))
  file.copy(file.path(archive, "coppock_green_porter_hypotheticals", zip_path), local_zip)
}
stopifnot(list.files(here::here("original")) == manifest$file,
          unname(md5sum(local_zip)) == manifest$md5,
          file.size(local_zip) == manifest$bytes)
write_csv(manifest, here::here("original_manifest.csv"), na = "")

unlink(here::here("original_extracted"), recursive = TRUE)
unzip(local_zip, exdir = here::here("original_extracted"))
root <- here::here("original_extracted", "replication_archive")
members <- tibble(member = list.files(root, recursive = TRUE)) |>
  mutate(bytes = file.size(file.path(root, member)), md5 = unname(md5sum(file.path(root, member))))
members_path <- here::here("original_members_manifest.csv")
if (file.exists(members_path)) {
  recorded <- read_csv(members_path, show_col_types = FALSE)
  stopifnot(setequal(recorded$member, members$member),
            all(arrange(recorded, member)$md5 == arrange(members, member)$md5))
}
write_csv(members, members_path)
message(nrow(members), " members verified.")
