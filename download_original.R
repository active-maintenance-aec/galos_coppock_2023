# galos_coppock_2023/download_original.R
# Output: original/ (the deposited replication archive, not redistributed in this repo)
# Depends on: original_manifest.csv
# Description: Fetch the deposited archive from Harvard Dataverse and verify every file.
#   Run this once before running anything in maintained/. Re-running is free: files already
#   present with the right checksum are not downloaded again.
#
#   The manifest carries two checksums per file. md5_served is the MD5 of the bytes Dataverse
#   returns for `?format=original`, which is what this code was written against. md5_published
#   is the checksum Dataverse displays. Here all 19 agree, but they do not always: a published
#   checksum can verify neither the deposited bytes nor the tabular file Dataverse derives from
#   them. Verification therefore runs against md5_served, and any disagreement with the
#   published value is reported rather than allowed to fail the check for the wrong reason.
#
#   None of the 19 files was ingested into a tabular representation, so served_as repeats the
#   deposited file name in every row. The deposit is organised into folders, which Dataverse
#   records as a directory label per file rather than in the file name; the manifest's file
#   column carries the full path so the archive's own relative paths ("data/...",
#   "code/helpers.R") resolve after download.

library(tidyverse)
library(here)

here::i_am("download_original.R")

dataset_doi <- "doi:10.7910/DVN/5MCRNY"
base_url <- "https://dataverse.harvard.edu/api/access/datafile"

# Manifest ----
manifest <- read_csv(here::here("original_manifest.csv"), show_col_types = FALSE)

# Download what is missing or wrong ----
# format=original asks for the deposited bytes rather than any representation Dataverse
# derives.
planned <- manifest |>
  mutate(
    path = here::here("original", file),
    url = str_glue("{base_url}/{dataverse_file_id}?format=original"),
    md5_local = unname(tools::md5sum(path)),
    needs_download = is.na(md5_local) | md5_local != md5_served
  )

walk(unique(dirname(planned$path)), dir.create, recursive = TRUE, showWarnings = FALSE)

walk2(
  planned$url[planned$needs_download],
  planned$path[planned$needs_download],
  function(url, path) download.file(url, destfile = path, mode = "wb", quiet = TRUE)
)

print(str_glue("Downloaded {sum(planned$needs_download)} of {nrow(planned)} files; ",
               "{sum(!planned$needs_download)} already present and verified."))

# Verify ----
# md5_served is the MD5 of the bytes `?format=original` returns, and it is the gate.
# md5_published is what Dataverse displays for those same bytes: where the two agree it
# adds nothing a check could fail on, and where they disagree the archive's own metadata
# is wrong and no local copy could satisfy both, so it is reported rather than enforced.
verified <- planned |>
  mutate(
    md5_downloaded = unname(tools::md5sum(path)),
    bytes_on_disk = file.size(path),
    md5_ok = md5_downloaded == md5_served,
    bytes_ok = bytes_on_disk == bytes,
    published_agrees = md5_served == md5_published
  ) |>
  select(file, bytes, bytes_on_disk, bytes_ok, md5_served, md5_downloaded, md5_ok,
         published_agrees)

print(verified |> select(file, bytes, bytes_ok, md5_ok, published_agrees), n = nrow(verified))

if (!all(verified$md5_ok)) {
  stop("Checksum mismatch in original/: ",
       paste(verified$file[!verified$md5_ok], collapse = ", "),
       ". Delete the offending files and re-run to refetch them from Dataverse.")
}

if (!all(verified$bytes_ok)) {
  stop("Byte size mismatch in original/: ",
       paste(verified$file[!verified$bytes_ok], collapse = ", "), ".")
}

# original/ must hold the deposit and nothing else ----
# A file the manifest does not list is either a stray left behind by running the archive or a
# renamed deposited file, and both make this working copy differ from what a fresh clone gets.
# Checking names alone would pass a rename, which is why the checksum check above comes first.
# all.files = TRUE is not optional: without it a stray dotfile passes unseen, and a
# deposit can ship dotfiles of its own, which the manifest then has to list.
strays <- setdiff(
  list.files(here::here("original"), recursive = TRUE, all.files = TRUE, no.. = TRUE),
  manifest$file
)

if (length(strays) > 0) {
  stop("original/ holds files the manifest does not list: ",
       paste(strays, collapse = ", "),
       ". Move them elsewhere; original/ is the deposit and only the deposit.")
}

print(str_glue("All {nrow(verified)} files match on MD5 and byte size, and original/ holds ",
               "nothing else. {sum(!verified$published_agrees)} carry a published checksum ",
               "that disagrees with what Dataverse serves."))
print(str_glue("Archive: {dataset_doi}"))
