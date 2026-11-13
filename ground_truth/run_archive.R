# galos_coppock_2023/ground_truth/run_archive.R
# Output: ground_truth/archive_run_status.csv, ground_truth/archive_overwrites.csv
# Depends on: original/ (run download_original.R first)
# Description: Run every deposited script in a fresh R process and record how each one ends,
#   twice: once on the deposit as shipped, and once with the shipped intermediate withheld.
#
#   archive_values.R answers which numbers the deposited code produces. This script answers the
#   prior question of which deposited scripts run at all, and what they write. Each script runs
#   as its own Rscript process, so its exit status is the one a reader would see, and with the
#   graphics device sent to a null target, so a top-level print of a ggplot writes no stray
#   Rplots.pdf into the copy.
#
#   The deposit ships only derived .rds files and no raw data, so the stripped pass withholds
#   the one shipped file the deposit's own code writes: data/gender_gradients_df.rds, the last
#   line of appendix_figures_and_loop.R. Every other .rds file is an input that no deposited
#   script produces, and withholding it would test nothing. helpers.R is only ever sourced by the
#   other scripts and gets no row.
#
#   Both passes run in copies under tempdir() and never in original/, which must keep the
#   deposited bytes exactly. The scripts run in the order the deposit's README lists them, which
#   puts appendix_figures_and_loop.R before appendix_gender_gradient.R, the script that reads
#   what it writes.
#
#   This script is not part of run_all.R, for the same reason archive_values.R is not.

library(here)
library(tidyverse)
library(withr)

here::i_am("ground_truth/run_archive.R")

scripts <- c("analyze_ates.R", "analyze_cates_ethnicity.R", "analyze_cates_regression_tables.R",
             "analyze_cates.R", "appendix_cates_survey_field.R", "appendix_evidence_map.R",
             "appendix_figures_and_loop.R", "appendix_gender_gradient.R",
             "appendix_survey_ates.R")

# Every deposited script draws a table, a figure or a number the paper prints. The loop script
# also writes the intermediate the gradient script reads, but what it exists to draw is the
# study panels of section S4, so it is analysis rather than cleaning.
script_roles <- tibble(script = scripts, role = "analysis")

intermediates <- "data/gender_gradients_df.rds"

timeout_seconds <- 600

# Running one pass ----
# file_hashes: the MD5 of every file under a directory, keyed by its path relative to it.
file_hashes <- function(dir) {
  files <- list.files(dir, recursive = TRUE, all.files = TRUE)
  tibble(file = files, md5 = unname(tools::md5sum(file.path(dir, files))))
}

run_pass <- function(pass, withheld) {
  copy <- file.path(tempdir(), str_glue("archive_{pass}"))
  unlink(copy, recursive = TRUE)
  dir.create(copy, recursive = TRUE)
  file.copy(here::here("original", "replication_archive"), copy, recursive = TRUE)
  archive_dir <- file.path(copy, "replication_archive")
  file.remove(file.path(archive_dir, withheld))

  before <- file_hashes(archive_dir)

  status <- map(scripts, function(script) {
    log <- tempfile(fileext = ".log")
    exit_status <- with_dir(archive_dir, system2(
      file.path(R.home("bin"), "Rscript"),
      c("-e", shQuote(str_glue("pdf(NULL); source('code/{script}')"))),
      stdout = log, stderr = log, timeout = timeout_seconds
    ))
    output <- read_lines(log)
    error_line <- str_which(output, "^Error")
    tibble(
      pass = pass,
      script = script,
      exit_status = exit_status,
      stopped_at = if (exit_status == 0 || length(error_line) == 0) {
        NA_character_
      } else {
        # The message only: the backtrace and the closing "Execution halted" that follow it
        # say where R was, not what went wrong.
        output[error_line[1]:length(output)] |>
          str_c(collapse = " ") |>
          str_remove("(Backtrace:|Calls:|Execution halted).*$") |>
          str_squish()
      }
    )
  }) |>
    list_rbind()

  after <- file_hashes(archive_dir)
  deposit <- file_hashes(here::here("original", "replication_archive"))

  # What the pass did to the deposit's files: a withheld intermediate either comes back
  # (rebuilt) or stays missing (withheld), a shipped file whose bytes change is overwritten,
  # one that disappears is deleted, and a file the deposit never shipped is added.
  touched <- full_join(rename(before, md5_before = md5), rename(after, md5_after = md5),
                       by = "file") |>
    full_join(tibble(file = withheld), by = "file") |>
    left_join(rename(deposit, md5_deposit = md5), by = "file") |>
    mutate(
      effect = case_when(
        file %in% withheld & !is.na(md5_after) ~ "rebuilt",
        file %in% withheld ~ "withheld",
        is.na(md5_after) ~ "deleted",
        is.na(md5_before) ~ "added",
        md5_before != md5_after ~ "overwritten",
        .default = NA_character_
      )
    ) |>
    filter(!is.na(effect)) |>
    transmute(
      pass = pass,
      file,
      effect,
      matches_deposit = if_else(effect %in% c("added", "withheld"), NA,
                                md5_after == md5_deposit)
    )

  list(status = status, touched = touched)
}

as_shipped <- run_pass("as_shipped", withheld = character())
stripped <- run_pass("stripped", withheld = intermediates)

# The two archive tables ----
run_status <- bind_rows(as_shipped$status, stripped$status) |>
  left_join(script_roles, by = "script") |>
  transmute(pass, script, role, exit_status,
            outcome = case_when(exit_status == 0 ~ "clean",
                                exit_status == 124 ~ "timeout",
                                .default = "error"),
            stopped_at)

overwrites <- bind_rows(as_shipped$touched, stripped$touched)

stopifnot(nrow(run_status) == 2 * length(scripts), !anyNA(run_status$role))

write_csv(run_status, here::here("ground_truth", "archive_run_status.csv"))
write_csv(overwrites, here::here("ground_truth", "archive_overwrites.csv"))

print(run_status |> select(pass, script, outcome), n = nrow(run_status))
print(overwrites)
