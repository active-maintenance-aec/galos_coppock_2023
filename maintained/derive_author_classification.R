# galos_coppock_2023/maintained/derive_author_classification.R
# Output: maintained/author_classification.csv
# Depends on: ~/Dropbox/meta_gender_public (NOT part of this repository)
# Description: Which of the 298 study-occupation cells carry a gender composition category
#   the ORIGINAL study authors assigned, and which carry one assigned by Galos and Coppock.
#
#   The deposited occupation_cates.rds gives every cell one of the three categories and
#   records nothing about where it came from. The published Fig. 4 caption is explicit that
#   the categories are "as given by study authors where available and by us otherwise", so
#   the article is accurate about what it did; what it cannot do is show the reader which
#   points are which. The successor project, meta_gender_public, carries the distinction as
#   an `author_classification` column in metadata/study_occupations.xlsx and drops the
#   filled-in categories rather than plotting them, and this file carries that coding back
#   to the 57 studies this article analyses AND NO FURTHER. meta_gender_public has 79
#   studies and 400 occupation cells; the 22 studies and the occupations collected after
#   this article went to press are not brought back here.
#
#   THIS SCRIPT IS NOT PART OF run_all.R. It reads a working directory outside this
#   repository, which nobody else has, so its output is committed and the reproduction
#   reads the CSV. Re-run it by hand if the coding upstream changes.

source(here::here("maintained", "helpers.R"))
library(readxl)

meta_gender_public <- path.expand("~/Dropbox/meta_gender_public")

occupation_cates <- read_rds(file.path(data_dir, "occupation_cates.rds")) |>
  ungroup() |>
  select(study_id, occupation_observed)

# The two projects do not share study ids: this article's archive says `ahmed_2` where
# meta_gender_public says `ahmed_andersson_hammarstedt_2013_study_1`. studies.xlsx carries
# the old id as study_id_legacy, and the map is one to many, because stasio_1 became three
# studies there.
crosswalk <- read_excel(file.path(meta_gender_public, "metadata", "studies.xlsx"), sheet = 1) |>
  select(study_id_legacy, study_id_new = study_id) |>
  drop_na(study_id_legacy)

# study_occupations.xlsx is the hand coding itself, and it is keyed by the NEW study id.
# It carries the occupation under two names, the raw `occupation` and the cleaned
# `occupation_observed`, and this article's archive uses sometimes one and sometimes the
# other, so both are matched and the results unioned.
coding <- read_excel(file.path(meta_gender_public, "metadata", "study_occupations.xlsx")) |>
  select(study_id_new = study_id, occupation, occupation_observed_new = occupation_observed,
         author_classification)

keyed <- occupation_cates |>
  left_join(crosswalk, by = c("study_id" = "study_id_legacy"), relationship = "many-to-many")

matched <- bind_rows(
  keyed |>
    inner_join(coding, by = c("study_id_new", "occupation_observed" = "occupation_observed_new")),
  keyed |>
    inner_join(coding, by = c("study_id_new", "occupation_observed" = "occupation"))
) |>
  select(study_id, occupation_observed, author_classification) |>
  distinct()

author_classification <- matched |>
  group_by(study_id, occupation_observed) |>
  summarise(author_classification = paste(sort(unique(author_classification)), collapse = "/"),
            .groups = "drop")

# A cell that matched two new studies with two different codings would be a judgment this
# script cannot make, and a cell that matched nothing would be a silent hole in the figure.
stopifnot(
  !any(str_detect(author_classification$author_classification, "/")),
  all(author_classification$author_classification %in% c("yes", "no")),
  nrow(author_classification) == nrow(occupation_cates),
  nrow(anti_join(occupation_cates, author_classification,
                 by = c("study_id", "occupation_observed"))) == 0
)

author_classification |>
  arrange(study_id, occupation_observed) |>
  write_csv(here::here("maintained", "author_classification.csv"))

print(table(author_classification$author_classification))
