# galos_coppock_2023/maintained/table_s2_1_study_manifest.R
# Output: output/table_s2_1_study_manifest.tex, .csv
# Depends on: helpers.R
# Description: The study manifest of table S2.1: one row per field study, with its subject
#   count, its number of occupations, and whether it enters the majority / minority analysis.
#
#   The deposit ships no code for this table, so each column is rebuilt from the data it does
#   ship. The number of occupations is a count of the study's rows in occupation_cates.rds.
#   Inclusion in the majority / minority analysis is presence in occupation_cates_ethnicity.rds.
#   The subject count is not stored anywhere, but each occupation's CATE carries the residual
#   degrees of freedom of the regression that produced it, and summing df + 1 over a study's
#   occupations recovers the published subject count for most studies. Where it does not, the
#   published count is a multiple of it, which is what a design with several applications per
#   job opening gives. The reconstruction is written as it comes out; the ground truth records
#   how far it gets rather than the code adjusting itself to the published number.

source(here::here("maintained", "helpers.R"))

occupation_cates <- read_rds(file.path(data_dir, "occupation_cates.rds")) |> ungroup()
occupation_cates_ethnicity <- read_rds(file.path(data_dir, "occupation_cates_ethnicity.rds")) |> ungroup()
study_citations <- read_rds(file.path(data_dir, "estimated_ates.rds")) |>
  ungroup() |>
  select(study_id, study_name_citation)

study_manifest <- occupation_cates |>
  group_by(study_id) |>
  summarize(n_occupations = n(), n_subjects_from_df = sum(df + 1)) |>
  mutate(included_majority_minority =
           if_else(study_id %in% occupation_cates_ethnicity$study_id, "Yes", "No")) |>
  left_join(study_citations, by = "study_id") |>
  arrange(included_majority_minority, n_subjects_from_df) |>
  select(study_id, study_name_citation, n_subjects_from_df, n_occupations,
         included_majority_minority)

study_manifest |>
  select(-study_id) |>
  kable(
    format = "latex",
    booktabs = TRUE,
    col.names = c("Study", "N subjects reconstructed", "N occupations",
                  "Included in majority/minority"),
    caption = paste("Gender discrimination audit field experiment study manifest. The subject",
                    "count is the sum of residual degrees of freedom plus one over the study's",
                    "occupations, which is the deposit's only record of sample size.")
  ) |>
  write_lines(file.path(out_dir, "table_s2_1_study_manifest.tex"))

write_csv(study_manifest, file.path(out_dir, "table_s2_1_study_manifest.csv"))
