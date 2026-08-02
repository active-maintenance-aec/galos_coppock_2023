# galos_coppock_2023/maintained/text_summary_stats.R
# Output: output/text_summary_stats.csv
# Depends on: clean_gender_gradients.R output, helpers.R
# Description: The counts quoted in the main text and in section S5: how many studies, how many
#   with a positive ATE, how many occupation CATEs, and how many study-level gender gradients
#   reach significance.

source(here::here("maintained", "helpers.R"))

ates <- read_rds(file.path(data_dir, "estimated_ates.rds")) |> ungroup()
occupation_cates <- read_rds(file.path(data_dir, "occupation_cates.rds")) |> ungroup()
survey_studies <- read_rds(file.path(data_dir, "survey_studies.rds")) |> ungroup()
survey_cates <- read_rds(file.path(data_dir, "survey_occupation_cates.rds")) |> ungroup()
gender_gradients <- read_csv(file.path(out_dir, "gender_gradients.csv"), show_col_types = FALSE)

occupations_per_study <- occupation_cates |>
  count(study_id)

results <- tibble(
  quantity = c(
    "Field studies",
    "Field studies with a positive ATE",
    "Occupation CATEs",
    "Mean occupations per study",
    "Median occupations per study",
    "Survey experiments",
    "Survey occupation CATEs",
    "Study-level gender gradients",
    "Gender gradients significant at p <= 0.05",
    "Gender gradients positive and significant at p <= 0.05"
  ),
  value = c(
    nrow(ates),
    sum(ates$estimate >= 0),
    nrow(occupation_cates),
    mean(occupations_per_study$n),
    median(occupations_per_study$n),
    nrow(survey_studies),
    nrow(survey_cates),
    nrow(gender_gradients),
    sum(gender_gradients$p.value <= 0.05),
    sum(gender_gradients$p.value <= 0.05 & gender_gradients$estimate > 0)
  )
)

write_csv(results, file.path(out_dir, "text_summary_stats.csv"))
print(results)
