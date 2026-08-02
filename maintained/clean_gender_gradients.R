# galos_coppock_2023/maintained/clean_gender_gradients.R
# Output: output/gender_gradients.csv
# Depends on: helpers.R
# Description: Re-derive the study-level gender gradients from the occupation CATEs, one
#   meta-regression per study with at least three occupations.
#
#   The archive ships these as data/gender_gradients_df.rds, written by
#   appendix_figures_and_loop.R, which fails to run. Rather than depend on a deposited
#   intermediate that the deposited code cannot regenerate, the rewrite recomputes it
#   from the occupation CATEs. The two agree to 4e-13 on both estimate and standard error.

source(here::here("maintained", "helpers.R"))

occupation_cates <- read_rds(file.path(data_dir, "occupation_cates.rds")) |> ungroup()

# The published forest plot labels each gradient with the study's citation rather than its
# id, and occupation_cates carries only the id, so the citation is taken from the ATE file.
study_citations <- read_rds(file.path(data_dir, "estimated_ates.rds")) |>
  ungroup() |>
  select(study_id, study_name_citation)

# occupation_cates carries a column called study as well as study_id, so the filter has to
# say explicitly that id is the argument and not a column.
fit_gradient <- function(id) {
  local_dat <- filter(occupation_cates, study_id == .env$id)
  if (nrow(local_dat) <= 2) {
    return(NULL)
  }
  rma.uni(yi = estimate, sei = std.error, mods = ~ fraction_female, data = local_dat) |>
    tidy(conf.int = TRUE) |>
    filter(term == "fraction_female") |>
    mutate(study_id = id, n_occupations = nrow(local_dat))
}

gender_gradients <- unique(occupation_cates$study_id) |>
  map(fit_gradient) |>
  list_rbind() |>
  left_join(study_citations, by = "study_id") |>
  select(study_id, study_name_citation, n_occupations, estimate, std.error, statistic,
         p.value, conf.low, conf.high)

write_csv(gender_gradients, file.path(out_dir, "gender_gradients.csv"))
