# galos_coppock_2023/ground_truth/archive_values.R
# Output: ground_truth/archive_values.csv
# Depends on: original/ (run download_original.R first)
# Description: Run the deposited archive's own code and record every number it produces, so
#   that the ground truth's value_script column is read out of the archive rather than typed.
#
#   "Does the deposited archive reproduce the paper?" and "does the maintained rewrite
#   reproduce the paper?" are different questions and are answered by different code. This
#   script answers the first one. It sources the deposited scripts unmodified, harvests the
#   objects they assign and the table output they print, and writes the result as a long file
#   keyed the same way as the ground truth.
#
#   The deposited scripts read from "data/" and source "code/helpers.R" by relative path, and
#   the last line of appendix_figures_and_loop.R writes back into "data/". They are therefore
#   run in a copy under tempdir() and never in original/, which must keep the deposited bytes
#   exactly. They also print ggplot objects at the top level, which opens a graphics device, so
#   the device is sent to a null target for the duration.
#
#   One caveat travels with these values. The deposit is nothing but derived .rds files, one of
#   which, gender_gradients_df.rds, is the output of the script that fails. appendix_gender_
#   gradient.R therefore passes here by reading a deposited intermediate its own archive cannot
#   regenerate, and its harvested values are a reading of that file rather than of a run.

library(here)
library(tidyverse)
library(withr)

here::i_am("ground_truth/archive_values.R")

scratch <- file.path(tempdir(), "archive_run")
unlink(scratch, recursive = TRUE)
dir.create(scratch, recursive = TRUE)
file.copy(here::here("original", "replication_archive"), scratch, recursive = TRUE)
scratch_archive <- file.path(scratch, "replication_archive")

run_archive_script <- function(script) {
  env <- new.env(parent = globalenv())
  result <- new.env()
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  printed <- with_dir(scratch_archive, {
    capture.output({
      # print.eval is what makes texreg's table reach the capture; the deposited script
      # calls it at the top level and never assigns the result.
      result$attempt <- try(source(file.path("code", script), local = env, print.eval = TRUE),
                            silent = TRUE)
    })
  })
  list(env = env, printed = printed, failed = inherits(result$attempt, "try-error"))
}

ates_run <- run_archive_script("analyze_ates.R")
cates_run <- run_archive_script("analyze_cates.R")
tables_run <- run_archive_script("analyze_cates_regression_tables.R")
survey_field_run <- run_archive_script("appendix_cates_survey_field.R")
survey_ates_run <- run_archive_script("appendix_survey_ates.R")
gradient_run <- run_archive_script("appendix_gender_gradient.R")
loop_run <- run_archive_script("appendix_figures_and_loop.R")

# Forest plot labels ----
# The three forest plots print an "estimate (SE)" label beside every point, and each script
# builds that label into an entry column before plotting it. Reading the estimate and standard
# error columns of the same frame gives what the published figure shows.
forest_rows <- function(dat, figure, stem) {
  dat |>
    ungroup() |>
    transmute(
      table_figure = figure,
      label = str_squish(study_name_citation),
      estimate,
      std.error
    ) |>
    pivot_longer(c(estimate, std.error), names_to = "part", values_to = "value_script") |>
    transmute(
      table_figure,
      claim = if_else(part == "estimate",
                      str_glue("{stem}: {label}"),
                      str_glue("{stem} SE: {label}")),
      value_script
    )
}

fig_s1_1 <- forest_rows(ates_run$env$gg_field, "fig_S1.1", "ATE")
fig_s3_4 <- forest_rows(survey_ates_run$env$gg_survey, "fig_S3.4", "ATE")

# The gender gradient plot labels its points with study ids rather than citations, so the
# citation is taken from the ATE file the same way the published figure does.
gradient_citations <- ates_run$env$ates |>
  ungroup() |>
  select(study_id, study_name_citation)

fig_s5_62 <- gradient_run$env$gg_gender_gradients |>
  ungroup() |>
  select(-any_of("study_name_citation")) |>
  left_join(gradient_citations, by = "study_id") |>
  mutate(study_name_citation = replace_na(study_name_citation, "Meta-analysis")) |>
  forest_rows("fig_S5.62", "Gender gradient")

# Regression tables ----
# analyze_cates_regression_tables.R prints both published tables through texreg and assigns
# nothing that survives, since fit_1 to fit_3 are overwritten by the ethnicity models. The
# printed LaTeX is the archive's table, significance markers included, so it is parsed here.
parse_texreg <- function(printed, table_label, models) {
  start <- str_which(printed, fixed("\\begin{tabular}"))
  stop <- str_which(printed, fixed("\\end{tabular}"))
  which_table <- if_else(table_label == "table_1", 1L, 2L)
  body <- printed[start[which_table]:stop[which_table]]

  cell <- function(row_label, column) {
    row <- body[str_detect(body, str_glue("^{row_label} +&"))]
    if (length(row) == 0) return(NA_character_)
    parts <- str_split(row[1], fixed("&"))[[1]]
    str_squish(parts[column + 1])
  }

  numeric_cell <- function(row_label, column) {
    txt <- cell(row_label, column)
    if (is.na(txt) || txt == "") return(NA_real_)
    as.numeric(str_extract(str_replace_all(txt, "[$()]", ""), "-?[0-9.]+"))
  }

  stars_cell <- function(row_label, column) {
    txt <- cell(row_label, column)
    if (is.na(txt) || txt == "") return(NA_real_)
    str_count(str_extract(txt, "\\^\\{\\*+\\}") |> replace_na(""), fixed("*"))
  }

  # The coefficient sits on the labelled row and its standard error on the unlabelled row
  # immediately below, which is why the SE is read from a row offset rather than a name.
  se_below <- function(row_label, column) {
    idx <- str_which(body, str_glue("^{row_label} +&"))
    if (length(idx) == 0) return(NA_real_)
    parts <- str_split(body[idx[1] + 1], fixed("&"))[[1]]
    txt <- str_squish(parts[column + 1])
    if (is.na(txt) || txt == "") return(NA_real_)
    as.numeric(str_extract(str_replace_all(txt, "[$()]", ""), "-?[0-9.]+"))
  }

  imap(models, function(term_rows, model_index) {
    model_number <- as.integer(model_index)
    map(term_rows, function(term) {
      tibble(
        table_figure = str_glue("{table_label}_model{model_number}"),
        claim = c(term$claim,
                  str_glue("{term$claim} SE"),
                  str_glue("{term$claim} significance stars (0 = none, 1 = one, 2 = two, 3 = three)")),
        value_script = c(numeric_cell(term$row, model_number),
                         se_below(term$row, model_number),
                         stars_cell(term$row, model_number))
      )
    }) |>
      list_rbind() |>
      bind_rows(tibble(
        table_figure = str_glue("{table_label}_model{model_number}"),
        claim = c("Tau squared", "Nobs"),
        value_script = c(numeric_cell("tau.squared", model_number),
                         numeric_cell("nobs", model_number))
      ))
  }) |>
    list_rbind()
}

table_1_terms <- list(
  list(row = "Intercept", claim = "Intercept"),
  list(row = "Gender gradient", claim = "Gender gradient")
)

table_2_terms <- c(
  table_1_terms,
  list(
    list(row = "Majority status", claim = "Majority status"),
    list(row = "Majority x gradient", claim = "Majority x gradient")
  )
)

table_1 <- parse_texreg(tables_run$printed, "table_1", set_names(rep(list(table_1_terms), 5), 1:5))
table_2 <- parse_texreg(tables_run$printed, "table_2", set_names(rep(list(table_2_terms), 3), 1:3))

# In-text quantities ----
occupations_per_study <- cates_run$env$occupation_cates |>
  ungroup() |>
  count(study_id)

archive_predictions <- predict(cates_run$env$meta_fit_1,
                               newmods = c(fraction_female = c(0, 1)))

survey_gradient <- broom::tidy(survey_field_run$env$survey_fit)
field_gradient <- broom::tidy(survey_field_run$env$field_fit)

text_rows <- tribble(
  ~table_figure, ~claim, ~value_script,
  "text", "Field studies collected", nrow(ates_run$env$ates),
  "text", "Field studies with a positive ATE", sum(ates_run$env$ates$estimate >= 0),
  "text", "Occupation CATEs meta-analysed", nrow(cates_run$env$occupation_cates),
  "text", "Mean occupations per study", mean(occupations_per_study$n),
  "text", "Median occupations per study", median(occupations_per_study$n),
  "text", "Predicted CATE at 0 percent women (pp)", archive_predictions$pred[1],
  "text", "Predicted CATE at 100 percent women (pp)", archive_predictions$pred[2],
  "text", "Difference in magnitude, 100 vs 0 percent women (pp)", cates_run$env$difference_in_magnitude,
  "text", "Bootstrapped SE of the difference in magnitude", sd(cates_run$env$difference_in_magnitude_bootstrap),
  "text_S3", "Survey experiments collected", nrow(survey_ates_run$env$survey_studies),
  "text_S3", "Survey occupation CATEs", nrow(survey_field_run$env$survey_df),
  "text_S5", "Study-level gender gradients estimated", nrow(gradient_run$env$gender_gradients_df),
  "text_S5", "Gender gradients significant at p <= 0.05", sum(gradient_run$env$gender_gradients_df$p.value <= 0.05),
  "fig_S3.3", "Field experimental gradient", field_gradient$estimate[field_gradient$term == "fraction_female"],
  "fig_S3.3", "Field experimental intercept", field_gradient$estimate[field_gradient$term == "intercept"],
  "fig_S3.3", "Survey experimental gradient", survey_gradient$estimate[survey_gradient$term == "fraction_female"],
  "fig_S3.3", "Survey experimental intercept", survey_gradient$estimate[survey_gradient$term == "intercept"]
)

# Study by study panels ----
# The deposited loop stops before it draws anything, so the archive produces none of the
# 57 published panels and none of the 335 labels on them. That is recorded as a count of zero
# rather than left out, so the gap is visible in the ground truth.
panel_rows <- tibble(
  table_figure = "figs_S4.5_to_S4.61",
  claim = c("Study panels drawn by the deposited archive",
            "Plotted estimate and standard error labels produced by the deposited archive"),
  value_script = 0
)

archive_values <- bind_rows(
  text_rows,
  table_1,
  table_2,
  fig_s1_1,
  fig_s3_4,
  fig_s5_62,
  panel_rows
) |>
  mutate(claim = as.character(claim), table_figure = as.character(table_figure))

write_csv(archive_values, here::here("ground_truth", "archive_values.csv"))

print(tibble(
  script = c("analyze_ates.R", "analyze_cates.R", "analyze_cates_regression_tables.R",
             "appendix_cates_survey_field.R", "appendix_survey_ates.R",
             "appendix_gender_gradient.R", "appendix_figures_and_loop.R"),
  failed = c(ates_run$failed, cates_run$failed, tables_run$failed, survey_field_run$failed,
             survey_ates_run$failed, gradient_run$failed, loop_run$failed)
))
print(str_glue("{nrow(archive_values)} values harvested from the deposited archive."))
