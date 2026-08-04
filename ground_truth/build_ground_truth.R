# galos_coppock_2023/ground_truth/build_ground_truth.R
# Output: ground_truth/galos_coppock_2023_ground_truth.csv
# Depends on: ground_truth/archive_values.csv, ground_truth/published_claims.csv,
#             maintained/in_text_claims.R, maintained/output/ (run run_all.R first)
# Description: Assemble the number-by-number comparison table, and gate it.
#
#   Three columns, three sources, and they are kept apart on purpose. value_paper is typed from
#   the published article and its supplementary materials and appears nowhere else in this
#   repository except the extraction; it is a comparison target and never an input to a
#   calculation. value_script is read from ground_truth/archive_values.csv, which
#   archive_values.R harvests by running the deposited code. value_rewrite is read out of
#   maintained/output/. Nothing in this file is transcribed from a run, and the verdicts are
#   computed rather than asserted.
#
#   Every published value carries the number of decimal places at which the article prints it,
#   so agreement is tested at the precision the reader sees rather than at whatever precision a
#   double happens to have, and value_paper is written out as the string the page prints rather
#   than as a double, which would turn the published 1.50 into 1.5.
#
#   Three comparison modes. Most claims are printed values, compared at the article's own
#   precision. A claim that reduces to a truth value (a majority, a sign, a ranking) is compared
#   as "holds": the rewrite side is 1 or 0 and the article's side is 1. A claim that names a
#   rough magnitude ("two-thirds", "over the past four decades", "very similar") is
#   "approximate": both numbers are recorded and no verdict is returned, because forcing a
#   string comparison there files an honest hedge as a defect.
#
#   Four gates run at the end. Every non-clean-match row must name where the fault lies; every
#   published float must have coverage, stated as a fraction of the numbers it prints; the
#   extraction in ground_truth/published_claims.csv must agree with this file's transcription of
#   the same pages; and maintained/in_text_claims.R must run, print a claim line for every claim
#   the extraction says needs one, and land on the same numbers this file does.

library(here)
library(tidyverse)

here::i_am("ground_truth/build_ground_truth.R")

paper_id <- "galos_coppock_2023"

out <- function(f) read_csv(here::here("maintained", "output", f), show_col_types = FALSE)

archive <- read_csv(here::here("ground_truth", "archive_values.csv"), show_col_types = FALSE)

summary_stats <- out("text_summary_stats.csv")
magnitude <- out("text_difference_in_magnitude.csv")
table_1 <- out("table_1_meta_regression.csv")
table_2 <- out("table_2_ethnicity_meta_regression.csv")
forest_s1_1 <- out("figure_s1_1_field_ates_forest.csv")
forest_s3_4 <- out("figure_s3_4_survey_ates_forest.csv")
forest_s5_62 <- out("figure_s5_62_gender_gradient_forest.csv")
survey_field <- out("figure_s3_3_survey_field_comparison.csv")
panels <- out("figure_s4_study_panels.csv")
study_manifest <- out("table_s2_1_study_manifest.csv")
evidence_map <- out("figure_s2_2_evidence_map.csv")
gradients <- out("gender_gradients.csv")

stat <- function(name) summary_stats$value[summary_stats$quantity == name]
mag <- function(name) magnitude$value[magnitude$quantity == name]

# A stable id for a study label, matching the one ground_truth/extract_published_claims.R
# builds, so a claim typed here and the same claim read off the published page meet under one
# claim_id however the two spell the minus sign.
claim_slug <- function(x) {
  x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_replace_all("^_|_$", "")
}

stars_from_p <- function(p) {
  case_when(is.na(p) ~ NA_real_, p < 0.001 ~ 3, p < 0.01 ~ 2, p < 0.05 ~ 1, .default = 0)
}

meta_row <- function(forest, column) {
  value <- forest[[column]][forest$facet == "Meta"]
  stopifnot(length(value) == 1)
  value
}

gradient_at <- function(type) {
  value <- survey_field$estimate[survey_field$experiment_type == type &
                                   survey_field$term == "fraction_female"]
  stopifnot(length(value) == 1)
  value
}

table_1_gradient <- function(model) {
  value <- table_1$estimate[table_1$model == model & table_1$term == "fraction_female"]
  stopifnot(length(value) == 1)
  value
}

# The earliest year from which at least 95 per cent of the evidence map's country-year cells
# come. The article dates the boom in audit experimentation to "around 2005", which is a claim
# about where the mass of the record starts rather than about any one study.
boom_year <- function() {
  years <- sort(evidence_map$year)
  years[ceiling(0.05 * length(years)) + 1]
}

# Published values: main text and the supplementary sections ----
# Galos and Coppock (2023), Science Advances 9, eade7979, and its supplementary materials.
# claim_id ties each row to ground_truth/published_claims.csv and to the block that recomputes
# it in maintained/in_text_claims.R. A row with no claim_id records a quantity the article does
# not state.
text_claims <- tribble(
  ~claim_id, ~table_figure, ~claim, ~value_paper, ~digits, ~mode, ~value_rewrite, ~locus, ~notes,

  # Abstract
  "abstract_since_year", "text", "Year the audit experimental record begins",
    1983, 0, "value", min(evidence_map$year), NA,
    "Abstract: 'Since 1983, more than 70 employment audit experiments'. The evidence map's earliest country-year cell",
  "abstract_countries", "text", "Countries the experiments were carried out in",
    26, 0, "approximate", n_distinct(evidence_map$country), NA,
    paste("Abstract: 'carried out in more than 26 countries across five continents'. The",
          "evidence map records the countries of the 57 studies reanalysed here and not those of",
          "the wider literature the sentence describes, so both numbers are recorded and no",
          "verdict is returned"),
  "abstract_continents", "text", "Continents the experiments were carried out across",
    5, 0, "value", n_distinct(evidence_map$continent), "paper_internal",
    paste("Abstract: 'carried out in more than 26 countries across five continents'. The",
          "paper's own evidence map, fig. S2.2, is drawn as six continent facets and section S2",
          "names Oceania among the regions where the boom occurred. The abstract describes the",
          "wider literature rather than the 57 studies reanalysed here, but a superset of those",
          "studies cannot span fewer continents, so five is too few under any reading"),
  "abstract_mixed_signs", "text", "Study-level effects run in both directions",
    1, 0, "holds",
    as.numeric(min(forest_s1_1$estimate[forest_s1_1$facet == "Study estimates"]) < 0 &
                 max(forest_s1_1$estimate[forest_s1_1$facet == "Study estimates"]) > 0), NA,
    "Abstract: 'The results are mixed: Some studies find discrimination against men, and others find discrimination against women'",

  # Introduction
  "intro_riach_ate", "text", "Riach and Rich (8) effect of being a woman (pp)",
    -2.4, 1, "value",
    forest_s1_1$estimate[forest_s1_1$study_name_citation == "Riach and Rich (8): Australia"], NA,
    "Introduction: 'found a -2.4 percentage point effect of being a woman on receiving an invitation to interview'",
  "intro_riach_se", "text", "Riach and Rich (8) cluster-robust SE",
    1.2, 1, "value",
    forest_s1_1$std.error[forest_s1_1$study_name_citation == "Riach and Rich (8): Australia"], NA,
    "Introduction: '(cluster-robust SE: 1.2 points)'",
  "intro_neumark_ate", "text", "Neumark et al. (9) effect of being a woman (pp)",
    -12.3, 1, "value",
    forest_s1_1$estimate[forest_s1_1$study_name_citation == "Neumark et al. (9): United States"], NA,
    "Introduction: 'finding a large and negative but nonsignificant effect (-12.3 points, SE: 8.4 points)'",
  "intro_neumark_se", "text", "Neumark et al. (9) SE",
    8.4, 1, "value",
    forest_s1_1$std.error[forest_s1_1$study_name_citation == "Neumark et al. (9): United States"], NA,
    "Introduction: '(-12.3 points, SE: 8.4 points)'",
  "intro_studies", "text", "Field studies collected",
    57, 0, "value", stat("Field studies"), NA,
    "Introduction: 'We have obtained or reconstructed the data for a total of 57 such studies'",
  "intro_positive", "text", "Field studies with a positive ATE",
    37, 0, "value", stat("Field studies with a positive ATE"), NA,
    "Introduction: 'two-thirds (37 of 57) show a positive effect of being a woman'",
  "intro_positive_of", "text", "Field studies the 37 positive effects are counted out of",
    57, 0, "value", stat("Field studies"), NA,
    "Introduction: the denominator of '37 of 57', restated from the sentence before",
  "intro_two_thirds", "text", "Share of studies with a positive ATE",
    0.667, 3, "approximate",
    stat("Field studies with a positive ATE") / stat("Field studies"), NA,
    "Introduction: 'two-thirds (37 of 57)'. A spelled-out approximation, recorded with no verdict",
  "intro_meta_ate", "text", "Random-effects meta-analytic ATE (pp)",
    1.2, 1, "value", meta_row(forest_s1_1, "estimate"), NA,
    "Introduction: 'yields an estimate of 1.2 percentage points with an SE of 0.4 points'",
  "intro_meta_se", "text", "SE of the meta-analytic ATE",
    0.4, 1, "value", meta_row(forest_s1_1, "std.error"), NA,
    "Introduction: 'an SE of 0.4 points'",

  # Theory
  "theory_composition_min", "text", "Gender composition of a fully men-dominated occupation",
    0, 0, "value", min(round(panels$fraction_female, 10), na.rm = TRUE), NA,
    "Theory: 'Among occupations that are fully men dominated, gender composition is 0'. The smallest composition the data carry is the floor of the scale rather than the definition itself",
  "theory_composition_max", "text", "Gender composition of a fully women-dominated occupation",
    1, 0, "value", ceiling(max(panels$fraction_female, na.rm = TRUE)), NA,
    "Theory: 'among occupations that are fully women-dominated, gender composition is 1'. The scale's ceiling, recovered from the observed range",

  # Design and answer strategy
  "design_studies", "text", "Separate audit studies the 51 papers give",
    57, 0, "value", stat("Field studies"), NA,
    "Design: 'for a total of 57 separate audit studies'",
  "design_boom_year", "text", "Year from which the mass of the evidence map dates",
    2005, 0, "value", boom_year(), NA,
    "Design: 'The explosion in audit experimentation began around 2005'. Read as the earliest year from which 95 per cent of the country-year cells come",
  "design_s4_studies", "text", "Studies whose CATE estimates section S4 presents",
    57, 0, "value", n_distinct(panels$study_name_citation), NA,
    "Answer strategy: 'We present CATE estimates for all 57 studies in section S4'",

  # Results
  "results_ci_level", "text", "Confidence level of the region overlaid on fig. 4 (per cent)",
    95, 0, "value",
    100 * (2 * pnorm(mean((table_1$conf.high[table_1$model == "Model 1"] -
                             table_1$estimate[table_1$model == "Model 1"]) /
                            table_1$std.error[table_1$model == "Model 1"])) - 1), NA,
    "Results: 'The meta-regression fit and 95% confidence region are overlaid on the data'. Recovered from the half width of the Table 1 model 1 intervals in standard errors",
  "results_gradient", "text", "Gender gradient quoted into the text",
    8.15, 2, "value", table_1_gradient("Model 1"), NA,
    "Results: 'We find that gender gradient is 8.15 with an SE of 1.50'",
  "results_gradient_se", "text", "SE of the gender gradient quoted into the text",
    1.50, 2, "value",
    table_1$std.error[table_1$model == "Model 1" & table_1$term == "fraction_female"], NA,
    "Results: 'with an SE of 1.50'",
  "results_cate_0", "text", "Predicted CATE at 0 percent women (pp)",
    -3.02, 2, "value", mag("Predicted CATE at 0 percent women"), NA,
    "Results: 'In an occupation with 0% women, we would expect the average CATE to be -3.02 percentage points'",
  "results_cate_0_restated", "text", "The same intercept, restated inside the arithmetic",
    -3.02, 2, "value", mag("Predicted CATE at 0 percent women"), NA,
    "Results: 'we would expect the average CATE to be -3.02 + 8.15 = +5.13 points'",
  "results_gradient_restated", "text", "The same gradient, restated inside the arithmetic",
    8.15, 2, "value", table_1_gradient("Model 1"), NA,
    "Results: 'we would expect the average CATE to be -3.02 + 8.15 = +5.13 points'",
  "results_cate_100", "text", "Predicted CATE at 100 percent women (pp)",
    5.13, 2, "value", mag("Predicted CATE at 100 percent women"), NA,
    "Results: 'in an occupation with 100% women, we would expect the average CATE to be -3.02 + 8.15 = +5.13 points'",
  "results_magnitude_difference", "text", "Difference in magnitude, 100 vs 0 percent women (pp)",
    2.10, 2, "value", mag("Difference in magnitude"), "paper_internal",
    "Results: 'by a statistically significant 2.10 points'",
  "results_magnitude_se", "text", "Bootstrapped SE of the difference in magnitude",
    0.58, 2, "value", mag("Bootstrapped SE of the difference in magnitude"), NA,
    "Results: '(bootstrapped SE: 0.58)'",
  "results_gradient_stable", "text", "Spread of the gender gradient across the five Table 1 models",
    NA, 2, "approximate",
    max(table_1$estimate[table_1$term == "fraction_female"]) -
      min(table_1$estimate[table_1$term == "fraction_female"]), NA,
    "Results: 'Across these specifications, the estimated gender gradient remains quite stable'. The article states no number, so the spread is recorded with no verdict",
  "results_minority_steeper", "text", "Gender gradient steeper for minority than majority applicants",
    1, 0, "holds",
    as.numeric(table_2$estimate[table_2$model == "Model 2" & table_2$term == "fraction_female"] >
                 table_2$estimate[table_2$model == "Model 1" & table_2$term == "fraction_female"]), NA,
    "Results: 'the gender gradient is estimated to be steeper for minority applicants than for majority applicants'",
  "results_interaction_not_significant", "text",
    "Majority by gradient interaction not significant at the 0.05 level",
    1, 0, "holds",
    as.numeric(table_2$p.value[table_2$model == "Model 3" &
                                 table_2$term == "fraction_female:ethnicity_fctMajority"] > 0.05), NA,
    "Results: 'the uncertainty attending to the estimates is too great to declare the difference between them statistically significant'",

  # Discussion
  "disc_decades", "text", "Decades of audit experimentation synthesized",
    4, 1, "approximate", (max(evidence_map$year) - min(evidence_map$year)) / 10, NA,
    "Discussion: 'what has been learned over the past four decades'. A spelled-out approximation",
  "disc_gradient_positive", "text", "Pooled gender gradient is positive and excludes zero",
    1, 0, "holds",
    as.numeric(table_1$estimate[table_1$model == "Model 1" & table_1$term == "fraction_female"] > 0 &
                 table_1$conf.low[table_1$model == "Model 1" &
                                    table_1$term == "fraction_female"] > 0), NA,
    "Discussion: 'The gender gradient is positive: Women are discriminated against where there are few women, and men are discriminated against where there are few men'",
  "disc_gradient_studies", "text", "Studies with a study-specific gender gradient estimate",
    37, 0, "value", stat("Study-level gender gradients"), NA,
    "Discussion: 'we can obtain study-specific gender gradient estimates for 37 of our studies'",
  "disc_significant", "text", "Study-specific gender gradients reaching significance",
    7, 0, "value", stat("Gender gradients significant at p <= 0.05"), NA,
    "Discussion: 'only 7 of which reach statistical significance'",
  "disc_power_numerator", "text", "Numerator of the power ratio 7/37",
    7, 0, "value", stat("Gender gradients significant at p <= 0.05"), NA,
    "Discussion: 'at 7/37 = 19%'",
  "disc_power_denominator", "text", "Denominator of the power ratio 7/37",
    37, 0, "value", stat("Study-level gender gradients"), NA,
    "Discussion: 'at 7/37 = 19%'",
  "disc_power", "text", "Power of the average audit study to detect the gender gradient (per cent)",
    19, 0, "value",
    100 * stat("Gender gradients significant at p <= 0.05") / stat("Study-level gender gradients"), NA,
    "Discussion: 'puts the statistical power of the average gender employment audit study to detect the gender gradient at 7/37 = 19%'",
  "disc_synthesized", "text", "Studies synthesized",
    57, 0, "value", stat("Field studies"), NA,
    "Discussion: 'by synthesizing 57 of them, we learn that gender composition predicts gender bias'",
  "disc_since_year", "text", "Year since which most of the record dates",
    2005, 0, "value", boom_year(), NA,
    "Discussion: 'Most were conducted in North America and Europe since 2005'",
  "disc_most_europe_north_america", "text",
    "Most country-year cells are in Europe or North America",
    1, 0, "holds",
    as.numeric(sum(evidence_map$continent %in% c("Europe", "N. America")) >
                 nrow(evidence_map) / 2), NA,
    "Discussion: 'Most were conducted in North America and Europe since 2005'",
  "disc_other_continents", "text",
    "Country-year cells in South America, Asia or Africa",
    NA, 0, "approximate",
    sum(evidence_map$continent %in% c("S. America", "Asia", "Africa")), NA,
    "Discussion: 'only a handful of studies have been conducted in South America, Asia, or Africa'. The article states no number, so the count is recorded with no verdict",

  # Section S1
  "s1_studies", "text_S1", "Studies in the fig. S1.1 forest plot",
    57, 0, "value", sum(forest_s1_1$facet == "Study estimates"), NA,
    "Section S1: 'Figure S1.1 shows a standard forest plot of the 57 studies in our database'",
  "s1_meta_ate", "text_S1", "Meta-analytic average effect (pp)",
    1.2, 1, "value", meta_row(forest_s1_1, "estimate"), NA,
    "Section S1: 'the effect of being a woman (versus a man) is a 1.2 percentage point increase in callback'",
  "s1_meta_se", "text_S1", "SE of the meta-analytic average",
    0.4, 1, "value", meta_row(forest_s1_1, "std.error"), NA,
    "Section S1: '(SE: 0.4)'",

  # Section S2
  "s2_early_1980s", "text_S2", "Evidence map cells in the 1980s",
    1, 0, "value", sum(evidence_map$year %/% 10 == 198), NA,
    "Section S2: 'We see one early study in Australia in the 1980s'",
  "s2_early_1990s", "text_S2", "Evidence map cells in the 1990s",
    1, 0, "value", sum(evidence_map$year %/% 10 == 199), NA,
    "Section S2: 'followed by one study in the United States in the 1990s'",
  "s2_boom_year", "text_S2", "Year from which the mass of the evidence map dates, restated",
    2005, 0, "value", boom_year(), NA,
    "Section S2: 'We see a boom in audit experiments beginning around 2005 in Europe, North America, and Oceania'",

  # Section S3
  "s3_survey_experiments", "text_S3", "Survey experiments collected",
    12, 0, "value", stat("Survey experiments"), NA,
    "Section S3: 'we also collected 12 vignette survey experiments'",
  "s3_gradients_similar", "text_S3",
    "Distance between the survey and field gender gradients",
    NA, 2, "approximate",
    abs(gradient_at("Survey experiments") - gradient_at("Field experiments")), NA,
    "Section S3: 'we find that the gender gradient (the slope with respect to gender composition) is very similar across the two experimental settings'. The article states no number, so the distance is recorded with no verdict",

  # Section S4
  "s4_minimum_cates", "text_S4", "Occupation CATEs a study needs before a gradient is estimated",
    3, 0, "value", min(gradients$n_occupations), NA,
    "Section S4: 'When we have a sufficient number of CATEs (3), we can estimate a study-level gender gradient'",
  "s4_gradients", "text_S4", "Gender gradients estimated across the 57 studies",
    37, 0, "value", nrow(gradients), NA,
    "Section S4: 'The 37 gender gradients we are able to estimate are meta-analyzed in Figure S5.62'",

  # Section S5
  "s5_gradients", "text_S5", "Study-by-study gender gradient estimates meta-analysed",
    37, 0, "value", sum(forest_s5_62$facet == "Study gender gradient estimates"), NA,
    "Section S5: 'We can meta-analyze the 37 study-by-study gender gradient estimates'",
  "s5_minimum_occupations", "text_S5", "Occupations a study needs to enter the meta-analysis",
    3, 0, "value", min(gradients$n_occupations), NA,
    "Section S5: 'can only include studies with three or more occupations'",
  "s5_fixed_effects_minimum", "text_S5",
    "Occupations a study needs to enter the study fixed effects estimation",
    2, 0, "value",
    min(count(panels, study_name_citation)$n[count(panels, study_name_citation)$n > 1]), NA,
    "Section S5: 'the fixed effects estimation includes the study if it distinguishes among two or more occupations'. The smallest multi-occupation study in the panels",
  "s5_studies", "text_S5", "Study-level gender gradients estimated",
    37, 0, "value", stat("Study-level gender gradients"), NA,
    "Section S5: 'can only include studies with three or more occupations (37 studies)'",
  "s5_meta_gradient", "text_S5", "Meta-analytic average gender gradient",
    9.0, 1, "value", meta_row(forest_s5_62, "estimate"), NA,
    "Section S5: 'The meta-analytic average gender gradient is 9.0'",
  "s5_meta_se", "text_S5", "SE of the meta-analytic average gender gradient",
    2.1, 1, "value", meta_row(forest_s5_62, "std.error"), NA,
    "Section S5: 'with a standard error of 2.1'",
  "s5_similar_to_table_1", "text_S5",
    "Distance between the gradient meta-analysis and the Table 1 model 1 gradient",
    NA, 2, "approximate", abs(meta_row(forest_s5_62, "estimate") - table_1_gradient("Model 1")), NA,
    "Section S5: 'which is quite similar to the gender gradient estimates presented in Table 1'. The article states no number, so the distance is recorded with no verdict",
  "s5_significant", "text_S5", "Gender gradients significant at p <= 0.05",
    7, 0, "value", stat("Gender gradients significant at p <= 0.05"), NA,
    "Section S5: 'Only 7 of these 37 gender gradient estimates are statistically significant'",
  "s5_significant_of", "text_S5", "Gradients the seven significant ones are counted out of",
    37, 0, "value", stat("Study-level gender gradients"), NA,
    "Section S5: 'Only 7 of these 37'",

  # Quantities the pipeline produces and the article never prints
  NA, "text", "Mean occupations per study",
    NA, 2, "value", stat("Mean occupations per study"), NA,
    "Computed by the deposited code and never printed in the article",
  NA, "text", "Median occupations per study",
    NA, 0, "value", stat("Median occupations per study"), NA,
    "Computed by the deposited code and never printed in the article",
  NA, "text_S3", "Survey occupation CATEs",
    NA, 0, "value", stat("Survey occupation CATEs"), NA,
    "Used by fig. S3.3 and never printed in the article",
  NA, "fig_S3.3", "Field experimental gradient",
    NA, 2, "value", gradient_at("Field experiments"), NA,
    "Fig. S3.3 draws the two fitted lines and prints no numbers",
  NA, "fig_S3.3", "Field experimental intercept",
    NA, 2, "value",
    survey_field$estimate[survey_field$experiment_type == "Field experiments" &
                            survey_field$term == "intercept"], NA,
    "Fig. S3.3 draws the two fitted lines and prints no numbers",
  NA, "fig_S3.3", "Survey experimental gradient",
    NA, 2, "value", gradient_at("Survey experiments"), NA,
    "Fig. S3.3 draws the two fitted lines and prints no numbers",
  NA, "fig_S3.3", "Survey experimental intercept",
    NA, 2, "value",
    survey_field$estimate[survey_field$experiment_type == "Survey experiments" &
                            survey_field$term == "intercept"], NA,
    "Fig. S3.3 draws the two fitted lines and prints no numbers"
)

# The page's own precision, where it differs from the precision the computed value needs. The
# article spells "four decades" as a whole number and the evidence map spans 3.8 of them, so the
# published string is written out at no decimals and the comparison keeps one.
text_claims <- text_claims |>
  mutate(paper_digits = if_else(claim_id %in% c("disc_decades"), 0, digits))

# Published values: Tables 1 and 2 ----
# Coefficients, standard errors and significance markers as typeset on pages 7 and 8.
published_table_1 <- tribble(
  ~model, ~term, ~estimate, ~std_error, ~stars, ~tau_squared, ~nobs,
  "Model 1", "Intercept", -3.02, 0.81, 3, 12.57, 298,
  "Model 1", "Gender gradient", 8.15, 1.50, 3, 12.57, 298,
  "Model 2", "Intercept", -2.48, 1.58, 0, 11.71, 298,
  "Model 2", "Gender gradient", 8.33, 1.58, 3, 11.71, 298,
  "Model 3", "Intercept", -5.43, 1.91, 2, 12.56, 298,
  "Model 3", "Gender gradient", 8.69, 1.56, 3, 12.56, 298,
  "Model 4", "Intercept", -5.20, 1.82, 2, 10.22, 298,
  "Model 4", "Gender gradient", 7.97, 1.57, 3, 10.22, 298,
  "Model 5", "Intercept", 1.05, 1.96, 0, 8.89, 298,
  "Model 5", "Gender gradient", 7.51, 1.54, 3, 8.89, 298
)

published_table_2 <- tribble(
  ~model, ~term, ~estimate, ~std_error, ~stars, ~tau_squared, ~nobs,
  "Model 1", "Intercept", -1.55, 1.31, 0, 6.60, 91,
  "Model 1", "Gender gradient", 5.70, 2.50, 1, 6.60, 91,
  "Model 2", "Intercept", -3.72, 1.13, 0, 6.28, 96,
  "Model 2", "Gender gradient", 9.08, 2.26, 3, 6.28, 96,
  "Model 3", "Intercept", -3.73, 1.14, 2, 6.44, 187,
  "Model 3", "Gender gradient", 9.11, 2.28, 3, 6.44, 187,
  "Model 3", "Majority status", 2.19, 1.74, 0, 6.44, 187,
  "Model 3", "Majority x gradient", -3.42, 3.37, 0, 6.44, 187
)

# The rewrite's table CSVs name their terms as metafor does; the published tables name them
# in words. The mapping is one to one.
rewrite_table_terms <- c(
  intercept = "Intercept",
  fraction_female = "Gender gradient",
  ethnicity_fctMajority = "Majority status",
  `fraction_female:ethnicity_fctMajority` = "Majority x gradient"
)

# The published tables name their terms in words and the extraction keys them by a short
# machine name, so the two meet through this map rather than through a slug of the label.
table_term_ids <- c(Intercept = "intercept", `Gender gradient` = "gradient",
                    `Majority status` = "majority",
                    `Majority x gradient` = "majority_x_gradient")

table_rows <- function(published, rewrite, stem) {
  rewrite <- rewrite |>
    mutate(term_label = unname(rewrite_table_terms[term]),
           stars = stars_from_p(p.value)) |>
    select(model, term_label, estimate, std.error, stars, tau.squared, nobs)

  # Three rows per published coefficient: the estimate, its standard error, and its marker.
  published |>
    left_join(rewrite, by = c("model", "term" = "term_label")) |>
    pivot_longer(
      c(estimate.x, std_error, stars.x, estimate.y, std.error, stars.y),
      names_to = "column", values_to = "value"
    ) |>
    mutate(
      part = case_when(column %in% c("estimate.x", "estimate.y") ~ "estimate",
                       column %in% c("std_error", "std.error") ~ "se",
                       .default = "stars"),
      side = if_else(column %in% c("estimate.x", "std_error", "stars.x"), "paper", "rewrite")
    ) |>
    select(model, term, part, side, value) |>
    pivot_wider(names_from = side, values_from = value) |>
    transmute(
      claim_id = str_glue(
        "{stem}_{str_replace(str_to_lower(model), ' ', '')}_{table_term_ids[term]}_",
        "{case_when(part == 'estimate' ~ 'estimate', part == 'se' ~ 'std_error', .default = 'stars')}"
      ),
      table_figure = str_glue("{stem}_{str_replace(str_to_lower(model), ' ', '')}"),
      claim = case_when(
        part == "estimate" ~ term,
        part == "se" ~ str_glue("{term} SE"),
        .default = str_glue("{term} significance stars (0 = none, 1 = one, 2 = two, 3 = three)")
      ),
      value_paper = paper,
      digits = if_else(part == "stars", 0, 2),
      value_rewrite = rewrite,
      notes = "Typeset coefficient, standard error and significance marker"
    )
}

gof_rows <- function(published, rewrite, stem) {
  rewrite_gof <- rewrite |>
    distinct(model, tau.squared, nobs) |>
    rename(rewrite_tau_squared = tau.squared, rewrite_nobs = nobs)
  published |>
    distinct(model, tau_squared, nobs) |>
    left_join(rewrite_gof, by = "model") |>
    pivot_longer(c(tau_squared, nobs, rewrite_tau_squared, rewrite_nobs),
                 names_to = "column", values_to = "value") |>
    mutate(
      part = if_else(str_detect(column, "tau"), "Tau squared", "Nobs"),
      side = if_else(str_starts(column, "rewrite_"), "rewrite", "paper")
    ) |>
    select(model, part, side, value) |>
    pivot_wider(names_from = side, values_from = value) |>
    transmute(
      claim_id = str_glue("{stem}_{str_replace(str_to_lower(model), ' ', '')}_",
                          "{if_else(part == 'Nobs', 'nobs', 'tau_squared')}"),
      table_figure = str_glue("{stem}_{str_replace(str_to_lower(model), ' ', '')}"),
      claim = part,
      value_paper = paper,
      digits = if_else(part == "Nobs", 0, 2),
      value_rewrite = rewrite,
      notes = "Goodness-of-fit row of the published table"
    )
}

# Published values: the study manifest, table S2.1 ----
# 57 rows of three columns. The deposit has no code for this table, so all three columns are
# rebuilt in table_s2_1_study_manifest.R and compared column by column here.
published_s2_1 <- tribble(
  ~label, ~n_subjects, ~n_occupations, ~included,
  "Neumark et al. (9): United States", 130, 1, "No",
  "Rivera and Tilcsik (101): United States, field experiment", 316, 1, "No",
  "Birkelund et al. (76): United States", 502, 6, "No",
  "Birkelund et al. (76): Norway", 547, 6, "No",
  "Birkelund et al. (76): Germany", 717, 6, "No",
  "Birkelund et al. (76): United Kingdom", 786, 6, "No",
  "Hipp (90): Germany", 820, 1, "No",
  "Baert et al. (72): Belgium", 864, 1, "No",
  "Petit (97): France", 942, 1, "No",
  "Birkelund et al. (76): Spain", 959, 6, "No",
  "Birkelund et al. (76): the Netherlands", 982, 6, "No",
  "Baert et al. (71): Belgium", 1152, 1, "No",
  "Saeed et al. (104): Pakistan", 1216, 1, "No",
  "Correll et al. (83): United States, field experiment", 1276, 1, "No",
  "Wu (106): China", 1344, 1, "No",
  "Capéau et al. (82): Belgium", 1607, 1, "No",
  "Erlandsson (86): Sweden", 1643, 3, "No",
  "Riach and Rich (100): England", 1746, 4, "No",
  "Rooth (102): Sweden", 1970, 7, "No",
  "Riach and Rich (8): Australia", 1982, 7, "No",
  "Thomas (4): United States, field experiment", 2096, 4, "No",
  "Quadlin (98): United States, field experiment", 2106, 1, "No",
  "Bygren et al. (78): Sweden", 2144, 13, "No",
  "Patacchini et al. (95): Italy", 2320, 7, "No",
  "Pedulla (96): United States, field experiment", 2420, 4, "No",
  "Albert et al. (67): Spain", 2760, 3, "No",
  "Ahmed at al. (14): Sweden", 3254, 15, "No",
  "Booth and Leigh (15): Australia", 3365, 4, "No",
  "Bursell (77): Sweden", 3636, 1, "No",
  "Ahmed at al. (66): Sweden", 3990, 10, "No",
  "Berson (75): France", 5000, 1, "No",
  "Jackson (91): United Kingdom", 5120, 1, "No",
  "Ruffle and Shtudiner (103): Israel", 5312, 10, "No",
  "Gonzalez et al. (89): Spain", 5620, 18, "No",
  "Carlsson et al. (81): Sweden", 5662, 3, "No",
  "Carlsson and Eriksson (80): Sweden", 6066, 7, "No",
  "Yavorsky (107): United States", 6302, 1, "No",
  "Horváth (16): China", 6404, 2, "No",
  "Carlsson (13): Sweden", 6456, 13, "No",
  "Becker et al. (74): Austria, Germany, Switzerland", 6690, 2, "No",
  "Mavlikeeva and Asanov (11): Russia", 8328, 6, "No",
  "Zhou et al. (108): China", 19130, 4, "No",
  "Maurer-Fazio and Lei (92): China", 24192, 4, "No",
  "Arai et al. (69): Sweden (equivalent CVs)", 566, 5, "Yes",
  "Arai et al. (69): Sweden (enhanced CVs)", 584, 5, "Yes",
  "Dahl and Krog (84): Denmark", 800, 24, "Yes",
  "Banerjee et al. (73): India", 1324, 2, "Yes",
  "Alden et al. (68): Sweden", 1350, 2, "Yes",
  "Asali et al. (70): Georgia", 2200, 9, "Yes",
  "Galarza and Yamada (88): Peru", 3828, 3, "Yes",
  "Galarza and Yamada (87): Peru", 4820, 3, "Yes",
  "Ramos et al. (99): Spain and the Netherlands", 9231, 7, "Yes",
  "Nunley et al. (93): United States", 9396, 6, "Yes",
  "Di Stasio and Larsen (105): United Kingdom, Germany and Norway", 9425, 10, "Yes",
  "Oreopoulos (94): Canada", 12910, 21, "Yes",
  "Edo et al. (85): France", 18144, 3, "Yes",
  "Busetta et al. (79): Italy", 21998, 1, "Yes"
)

manifest_gt <- published_s2_1 |>
  mutate(label = str_squish(label)) |>
  left_join(
    study_manifest |>
      transmute(label = str_squish(study_name_citation),
                rewrite_subjects = n_subjects_from_df,
                rewrite_occupations = n_occupations,
                rewrite_included = included_majority_minority),
    by = "label"
  ) |>
  summarize(
    `Studies listed` = sum(!is.na(rewrite_included)),
    `Subject counts reproduced` = sum(n_subjects == rewrite_subjects, na.rm = TRUE),
    `Occupation counts reproduced` = sum(n_occupations == rewrite_occupations, na.rm = TRUE),
    `Majority / minority inclusion reproduced` = sum(included == rewrite_included, na.rm = TRUE)
  ) |>
  pivot_longer(everything(), names_to = "claim", values_to = "value_rewrite") |>
  transmute(
    claim_id = unname(c(
      "Studies listed" = "table_s2_1_studies_listed",
      "Subject counts reproduced" = "table_s2_1_subjects",
      "Occupation counts reproduced" = "table_s2_1_occupations",
      "Majority / minority inclusion reproduced" = "table_s2_1_inclusion"
    )[claim]),
    table_figure = "table_S2.1",
    claim,
    value_paper = nrow(published_s2_1),
    digits = 0,
    value_rewrite,
    notes = paste("Count of the 57 published rows whose cell the rewrite reproduces in this",
                  "column. The deposit ships no code for table S2.1, which is why value_script",
                  "is empty")
  )

# Published values: the three forest plots ----
# Figs. S1.1, S3.4 and S5.62 print an estimate and standard error beside every point. These
# 218 numbers appear nowhere in the prose, so they are typed from the supplementary PDF.
published_s1_1 <- tribble(
  ~label, ~estimate, ~std_error,
  "Albert et al. (67): Spain", 9.9, 1.8,
  "Birkelund et al. (76): the Netherlands", 8.3, 3.2,
  "Becker et al. (74): Austria, Germany, Switzerland", 8.1, 0.9,
  "Birkelund et al. (76): Germany", 7.5, 3.7,
  "Booth and Leigh (15): Australia", 7.2, 1.6,
  "Birkelund et al. (76): Spain", 6.2, 2.7,
  "Hipp (90): Germany", 5.2, 3.2,
  "Alden et al. (68): Sweden", 4.8, 2.2,
  "Birkelund et al. (76): United Kingdom", 4.8, 3.1,
  "Edo et al. (85): France", 4.8, 1.1,
  "Ahmed at al. (14): Sweden", 4.7, 1.6,
  "Riach and Rich (100): England", 4.1, 1.2,
  "Di Stasio and Larsen (105): United Kingdom, Germany and Norway", 3.6, 0.9,
  "Rooth (102): Sweden", 3.4, 2.7,
  "Arai et al. (69): Sweden (enhanced CVs)", 3.0, 3.9,
  "Carlsson (13): Sweden", 2.9, 1.0,
  "Mavlikeeva and Asanov (11): Russia", 2.7, 1.0,
  "Carlsson et al. (81): Sweden", 2.5, 1.5,
  "Birkelund et al. (76): Norway", 2.4, 4.0,
  "Berson (75): France", 2.0, 0.6,
  "Pedulla (96): United States, field experiment", 2.0, 1.3,
  "Ramos et al. (99): Spain and the Netherlands", 1.9, 0.9,
  "Birkelund et al. (76): United States", 1.9, 4.1,
  "Zhou et al. (108): China", 1.9, 0.3,
  "Saeed et al. (104): Pakistan", 1.8, 1.9,
  "Carlsson and Eriksson (80): Sweden", 1.8, 0.7,
  "Oreopoulos (94): Canada", 1.2, 0.3,
  "Maurer-Fazio and Lei (92): China", 1.1, 0.4,
  "Ahmed at al. (66): Sweden", 1.1, 1.4,
  "Correll et al. (83): United States, field experiment", 0.9, 1.1,
  "Nunley et al. (93): United States", 0.9, 0.7,
  "Ruffle and Shtudiner (103): Israel", 0.8, 1.2,
  "Banerjee et al. (73): India", 0.4, 1.6,
  "Patacchini et al. (95): Italy", 0.4, 1.3,
  "Galarza and Yamada (88): Peru", 0.3, 1.0,
  "Yavorsky (107): United States", 0.3, 0.8,
  "Jackson (91): United Kingdom", 0.0, 0.3,
  "Dahl and Krog (84): Denmark", -0.1, 3.8,
  "Bursell (77): Sweden", -0.1, 1.5,
  "Thomas (4): United States, field experiment", -0.2, 1.4,
  "Baert et al. (71): Belgium", -1.0, 1.3,
  "Asali et al. (70): Georgia", -1.6, 1.3,
  "Horváth (16): China", -1.7, 0.8,
  "Petit (97): France", -1.7, 2.1,
  "Quadlin (98): United States, field experiment", -2.1, 1.5,
  "Galarza and Yamada (87): Peru", -2.1, 0.9,
  "Riach and Rich (8): Australia", -2.4, 1.2,
  "Bygren et al. (78): Sweden", -2.5, 2.1,
  "Baert et al. (72): Belgium", -2.5, 3.1,
  "Capéau et al. (82): Belgium", -2.7, 2.6,
  "Gonzalez et al. (89): Spain", -3.3, 0.7,
  "Busetta et al. (79): Italy", -3.5, 0.3,
  "Wu (106): China", -3.6, 2.7,
  "Erlandsson (86): Sweden", -3.7, 2.5,
  "Rivera and Tilcsik (101): United States, field experiment", -3.8, 3.3,
  "Arai et al. (69): Sweden (equivalent CVs)", -6.3, 4.0,
  "Neumark et al. (9): United States", -12.3, 8.4,
  "Meta-analysis", 1.2, 0.4
)

published_s3_4 <- tribble(
  ~label, ~estimate, ~std_error,
  "Bell and Klein (122): United States", 11.9, 8.6,
  "Carlsson et al. (124): Iceland, Norway, Sweden", 8.0, 3.1,
  "Pedulla (96): United States, survey experiment", 5.5, 2.4,
  "Mari and Luijkx (126): the Netherlands", 5.2, 3.1,
  "Thomas (4): United States, survey experiment", 4.3, 2.2,
  "Bertogg et al. (125): Norway", 2.6, 1.1,
  "Bertogg et al. (125): Bulgaria", 2.4, 1.0,
  "Bertogg et al. (125): Switzerland", 2.2, 0.7,
  "Bertogg et al. (125): Greece", 1.7, 0.7,
  "Blommaert et al. (123): the Netherlands", 0.7, 0.9,
  "Di Stasio (127): Italy", 0.4, 2.5,
  "Di Stasio and Werfhorst (128): England and the Netherlands", -0.1, 3.0,
  "Meta-analysis", 2.1, 0.4
)

published_s5_62 <- tribble(
  ~label, ~estimate, ~std_error,
  "Albert et al. (67): Spain", 139.8, 43.9,
  "Birkelund et al. (76): Spain", 49.9, 23.7,
  "Mavlikeeva and Asanov (11): Russia", 32.9, 4.9,
  "Ahmed at al. (14): Sweden", 29.0, 7.6,
  "Birkelund et al. (76): the Netherlands", 27.0, 19.8,
  "Patacchini et al. (95): Italy", 15.8, 12.6,
  "Maurer-Fazio and Lei (92): China", 15.0, 2.3,
  "Carlsson and Eriksson (80): Sweden", 13.1, 8.6,
  "Erlandsson (86): Sweden", 12.2, 31.2,
  "Galarza and Yamada (88): Peru", 12.2, 28.7,
  "Riach and Rich (100): England", 12.2, 15.6,
  "Ramos et al. (99): Spain and the Netherlands", 10.9, 8.4,
  "Galarza and Yamada (87): Peru", 9.8, 11.5,
  "Di Stasio and Larsen (105): United Kingdom, Germany and Norway", 9.1, 4.2,
  "Arai et al. (69): Sweden (equivalent CVs)", 8.1, 17.0,
  "Nunley et al. (93): United States", 8.1, 15.3,
  "Edo et al. (85): France", 7.4, 5.9,
  "Gonzalez et al. (89): Spain", 7.3, 3.3,
  "Arai et al. (69): Sweden (enhanced CVs)", 7.2, 22.3,
  "Asali et al. (70): Georgia", 7.0, 4.9,
  "Birkelund et al. (76): United States", 6.8, 19.2,
  "Pedulla (96): United States, field experiment", 5.5, 13.1,
  "Carlsson (13): Sweden", 4.1, 8.6,
  "Bygren et al. (78): Sweden", 3.8, 16.2,
  "Riach and Rich (8): Australia", 3.6, 9.8,
  "Zhou et al. (108): China", 2.3, 6.5,
  "Ahmed at al. (66): Sweden", 1.0, 6.7,
  "Oreopoulos (94): Canada", 0.3, 1.8,
  "Birkelund et al. (76): Germany", -0.5, 27.8,
  "Carlsson et al. (81): Sweden", -3.5, 10.5,
  "Ruffle and Shtudiner (103): Israel", -5.3, 13.5,
  "Dahl and Krog (84): Denmark", -6.1, 20.9,
  "Thomas (4): United States, field experiment", -7.3, 12.8,
  "Birkelund et al. (76): United Kingdom", -7.5, 35.1,
  "Birkelund et al. (76): Norway", -11.5, 18.8,
  "Booth and Leigh (15): Australia", -26.4, 61.1,
  "Rooth (102): Sweden", -26.6, 25.3,
  "Meta-analysis", 9.0, 2.1
)

forest_gt <- function(published, rewrite, figure, stem) {
  rewrite <- rewrite |>
    transmute(label = str_squish(study_name_citation),
              rewrite_estimate = estimate,
              rewrite_se = std.error)
  published |>
    mutate(label = str_squish(label)) |>
    left_join(rewrite, by = "label") |>
    pivot_longer(c(estimate, std_error, rewrite_estimate, rewrite_se),
                 names_to = "column", values_to = "value") |>
    mutate(
      part = if_else(column %in% c("estimate", "rewrite_estimate"), "estimate", "se"),
      side = if_else(column %in% c("estimate", "std_error"), "paper", "rewrite")
    ) |>
    select(label, part, side, value) |>
    pivot_wider(names_from = side, values_from = value) |>
    transmute(
      claim_id = str_glue("{claim_slug(figure)}_{if_else(part == 'estimate', 'est', 'se')}_",
                          "{claim_slug(label)}"),
      table_figure = figure,
      claim = if_else(part == "estimate",
                      str_glue("{stem}: {label}"),
                      str_glue("{stem} SE: {label}")),
      value_paper = paper,
      digits = 1,
      value_rewrite = rewrite,
      notes = str_glue("Printed beside the point in {str_replace(figure, 'fig_', 'fig. ')}")
    )
}

# Published values: the study by study panels ----
# Figs. S4.5 to S4.61 are 57 separate published figures, one per study. Each labels every
# occupation with an "estimate (SE)" pair, the share of women in the occupation, and a sample
# size, and adds the study's own gender gradient where it has one. Rather than one ground
# truth row per number, which would be 931 of them, each figure contributes three rows
# recording how many of its published labels the rewrite reproduces. The published counts and
# the published label text are typed from the supplementary PDF.
published_panels <- tribble(
  ~figure, ~label, ~entries, ~compositions, ~sample_sizes,
  "S4.5", "Ahmed at al. (14): Sweden",
    "13.00 (6.80); 14.81 (13.88); 11.11 (6.02); 27.06 (11.29); 15.68 (3.75); 1.86 (6.89); -0.14 (7.77); -0.47 (4.41); 12.22 (14.75); 10.14 (6.44); 3.35 (4.22); -8.79 (6.54); 2.58 (7.99); -0.61 (5.19); -10.96 (6.27); 28.96 (7.57)",
    "78; 74; 72; 72; 68; 59; 59; 57; 55; 55; 49; 27; 23; 8; 6",
    "206; 51; 270; 71; 434; 127; 152; 392; 43; 166; 497; 141; 153; 337; 214",
  "S4.6", "Ahmed at al. (66): Sweden",
    "5.55 (7.26); -1.27 (3.87); 0.44 (5.88); 2.10 (3.00); -3.52 (2.66); -0.08 (4.16); 10.31 (3.78); 1.36 (4.67); 0.44 (5.01); -1.96 (4.35); 1.05 (6.69)",
    "77; 73; 73; 68; 60; 60; 50; 29; 8; 6",
    "186; 667; 289; 480; 500; 488; 542; 362; 209; 267",
  "S4.7", "Albert et al. (67): Spain",
    "28.77 (3.16); -1.36 (3.07); 7.80 (2.89); 139.82 (43.88)",
    "72; 53; 53",
    "730; 1030; 1000",
  "S4.8", "Alden et al. (68): Sweden",
    "6.75 (2.96); 2.73 (3.27)",
    "56; 23",
    "700; 650",
  "S4.9", "Arai et al. (69): Sweden (equivalent CVs)",
    "-8.33 (8.77); 1.21 (9.10); -0.47 (8.29); -6.67 (9.56); -11.62 (13.13); 8.12 (16.99)",
    "77; 73; 53; 22; 8",
    "112; 104; 114; 120; 116",
  "S4.10", "Arai et al. (69): Sweden (enhanced CVs)",
    "2.08 (7.49); -17.00 (12.72); 5.19 (6.99); 11.11 (9.50); -16.12 (6.50); 7.16 (22.27)",
    "77; 73; 53; 22; 8",
    "96; 64; 162; 110; 152",
  "S4.11", "Asali et al. (70): Georgia",
    "0.00 (2.25); -0.47 (2.45); -2.86 (3.53); 2.13 (4.02); 0.00 (3.29); -7.35 (3.19); 8.33 (5.10); -4.62 (3.05); -12.00 (6.00); 6.97 (4.92)",
    "82; 60; 58; 54; 41; 37; 24; 19; 13",
    "128; 844; 140; 188; 124; 272; 144; 260; 100",
  "S4.12", "Mavlikeeva and Asanov (11): Russia",
    "5.69 (1.48); 9.87 (1.56); 0.79 (0.72); 1.96 (1.71); 1.05 (1.05); -3.36 (1.41); 32.85 (4.90)",
    "53; 53; 34; 34; 33; 19",
    "1276; 1500; 1444; 1244; 1372; 1492",
  "S4.13", "Baert et al. (71): Belgium",
    "-1.04 (1.30)", "47", "1152",
  "S4.14", "Baert et al. (72): Belgium",
    "-2.55 (3.08)", "39", "864",
  "S4.15", "Banerjee et al. (73): India",
    "1.44 (3.81); -0.18 (1.50)", "67; 20", "400; 924",
  "S4.16", "Becker et al. (74): Austria, Germany, Switzerland",
    "9.34 (1.04); 6.86 (1.48)", "86; 41", "3679; 3011",
  "S4.17", "Berson (75): France",
    "2.00 (0.63)", "66", "5000",
  "S4.18", "Birkelund et al. (76): Germany",
    "12.64 (7.52); 22.90 (8.88); -5.27 (9.27); 21.41 (8.15); -10.00 (8.23); 14.24 (8.19); -0.47 (27.75)",
    "85; 69; 69; 64; 56; 14",
    "123; 113; 121; 127; 121; 112",
  "S4.19", "Birkelund et al. (76): the Netherlands",
    "9.06 (6.76); 18.58 (9.12); 24.12 (7.86); 9.71 (8.11); -3.71 (5.32); -5.15 (6.47); 26.97 (19.81)",
    "83; 70; 66; 66; 62; 13",
    "186; 112; 133; 151; 246; 154",
  "S4.20", "Birkelund et al. (76): Norway",
    "-7.03 (9.70); -26.26 (21.33); 11.94 (8.12); 6.87 (6.87); 0.81 (10.94); 5.18 (9.53); -11.50 (18.83)",
    "85; 66; 63; 63; 50; 16",
    "89; 20; 74; 175; 77; 112",
  "S4.21", "Birkelund et al. (76): Spain",
    "18.27 (6.52); 11.97 (6.17); 7.58 (5.21); -6.81 (11.62); 1.04 (4.37); -4.97 (14.28); 49.91 (23.74)",
    "73; 70; 66; 66; 53; 21",
    "127; 167; 181; 69; 364; 51",
  "S4.22", "Birkelund et al. (76): United Kingdom",
    "-7.10 (7.09); 22.95 (5.23); 3.91 (9.58); -15.19 (8.31); 3.22 (6.79); 8.31 (9.01); -7.52 (35.09)",
    "71; 69; 68; 62; 62; 17",
    "104; 208; 110; 112; 138; 114",
  "S4.23", "Birkelund et al. (76): United States",
    "1.98 (7.64); 5.84 (8.58); -9.05 (11.51); 7.75 (9.47); -9.63 (10.89); -1.33 (10.78); 6.83 (19.24)",
    "90; 69; 61; 55; 55; 24",
    "89; 84; 77; 94; 76; 82",
  "S4.24", "Booth and Leigh (15): Australia",
    "3.17 (3.11); 9.98 (3.23); 14.03 (2.97); 0.96 (3.05); -26.41 (61.08)",
    "67; 58; 54; 53",
    "832; 863; 851; 819",
  "S4.25", "Bursell (77): Sweden",
    "-0.07 (1.51)", "50", "3636",
  "S4.26", "Busetta et al. (79): Italy",
    "-3.46 (0.34)", "39", "21998",
  "S4.27", "Bygren et al. (78): Sweden",
    "-24.00 (9.87); 10.66 (8.53); -6.16 (7.53); -11.72 (6.62); 10.70 (6.18); 8.71 (4.14); -3.10 (4.76); -6.31 (7.50); -17.31 (6.11); 3.22 (6.93); -6.63 (6.48); 3.63 (6.52); -10.29 (9.21); 3.77 (16.24)",
    "77; 77; 75; 73; 73; 71; 61; 61; 57; 52; 49; 22; 16",
    "94; 111; 121; 204; 230; 168; 138; 171; 233; 200; 140; 230; 104",
  "S4.28", "Carlsson (13): Sweden",
    "-4.00 (2.66); -3.12 (6.30); 5.98 (3.55); 0.00 (6.30); -2.38 (7.24); 3.23 (3.23); -0.00 (2.75); 6.83 (2.88); 8.06 (2.84); 11.43 (3.22); -9.38 (5.78); 0.94 (4.14); -3.85 (4.99); 4.06 (8.58)",
    "77; 73; 73; 73; 73; 68; 60; 60; 53; 50; 29; 22; 8",
    "600; 256; 736; 240; 168; 248; 800; 1112; 744; 560; 256; 424; 312",
  "S4.29", "Carlsson and Eriksson (80): Sweden",
    "5.53 (1.33); 9.50 (1.94); 0.23 (2.44); 1.41 (1.31); -5.83 (2.25); 1.33 (1.27); -3.03 (3.44); 13.08 (8.59)",
    "83; 68; 61; 61; 58; 51; 8",
    "1110; 789; 558; 918; 1059; 1137; 495",
  "S4.30", "Carlsson et al. (81): Sweden",
    "-4.22 (5.38); 3.03 (1.63); 2.74 (1.81); -3.53 (10.55)",
    "66; 65; 20",
    "299; 2664; 2699",
  "S4.31", "Correll et al. (83): United States, field experiment",
    "0.91 (1.15)", "49", "1276",
  "S4.32", "Dahl and Krog (84): Denmark",
    "4.17 (12.28); -18.75 (19.55); 32.14 (23.54); 14.02 (12.71); -2.08 (12.16); -13.69 (16.18); -15.87 (12.84); -25.00 (13.35); 0.32 (9.59); -13.64 (17.73); 40.00 (25.17); 12.50 (11.98); 14.29 (21.36); -8.33 (17.61); 25.00 (30.93); 3.43 (8.75); 20.83 (27.88); 1.79 (16.10); 49.21 (15.43); -50.00 (15.46); 12.50 (47.09); -6.05 (20.87)",
    "83; 81; 81; 76; 68; 66; 66; 59; 59; 45; 45; 45; 42; 42; 37; 33; 29; 17; 5; 3; 3",
    "40; 32; 26; 46; 34; 52; 58; 24; 124; 44; 12; 16; 44; 38; 16; 58; 14; 30; 50; 18; 12",
  "S4.33", "Edo et al. (85): France",
    "8.62 (2.42); 4.65 (1.81); 2.41 (1.62); 7.36 (5.90)",
    "93; 93; 40",
    "4176; 7488; 6480",
  "S4.34", "Erlandsson (86): Sweden",
    "-0.57 (3.26); -10.96 (5.87); -5.72 (4.91); 12.20 (31.21)",
    "68; 61; 39",
    "944; 286; 413",
  "S4.35", "Galarza and Yamada (87): Peru",
    "-1.85 (1.59); 0.24 (1.39); -4.92 (1.91); 9.76 (11.53)",
    "54; 40; 19",
    "1620; 1656; 1544",
  "S4.36", "Galarza and Yamada (88): Peru",
    "6.68 (1.76); -4.13 (1.35); 1.32 (1.88); 12.16 (28.74)",
    "54; 40; 19",
    "808; 1500; 1520",
  "S4.37", "Gonzalez et al. (89): Spain",
    "0.00 (1.41); -2.25 (2.90); -0.68 (2.46); 0.60 (1.59); -4.63 (2.03); -3.13 (2.46); -1.73 (2.30); -11.11 (2.74); -3.74 (2.63); -1.39 (4.08); -1.95 (1.72); -6.25 (4.60); -5.17 (4.03); -6.95 (2.91); -1.44 (2.41); -3.57 (1.55); -7.35 (3.75); 7.32 (3.31)",
    "70; 70; 69; 66; 56; 55; 53; 48; 45; 45; 45; 35; 33; 30; 21; 18; 1",
    "198; 376; 296; 336; 432; 256; 462; 432; 428; 432; 308; 96; 232; 374; 416; 280; 136",
  "S4.38", "Hipp (90): Germany",
    "5.20 (3.25)", "57", "820",
  "S4.39", "Horváth (16): China",
    "2.17 (0.95); -5.51 (1.26)", "50; 34", "3203; 3201",
  "S4.40", "Jackson (91): United Kingdom",
    "0.04 (0.26)", "44", "5120",
  "S4.41", "Maurer-Fazio and Lei (92): China",
    "4.27 (0.68); 1.62 (0.92); 1.69 (0.71); -3.21 (0.93); 15.02 (2.32)",
    "70; 53; 50; 20",
    "6048; 6048; 6048; 6048",
  "S4.42", "Neumark et al. (9): United States",
    "-12.31 (8.38)", "62", "130",
  "S4.43", "Nunley et al. (93): United States",
    "1.68 (1.55); -0.15 (1.65); 1.72 (2.12); -1.29 (1.37); 1.47 (1.13); 1.51 (1.62); 8.05 (15.31)",
    "55; 50; 50; 44; 44; 44",
    "2326; 1692; 1152; 1661; 1636; 929",
  "S4.44", "Oreopoulos (94): Canada",
    "-1.89 (1.87); -1.10 (1.11); 1.22 (0.87); 3.06 (6.02); -3.12 (2.20); -0.69 (0.70); 3.03 (3.18); 0.06 (1.37); 3.35 (1.44); 1.48 (0.80); 6.00 (4.21); 0.58 (0.58); 1.83 (0.74); 1.20 (0.78); -3.23 (3.24); -0.31 (1.06); 1.88 (1.18); -1.49 (1.48); 2.13 (2.11); 0.04 (0.89); -1.25 (1.86); 0.29 (1.79)",
    "70; 70; 70; 69; 58; 54; 53; 53; 53; 50; 50; 50; 50; 50; 46; 20; 20; 20; 20; 16; 16",
    "103; 899; 1816; 76; 136; 299; 64; 632; 1391; 1985; 225; 340; 1883; 407; 72; 493; 1172; 126; 177; 323; 291",
  "S4.45", "Patacchini et al. (95): Italy",
    "3.11 (2.35); 3.76 (2.82); 2.93 (3.72); -9.07 (4.87); -0.36 (1.75); 3.76 (3.26); -0.04 (2.83); 15.81 (12.63)",
    "74; 74; 64; 64; 58; 58; 52",
    "322; 303; 288; 368; 373; 359; 307",
  "S4.46", "Pedulla (96): United States, field experiment",
    "4.97 (3.21); 0.25 (2.98); 8.11 (2.68); -0.15 (1.54); 5.51 (13.11)",
    "92; 55; 49; 45",
    "416; 804; 372; 828",
  "S4.47", "Petit (97): France",
    "-1.70 (2.15)", "45", "942",
  "S4.48", "Quadlin (98): United States, field experiment",
    "-2.09 (1.46)", "51", "2106",
  "S4.49", "Ramos et al. (99): Spain and the Netherlands",
    "5.70 (2.32); 4.80 (2.02); 5.94 (1.82); 0.09 (2.98); -0.48 (2.70); -1.76 (1.87); 1.90 (3.37); 10.88 (8.39)",
    "73; 70; 66; 66; 53; 53; 21",
    "1003; 1546; 1328; 889; 1056; 2527; 882",
  "S4.50", "Riach and Rich (8): Australia",
    "0.58 (3.14); -3.79 (2.50); 2.13 (3.71); -6.76 (2.99); 3.48 (3.70); -2.02 (3.80); -7.24 (3.12); 3.63 (9.76)",
    "70; 50; 34; 34; 20; 20; 20",
    "344; 422; 188; 296; 230; 198; 304",
  "S4.51", "Riach and Rich (100): England",
    "9.52 (2.60); 3.24 (1.69); -5.20 (2.99); 9.23 (3.36); 12.15 (15.62)",
    "73; 44; 19; 16",
    "462; 678; 346; 260",
  "S4.52", "Rivera and Tilcsik (101): United States, field experiment",
    "-3.80 (3.29)", "53", "316",
  "S4.53", "Rooth (102): Sweden",
    "-12.50 (8.59); -3.50 (6.79); -4.59 (5.20); -7.65 (6.34); 16.40 (5.84); 8.81 (5.61); -0.71 (8.99); -26.59 (25.32)",
    "77; 73; 60; 60; 53; 50; 22",
    "248; 368; 288; 386; 206; 328; 146",
  "S4.54", "Ruffle and Shtudiner (103): Israel",
    "-1.89 (3.21); 4.59 (2.69); -6.58 (3.44); 2.78 (3.73); -0.89 (2.74); 8.73 (2.77); 0.93 (2.70); 2.24 (3.33); -7.14 (5.90); 2.27 (7.46); -5.34 (13.48)",
    "73; 60; 60; 46; 46; 46; 45; 45; 30; 24",
    "952; 774; 778; 144; 448; 848; 432; 536; 224; 176",
  "S4.55", "Saeed et al. (104): Pakistan",
    "1.81 (1.87)", "4", "1216",
  "S4.56", "Di Stasio and Larsen (105): United Kingdom, Germany and Norway",
    "5.36 (1.60); 3.66 (2.98); 2.61 (4.96); 4.08 (2.81); 4.77 (2.28); 4.44 (2.01); 2.26 (2.70); -9.93 (6.79); -3.75 (8.71); -2.82 (6.28); 9.07 (4.22)",
    "73; 71; 67; 67; 62; 62; 16; 2; 1; 1",
    "1817; 1030; 398; 1229; 1264; 1756; 1373; 199; 136; 223",
  "S4.57", "Thomas (4): United States, field experiment",
    "1.30 (2.12); -4.62 (1.89); 2.67 (3.15); -0.22 (2.64); -7.35 (12.82)",
    "90; 90; 68; 55",
    "262; 528; 532; 774",
  "S4.58", "Wu (106): China",
    "-3.57 (2.69)", "51", "1344",
  "S4.59", "Yavorsky (107): United States",
    "0.29 (0.78)", "50", "6302",
  "S4.60", "Zhou et al. (108): China",
    "3.59 (0.54); 3.03 (0.59); -0.56 (0.37); 2.22 (0.91); 2.28 (6.53)",
    "70; 50; 50; 20",
    "5234; 4560; 6456; 2880",
  "S4.61", "Capéau et al. (82): Belgium",
    "-2.73 (2.64)", "53", "1607"
)

split_labels <- function(x) str_split(x, "; ")

# The rewrite writes one row per plotted point with the same "estimate (SE)" label the
# published panel prints, so agreement is a set comparison rather than a numeric one. The
# gender composition labels are compared the same way. The sample size labels have no
# counterpart in the deposit and are compared against the residual degrees of freedom plus one,
# which is the only sample size the deposited data carry.
panel_rewrite <- panels |>
  mutate(label = str_squish(study_name_citation)) |>
  group_by(label) |>
  summarize(
    rewrite_entries = list(str_squish(entry)),
    rewrite_compositions = list(as.character(round(fraction_female[!is.na(fraction_female)] * 100))),
    rewrite_sample_sizes = list(as.character(n_from_df[!is.na(n_from_df)]))
  )

# Both sides are multisets: a panel can print the same label twice and should then be credited
# twice only if the rewrite produces it twice.
count_matched <- function(published_string, rewrite_values) {
  pub <- table(split_labels(published_string)[[1]])
  rew <- table(rewrite_values)
  common <- intersect(names(pub), names(rew))
  sum(pmin(pub[common], rew[common]))
}

panel_gt <- published_panels |>
  mutate(label = str_squish(label)) |>
  left_join(panel_rewrite, by = "label") |>
  transmute(
    figure,
    label,
    n_entries = lengths(split_labels(entries)),
    n_compositions = lengths(split_labels(compositions)),
    n_sample_sizes = lengths(split_labels(sample_sizes)),
    matched_entries = map2_dbl(entries, rewrite_entries, count_matched),
    matched_compositions = map2_dbl(compositions, rewrite_compositions, count_matched),
    matched_sample_sizes = map2_dbl(sample_sizes, rewrite_sample_sizes, count_matched)
  ) |>
  pivot_longer(
    c(n_entries, n_compositions, n_sample_sizes,
      matched_entries, matched_compositions, matched_sample_sizes),
    names_to = "column", values_to = "value"
  ) |>
  mutate(
    part = str_remove(column, "^(n|matched)_"),
    side = if_else(str_starts(column, "n_"), "paper", "rewrite")
  ) |>
  select(figure, label, part, side, value) |>
  pivot_wider(names_from = side, values_from = value) |>
  transmute(
    claim_id = str_glue("{claim_slug(figure)}_{part}"),
    table_figure = str_glue("fig_{figure}"),
    claim = case_when(
      part == "entries" ~ str_glue("Plotted estimate and standard error labels, {label}"),
      part == "compositions" ~ str_glue("Occupation gender composition labels, {label}"),
      .default = str_glue("Occupation sample size labels, {label}")
    ),
    value_paper = paper,
    digits = 0,
    value_rewrite = rewrite,
    notes = if_else(
      part == "sample_sizes",
      "Count of the sample sizes printed on the published panel that equal the deposited residual degrees of freedom plus one",
      "Count of the labels printed on the published panel that the rewrite reproduces"
    )
  )

# Coverage of the study by study panels ----
# The per-panel rows above have no archive counterpart, because the deposited loop errors on its
# first study and draws nothing. Recording that as two rows keyed to what archive_values.R
# harvests keeps the gap visible instead of leaving 171 silently unverifiable rows.
panel_coverage <- tibble(
  claim_id = c("s4_panels_published", "s4_panel_entries_published"),
  table_figure = "figs_S4.5_to_S4.61",
  claim = c("Study panels drawn by the deposited archive",
            "Plotted estimate and standard error labels produced by the deposited archive"),
  value_paper = c(nrow(published_panels), sum(lengths(split_labels(published_panels$entries)))),
  digits = 0,
  value_rewrite = c(n_distinct(panels$study_name_citation), nrow(panels)),
  notes = "The published appendix draws one panel per study; the deposited loop draws none"
)

# Assemble ----
gt <- bind_rows(
  text_claims,
  table_rows(published_table_1, table_1, "table_1"),
  gof_rows(published_table_1, table_1, "table_1"),
  table_rows(published_table_2, table_2, "table_2"),
  gof_rows(published_table_2, table_2, "table_2"),
  manifest_gt,
  forest_gt(published_s1_1, forest_s1_1, "fig_S1.1", "ATE"),
  forest_gt(published_s3_4, forest_s3_4, "fig_S3.4", "ATE"),
  forest_gt(published_s5_62, forest_s5_62, "fig_S5.62", "Gender gradient"),
  panel_gt,
  panel_coverage
) |>
  mutate(claim_id = as.character(claim_id),
         table_figure = as.character(table_figure),
         claim = as.character(claim),
         mode = replace_na(mode, "value"),
         # A row in approximate mode carries two precisions: the page's, at which value_paper is
         # written out, and the fuller one the computed value needs. Rendering "over the past
         # four decades" as 4.0 would be the article's own hedge restated to a precision it
         # never claimed.
         paper_digits = coalesce(paper_digits, digits))

gt <- gt |>
  left_join(archive, by = c("table_figure", "claim"))

# Verdicts ----
# Agreement is tested at the precision the article prints, which is why every row carries the
# number of decimals rather than having it inferred from a double. A row in "holds" mode has no
# printed value to agree with and asks whether the statement is true, so the rewrite side is
# already 1 or 0. A row in "approximate" mode returns no verdict at all: the article named a
# rough magnitude, and a string comparison there would file an honest hedge as a defect.
# The epsilon is not decoration. Half a unit in the last printed digit is not representable in
# binary, so a value that agrees exactly at the page's precision can miss by a few parts in
# 10^16: abs(-0.375 - -0.37) evaluates to 0.005000000000000004 against a tolerance of 0.005.
# Without it the comparison rejects a truncation the moment one appears.
agrees <- function(value, target, digits, mode) {
  case_when(
    mode == "approximate" ~ NA_real_,
    is.na(value) | is.na(target) ~ NA_real_,
    abs(value - target) <= 0.5 * 10^(-digits) + 1e-9 ~ 1,
    .default = 0
  )
}

gt <- gt |>
  mutate(
    paper_id = paper_id,
    match = agrees(value_script, value_paper, digits, mode),
    match_rewrite = agrees(value_rewrite, value_paper, digits, mode)
  )

# defect_locus, set on every row carrying an adverse verdict ----
# A zero here reads as a failure of the rewrite unless the fault is named, and it almost never
# is the rewrite. Each case below was established by inspection, not inferred. The clause fires
# on either verdict: a row where the archive fails and the rewrite succeeds is the signature of
# a defect too, and leaving it unlabelled is how environment drift goes unrecorded.
gt <- gt |>
  mutate(
    defect_locus = case_when(
      !is.na(locus) & (replace_na(match_rewrite, 0) == 0 | replace_na(match, 1) == 0) ~ locus,
      (is.na(match_rewrite) | match_rewrite == 1) & (is.na(match) | match == 1) ~ NA_character_,
      claim == "Difference in magnitude, 100 vs 0 percent women (pp)" ~ "paper_internal",
      table_figure == "table_2_model2" &
        str_starts(claim, "Intercept significance stars") ~ "paper_internal",
      claim == "Occupation counts reproduced" ~ "paper_internal",
      claim == "Subject counts reproduced" ~ "archive",
      str_starts(claim, "Occupation sample size labels") ~ "archive",
      # The archive alone fails on these three, and in both cases the deposit is the reason: it
      # ships the bootstrap at five resamples, and its panel loop errors on the first study and
      # draws nothing. Left unlabelled they would read as unexplained, which is the shape a
      # finding takes when nothing names its cause.
      claim == "Bootstrapped SE of the difference in magnitude" ~ "archive",
      table_figure == "figs_S4.5_to_S4.61" ~ "archive",
      .default = "unresolved"
    ),
    notes = case_when(
      claim == "Difference in magnitude, 100 vs 0 percent women (pp)" ~
        "The article prints 2.10 where the deposited code and the rewrite both give 2.10547, which rounds to 2.11. The published figure is the truncation, not a different number",
      table_figure == "table_2_model2" & str_starts(claim, "Intercept significance stars") ~
        "Published Table 2 prints the Model 2 intercept as -3.72 with no marker. The deposited code's own table output prints -3.72** at p = 0.001, and the numerically identical -3.73 in Model 3 carries its two stars. A dropped marker in the typeset table",
      claim == "Occupation counts reproduced" ~
        "Table S2.1 gives 18 occupations for Gonzalez et al. (89) and 24 for Dahl and Krog (84); figs. S4.37 and S4.32 plot 17 and 21, which is also what the deposited data hold. The article disagrees with itself in two rows",
      claim == "Subject counts reproduced" ~
        "The deposit carries no subject count. Summing residual degrees of freedom plus one over a study's occupations recovers the published total where the CATE regressions were fit on subjects; where the study had several applications per job opening the published total is a multiple of it, and the deposit gives no way to recover the multiplier",
      str_starts(claim, "Occupation sample size labels") & match_rewrite == 0 ~
        "The published panel prints a sample size the deposit does not carry. Residual degrees of freedom plus one recovers it for the studies whose CATE regressions were fit on subjects and not for the rest",
      claim == "Bootstrapped SE of the difference in magnitude" & match == 0 ~
        "The deposit ships analyze_cates.R with sims <- 5 and the published 500 commented out one line above, so the harvest of the deposited code gives 0.54 where the article and the rewrite give 0.58",
      .default = notes
    )
  )

# value_paper is written out as the string the article prints ----
# Stored as a double it comes back as 1.5 where the page prints 1.50, and the printed precision
# is the only typographic fact any of the comparisons above depend on. The comparison is done
# numerically and the string is rendered afterwards, at the digit count each block recorded.
gt <- gt |>
  mutate(
    value_paper_number = value_paper,
    value_paper = if_else(is.na(value_paper), NA_character_,
                          sprintf(paste0("%.", paper_digits, "f"), value_paper))
  )

gt <- gt |>
  select(paper_id, claim_id, table_figure, claim, value_script, value_paper, match,
         value_rewrite, match_rewrite, defect_locus, notes,
         value_paper_number, digits, mode) |>
  arrange(factor(table_figure, levels = unique(table_figure)), .locale = "en")

# Gate: a published value with no rewrite counterpart ----
# A join that silently found nothing would otherwise be indistinguishable from a quantity the
# pipeline does not produce.
orphans <- gt |> filter(!is.na(value_paper), is.na(value_rewrite))

if (nrow(orphans) > 0) {
  print(orphans |> select(table_figure, claim), n = 50)
  stop(str_glue("{nrow(orphans)} published values have no rewrite counterpart. ",
                "Either a label failed to join or an output is missing."))
}

# Gate: an adverse verdict on either side needs a locus ----
# Stated on match_rewrite alone it cannot see a row where the archive fails and the rewrite
# succeeds, which is the shape environment drift takes.
adverse <- (!is.na(gt$match_rewrite) & gt$match_rewrite == 0) | (!is.na(gt$match) & gt$match == 0)
clean <- (is.na(gt$match_rewrite) | gt$match_rewrite == 1) & (is.na(gt$match) | gt$match == 1)

if (any(adverse & is.na(gt$defect_locus)) || any(clean & !is.na(gt$defect_locus))) {
  print(gt |> filter(adverse & is.na(defect_locus)) |> select(claim_id, table_figure, claim))
  stop("Every row carrying an adverse verdict needs a defect_locus, and a clean match needs none")
}

# Gate: the extraction and this file must tell the same story about the page ----
# ground_truth/published_claims.csv is a second transcription of the same published pages, made
# by reading the supplement's text layer rather than by typing. Two hand transcriptions are free
# to drift and nothing else compares them. value_paper is forced to character in every reader:
# a column whose entries all look numeric is guessed as double, which turns the published 1.50
# into 1.5 and destroys the printed precision the comparison depends on.
published_claims <- read_csv(
  here::here("ground_truth", "published_claims.csv"),
  col_types = cols(.default = col_character(), value_paper = col_character())
) |>
  mutate(needs_block = needs_block == "TRUE")

stopifnot(!anyDuplicated(published_claims$claim_id),
          !anyDuplicated(na.omit(gt$claim_id)),
          all(na.omit(gt$claim_id) %in% published_claims$claim_id))

transcription <- gt |>
  drop_na(claim_id) |>
  select(claim_id, gt_value = value_paper, gt_number = value_paper_number) |>
  inner_join(select(published_claims, claim_id, extraction_value = value_paper),
             by = "claim_id") |>
  filter(!is.na(gt_number)) |>
  mutate(
    numbers_agree = abs(gt_number - as.numeric(extraction_value)) < 1e-9,
    strings_agree = gt_value == extraction_value
  )

if (any(!transcription$numbers_agree | !transcription$strings_agree)) {
  print(transcription |> filter(!numbers_agree | !strings_agree), n = 40)
  stop("The extraction and the ground truth disagree about what the published page prints")
}

# Gate: every published float is covered, and by how much of what it prints ----
# The supplement states its own inventory in its front matter, so coverage is checked against
# that list rather than against whatever the pipeline happens to produce. "At least one row" is
# not coverage: each float records the fraction of the numbers it prints that the ground truth
# reaches.
published_floats <- c("Table 1", "Table 2", "Table S2.1", "Figure 1", "Figure 2", "Figure 3",
                      "Figure 4", "Figure 5", "Figure S1.1", "Figure S2.2", "Figure S3.3",
                      "Figure S3.4", str_glue("Figure S4.{5:61}"), "Figure S5.62", "Figure S6.63")

float_of_row <- function(table_figure) {
  case_when(
    str_starts(table_figure, "table_1") ~ "Table 1",
    str_starts(table_figure, "table_2") ~ "Table 2",
    table_figure == "table_S2.1" ~ "Table S2.1",
    str_starts(table_figure, "fig_S4\\.") ~ str_replace(table_figure, "fig_S", "Figure S"),
    str_starts(table_figure, "fig_") ~ str_replace(table_figure, "fig_S", "Figure S"),
    .default = NA_character_
  )
}

# Numbers each float prints on its face, counted from the published page rather than assumed.
# A figure that prints none still asserts something, and what it asserts is recorded in the
# reason beside it.
float_printed <- bind_rows(
  tibble(float = c("Table 1", "Table 2"),
         printed = c(3 * nrow(published_table_1) + 2 * n_distinct(published_table_1$model),
                     3 * nrow(published_table_2) + 2 * n_distinct(published_table_2$model))),
  tibble(float = "Table S2.1", printed = 3 * nrow(published_s2_1)),
  tibble(float = "Figure S1.1", printed = 2 * nrow(published_s1_1)),
  tibble(float = "Figure S3.4", printed = 2 * nrow(published_s3_4)),
  tibble(float = "Figure S5.62", printed = 2 * nrow(published_s5_62)),
  published_panels |>
    transmute(float = str_glue("Figure {figure}"),
              printed = lengths(split_labels(entries)) + lengths(split_labels(compositions)) +
                lengths(split_labels(sample_sizes)))
)

uncovered_reasons <- tribble(
  ~float, ~reason,
  "Figure 1", "A two-panel diagram of the two gender gradients the theory admits. It plots no estimates and the deposit has no code for it",
  "Figure 2", "The same diagram split by advantaged and disadvantaged group. It plots no estimates and the deposit has no code for it",
  "Figure 3", "A summary of the discrimination mechanisms the 51 papers discuss, coded by hand. The deposit carries no mechanism or discipline variable, so nothing in the pipeline can reach it",
  "Figure 4", "The occupation CATEs against gender composition, with the Table 1 model 1 line. It prints no numbers on its face; its estimates are the 298 CATEs and its line is the Table 1 model 1 row, both covered here",
  "Figure 5", "The same estimates split by majority and minority status, with the Table 2 model 1 and 2 lines. It prints no numbers on its face and both lines are covered here",
  "Figure S2.2", "An evidence map of country by year tiles. It prints no numbers; what it asserts is which country-year cells are occupied, and the section S2 sentences describing it are covered here",
  "Figure S3.3", "The survey and field gradients drawn as two fitted lines. It prints no numbers; both fits are recorded here with no published counterpart",
  "Figure S6.63", "A PRISMA flow diagram of the literature search. Its counts come from the screening process and no data file in the deposit records them"
)

# A row covers one published number, except where a float is summarised rather than recorded
# cell by cell: a panel's three rows and the manifest's three column rows each account for a
# whole column of published labels, and the number they account for is the number the page
# prints. Counting rows instead would report a 57-label panel as three numbers covered.
float_coverage <-
  gt |>
  mutate(
    float = float_of_row(table_figure),
    covers = case_when(
      str_starts(table_figure, "fig_S4\\.") ~ value_paper_number,
      table_figure == "table_S2.1" & claim != "Studies listed" ~ value_paper_number,
      table_figure == "table_S2.1" ~ 0,
      .default = 1
    )
  ) |>
  drop_na(float) |>
  summarize(covered = sum(covers), .by = float) |>
  full_join(float_printed, by = "float") |>
  full_join(tibble(float = published_floats), by = "float") |>
  left_join(uncovered_reasons, by = "float") |>
  mutate(covered = replace_na(covered, 0),
         share = if_else(is.na(printed) | printed == 0, NA_real_, covered / printed))

# Written out so the report can state the fraction rather than assert that a float has a row.
# "At least one row" is not coverage: one row against a hundred published cells satisfies every
# check that asks only whether a float appears, which is the failure this file exists to make
# visible. Only properties of the published floats and of the ground truth go in it.
float_coverage |>
  select(float, printed, covered, share, reason) |>
  arrange(factor(float, levels = published_floats), .locale = "en") |>
  write_csv(here::here("ground_truth", "float_coverage.csv"))

unlisted <- setdiff(float_coverage$float, published_floats)
uncovered <- float_coverage |> filter(covered == 0, is.na(reason))
partial <- float_coverage |> filter(!is.na(share), share < 1)

if (length(unlisted) > 0 || nrow(uncovered) > 0 || nrow(partial) > 0) {
  print(float_coverage |> filter(covered == 0 | (!is.na(share) & share < 1)), n = 80)
  stop(str_glue(
    "Float coverage gate failed. Floats the supplement does not list: ",
    "{str_c(unlisted, collapse = ', ')}. Floats with no row and no reason: ",
    "{str_c(uncovered$float, collapse = ', ')}. Floats covered in part: ",
    "{str_c(partial$float, collapse = ', ')}."
  ))
}

# Gate: the second instrument ran, printed, and agrees ----
# maintained/in_text_claims.R reaches the same claimed numbers out of the same outputs by its
# own path, doing its own selection, unit conversion and rounding. It is run here rather than
# read: a block that errors on its first line, or that ends in a bare expression and so prints
# nothing under source(), satisfies a scan for markers while checking nothing at all.
#
# Sourced into its own environment. Both files necessarily read the same outputs and name their
# objects for what they hold, so a bare source() would replace this script's own
# `published_claims` and `out()` with the claims file's, and every gate below would then be
# running against the wrong objects.
claims_output <- capture.output(
  source(here::here("maintained", "in_text_claims.R"), local = new.env())
)

printed_claims <- tibble(line = claims_output) |>
  filter(str_starts(line, "CLAIM ")) |>
  transmute(claim_id = str_match(line, "^CLAIM ([^ ]+) = ")[, 2],
            value_printed = str_match(line, "^CLAIM [^ ]+ = (.*?) \\|\\| ")[, 2])

stopifnot(!anyDuplicated(printed_claims$claim_id),
          !any(is.na(printed_claims$claim_id)),
          !any(is.na(printed_claims$value_printed)))

declared <- published_claims$claim_id[published_claims$needs_block]
must_have_row <- published_claims$claim_id[published_claims$claim_type %in%
                                             c("pipeline", "descriptive")]

blockless <- setdiff(declared, printed_claims$claim_id)
invented <- setdiff(printed_claims$claim_id, declared)
rowless <- setdiff(must_have_row, na.omit(gt$claim_id))

if (length(blockless) > 0 || length(invented) > 0 || length(rowless) > 0) {
  stop(str_glue(
    "Coverage gate failed. Claims with no block in maintained/in_text_claims.R ",
    "({length(blockless)}): {str_c(head(blockless, 40), collapse = ', ')}. ",
    "Blocks naming a claim the extraction does not declare ({length(invented)}): ",
    "{str_c(head(invented, 40), collapse = ', ')}. ",
    "Claims with no ground truth row ({length(rowless)}): ",
    "{str_c(head(rowless, 40), collapse = ', ')}."
  ))
}

stopifnot(nrow(printed_claims) == length(declared))

# The two instruments must land on the same number. in_text_claims.R prints in the article's
# units and rounding and never reads this file, so a disagreement is one of the two being wrong.
#
# The comparison covers every claim except the ones whose value_rewrite is an agreement count.
# Where a float is summarised rather than recorded cell by cell, this file's rewrite value is
# how many of the published labels the rewrite reproduces, and that is not a quantity an
# instrument which never reads the published page can compute: for those claims the claims file
# prints what the rewrite writes, which is the strongest statement available to it. The two
# coincide wherever the labels all reproduce and would silently look like agreement, so the
# exemption is stated on the class of claim rather than on the rows that differ.
agreement_counts <- str_starts(gt$table_figure, "fig_S4\\.") | gt$table_figure == "table_S2.1"

instrument_disagreements <-
  gt |>
  filter(!agreement_counts) |>
  drop_na(claim_id) |>
  inner_join(printed_claims, by = "claim_id") |>
  filter(!is.na(value_rewrite),
         sprintf(paste0("%.", digits, "f"), value_rewrite) != value_printed)

if (nrow(instrument_disagreements) > 0) {
  print(instrument_disagreements |>
          select(claim_id, value_paper, value_rewrite, value_printed), n = 40)
  stop(str_glue("The ground truth and maintained/in_text_claims.R disagree on ",
                "{nrow(instrument_disagreements)} claims."))
}

gt <- gt |> select(-value_paper_number, -digits, -mode)

write_csv(gt, here::here("ground_truth", str_glue("{paper_id}_ground_truth.csv")))

print(str_glue(
  "{nrow(gt)} rows. ",
  "Archive: {sum(gt$match == 1, na.rm = TRUE)} match, {sum(gt$match == 0, na.rm = TRUE)} do not, ",
  "{sum(is.na(gt$match))} unverifiable. ",
  "Rewrite: {sum(gt$match_rewrite == 1, na.rm = TRUE)} match, ",
  "{sum(gt$match_rewrite == 0, na.rm = TRUE)} do not, ",
  "{sum(is.na(gt$match_rewrite))} unverifiable."
))
print(str_glue(
  "Extraction: {nrow(published_claims)} claims, {length(declared)} needing a block, ",
  "{nrow(printed_claims)} printed by in_text_claims.R, ",
  "{nrow(transcription)} reconciled against the ground truth, ",
  "{sum(!agreement_counts & !is.na(gt$claim_id) & !is.na(gt$value_rewrite))} compared across ",
  "the two instruments, {nrow(instrument_disagreements)} disagreements."
))
print(gt |> filter(match_rewrite == 0 | match == 0) |>
        select(table_figure, claim, value_paper, value_rewrite, defect_locus),
      n = 100, width = 200)
