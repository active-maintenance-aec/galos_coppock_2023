# galos_coppock_2023/maintained/in_text_claims.R
# Output: printed CLAIM lines only; no files
# Depends on: maintained/output/, ground_truth/published_claims.csv, helpers.R
# Description: One block per claim the published article and its supplementary materials make,
#   each carrying the article's own sentence verbatim and then recomputing the number from the
#   pipeline's committed output. This is the second instrument: it never reads
#   ground_truth/galos_coppock_2023_ground_truth.csv, it never refits a model, and it reaches
#   every quantity by its own path, so where it and build_ground_truth.R disagree one of them is
#   wrong.
#
#   Every block prints a line of the form
#
#       CLAIM <claim_id> = <value> || <label>
#
#   which is what build_ground_truth.R parses to check coverage against
#   ground_truth/published_claims.csv. The value is printed at the precision the article prints,
#   so it can be read against the page without re-rounding.
#
#   It reads the extraction, ground_truth/published_claims.csv, and not the comparison. A block
#   about the supplement's own figure numbering cannot say anything without the numbers the
#   published pages carry, and reading the extraction is not the prohibited read.
#
#   Two transcription conventions. The published PDF sets the minus sign as U+2212 and the
#   multiplication sign in "Majority x gradient" as U+00D7; both are written here as ASCII.
#   Quotes are the sentences the article prints today, however wrong. Where a sentence is wrong
#   that is recorded in the ground truth and in the errata, never by editing the quote.

source(here::here("maintained", "helpers.R"))

options(width = 200)

out <- function(f) read_csv(here::here("maintained", "output", f), show_col_types = FALSE)

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
summary_stats <- out("text_summary_stats.csv")
magnitude <- out("text_difference_in_magnitude.csv")

# The extraction, read for the published strings a block needs in order to say anything at all
# about the article's own cross-references. value_paper is forced to character: a column whose
# entries all look numeric is guessed as double, which turns a published 1.50 into 1.5.
published_claims <- read_csv(
  here::here("ground_truth", "published_claims.csv"),
  col_types = cols(.default = col_character(), value_paper = col_character()),
  na = character()
)

paper <- function(id) {
  value <- published_claims$value_paper[published_claims$claim_id == id]
  stopifnot(length(value) == 1)
  value
}

# The same key the extraction and the ground truth build a study label into, so a claim about a
# study meets its published counterpart whichever way the two spell the minus sign.
claim_slug <- function(x) {
  x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_replace_all("^_|_$", "")
}

# claim: print one claim line in the form the coverage gate parses. digits is the number of
# decimal places the article prints, so a value printed here can be laid against the page
# without being rounded a second time.
claim <- function(id, value, label, digits = 0) {
  printed <- if (length(value) != 1 || is.na(value)) {
    "NA"
  } else {
    sprintf(paste0("%.", digits, "f"), value)
  }
  cat("CLAIM ", id, " = ", printed, " || ", label, "\n", sep = "")
}

# one: pull a single value out of an output file by an identifier that has to select exactly one
# row. The assertion is what stops a widened output file from turning a scalar into a vector and
# a claim into a silently empty line.
one <- function(x) {
  stopifnot(length(x) == 1, !is.na(x))
  x
}

stat <- function(name) one(summary_stats$value[summary_stats$quantity == name])
mag <- function(name) one(magnitude$value[magnitude$quantity == name])

study_estimates <- forest_s1_1 |> filter(facet == "Study estimates")
gradient_estimates <- forest_s5_62 |> filter(facet == "Study gender gradient estimates")

coefficient <- function(tbl, model_name, term_name, column) {
  one(tbl[[column]][tbl$model == model_name & tbl$term == term_name])
}

fitted_gradient <- function(type) {
  one(survey_field$estimate[survey_field$experiment_type == type &
                              survey_field$term == "fraction_female"])
}

# The earliest year from which at least 95 per cent of the evidence map's country-year cells
# come. "Around 2005" is a claim about where the mass of the record starts.
boom_year <- {
  years <- sort(evidence_map$year)
  years[ceiling(0.05 * length(years)) + 1]
}

# The supplement numbers its figures section.index, running S1.1, S2.2, S3.3, S3.4, then one
# panel per study from S4.5, then the gender gradient forest plot and the PRISMA diagram. The
# index of the last one therefore follows from how many studies there are.
supplementary_figures <- 4 + n_distinct(panels$study_name_citation) + 2

# Abstract ----

# "Since 1983, more than 70 employment audit experiments, carried out in more than 26 countries
#  across five continents, have randomized the gender of fictitious applicants to measure the
#  extent of hiring discrimination on the basis of gender."
claim("abstract_since_year", min(evidence_map$year),
      "earliest country-year cell in the evidence map of fig. S2.2", 0)
claim("abstract_countries", n_distinct(evidence_map$country),
      str_glue("countries the evidence map records for the 57 reanalysed studies; the abstract ",
               "describes the wider literature the search identified, which the deposit does ",
               "not enumerate"), 0)
claim("abstract_continents", n_distinct(evidence_map$continent),
      str_glue("continents the evidence map records: ",
               "{str_c(sort(unique(evidence_map$continent)), collapse = ', ')}"), 0)

# "The results are mixed: Some studies find discrimination against men, and others find
#  discrimination against women."
claim("abstract_mixed_signs",
      as.numeric(min(study_estimates$estimate) < 0 & max(study_estimates$estimate) > 0),
      str_glue("1 if the 57 study-level effects run in both directions; they run from ",
               "{sprintf('%.1f', min(study_estimates$estimate))} to ",
               "{sprintf('%.1f', max(study_estimates$estimate))} points, with ",
               "{sum(study_estimates$estimate < 0)} negative and ",
               "{sum(study_estimates$estimate >= 0)} positive"), 0)

# Introduction ----

# "The first study to deploy the audit experiment design for the study of discrimination on the
#  basis of gender appears to be (8), who sent 1982 resumes of fictitious men and women
#  applicants to 991 job openings in Victoria, Australia in 1986 and found a -2.4 percentage
#  point effect of being a woman on receiving an invitation to interview (cluster-robust SE: 1.2
#  points)."
#
# The year is the one quantity in that sentence the deposit contradicts: studies.rds dates the
# study to 1983, which is also what the abstract's "Since 1983" and the evidence map's earliest
# cell say. Both numbers are printed here rather than one, since the disagreement is between two
# sentences of the same article.
claim("intro_riach_year", min(evidence_map$year),
      str_glue("year the deposit and the evidence map give for the earliest study, an ",
               "Australian one, against an introduction that dates it to ",
               "{paper('intro_riach_year')}"), 0)
claim("intro_riach_ate",
      one(study_estimates$estimate[study_estimates$study_name_citation ==
                                     "Riach and Rich (8): Australia"]),
      "Riach and Rich (8) effect of being a woman, in percentage points", 1)
claim("intro_riach_se",
      one(study_estimates$std.error[study_estimates$study_name_citation ==
                                      "Riach and Rich (8): Australia"]),
      "Riach and Rich (8) cluster-robust standard error", 1)

# "A second early study in this line (9) randomized the gender of 130 applicants for 65
#  restaurant jobs in New York City in 1994, finding a large and negative but nonsignificant
#  effect (-12.3 points, SE: 8.4 points) of being a woman."
claim("intro_neumark_year", min(evidence_map$year[evidence_map$country == "United States"]),
      "earliest United States cell in the evidence map, against an introduction that dates the study to 1994", 0)
claim("intro_neumark_ate",
      one(study_estimates$estimate[study_estimates$study_name_citation ==
                                     "Neumark et al. (9): United States"]),
      "Neumark et al. (9) effect of being a woman, in percentage points", 1)
claim("intro_neumark_se",
      one(study_estimates$std.error[study_estimates$study_name_citation ==
                                      "Neumark et al. (9): United States"]),
      "Neumark et al. (9) standard error", 1)

# "We have obtained or reconstructed the data for a total of 57 such studies, reported in 51
#  papers."
claim("intro_studies", stat("Field studies"),
      "studies whose data were obtained or reconstructed", 0)
claim("intro_papers", NA_real_,
      str_glue("no counterpart in the deposit: the deposited frames carry a study identifier ",
               "and no paper identifier, so the {stat('Field studies')} studies cannot be ",
               "grouped into the papers that report them"), 0)

# "In contrast to the early results, two-thirds (37 of 57) show a positive effect of being a
#  woman, and the remainder show a negative effect."
claim("intro_positive", stat("Field studies with a positive ATE"),
      "studies with a positive effect of being a woman", 0)
claim("intro_positive_of", stat("Field studies"),
      "studies the positive ones are counted out of", 0)
claim("intro_two_thirds",
      stat("Field studies with a positive ATE") / stat("Field studies"),
      "share of studies with a positive effect, against a sentence that calls it two-thirds", 3)

# "A standard random-effects meta-analysis of the average treatment effect estimates of these
#  studies yields an estimate of 1.2 percentage points with an SE of 0.4 points."
claim("intro_meta_ate", one(forest_s1_1$estimate[forest_s1_1$facet == "Meta"]),
      "random-effects meta-analytic average treatment effect, in percentage points", 1)
claim("intro_meta_se", one(forest_s1_1$std.error[forest_s1_1$facet == "Meta"]),
      "standard error of the meta-analytic average treatment effect", 1)

# Theory ----

# "Among occupations that are fully men dominated, gender composition is 0, and among
#  occupations that are fully women-dominated, gender composition is 1."
claim("theory_composition_min", min(round(panels$fraction_female, 10), na.rm = TRUE),
      str_glue("smallest gender composition in the data, on a scale whose floor is 0; the ",
               "largest is {sprintf('%.2f', max(panels$fraction_female, na.rm = TRUE))}"), 0)
claim("theory_composition_max", ceiling(max(panels$fraction_female, na.rm = TRUE)),
      "ceiling of the gender composition scale, recovered from the observed range", 0)

# Mechanisms of gender discrimination ----

# "We manually coded the mechanisms described in 51 papers (encompassing 57 studies) into five
#  broad categories, described in brief below. We include a sixth category for studies whose
#  theory sections were not about the mechanisms through which applicant gender results in
#  hiring decisions."
#
# The coding behind Fig. 3 is not in the deposit, so the four claims of this paragraph and the
# three journal counts below have no pipeline counterpart at all. Printing what was checked
# instead is the informative thing a block can do here.
claim("mech_papers", NA_real_,
      "no counterpart in the deposit: no paper identifier is recorded, only study identifiers", 0)
claim("mech_studies", stat("Field studies"),
      "studies the 51 coded papers are said to encompass, against the 57 the deposit carries", 0)
claim("mech_categories", NA_real_,
      "no counterpart in the deposit: no mechanism coding is deposited, and fig. 3 has no code", 0)
claim("mech_sixth", NA_real_,
      "no counterpart in the deposit: the residual mechanism category is not recorded", 0)
claim("mech_theory_groups", NA_real_,
      "no counterpart in the deposit: the mechanism categories are not recorded", 0)

# "Most of our included studies were published in either economics (24) or sociology (14)
#  journals, with a handful of studies published in interdisciplinary, psychology, or management
#  science journals (13)."
claim("mech_economics", NA_real_,
      "no counterpart in the deposit: no journal or discipline variable is deposited", 0)
claim("mech_sociology", NA_real_,
      "no counterpart in the deposit: no journal or discipline variable is deposited", 0)
claim("mech_other_journals", NA_real_,
      str_glue("no counterpart in the deposit; the three counts sum to ",
               "{as.numeric(paper('mech_economics')) + as.numeric(paper('mech_sociology')) + as.numeric(paper('mech_other_journals'))}, ",
               "which is the number of papers the article says it coded"), 0)

# Design ----

# "The PRISMA search process yielded over 70 papers that met our inclusion criteria. Figure
#  S6.66 is a 'PRISMA flow diagram' that illustrates the search strategy and reports the number
#  of records retained at each stage of the process."
#
# The article's pointer is to a figure the supplement does not contain. The supplement's own
# front matter lists Figs. S1.1 to S6.63, and its PRISMA flow diagram is fig. S6.63.
claim("design_prisma_papers", as.numeric(paper("s6_included_papers")),
      str_glue("papers the PRISMA flow diagram reports as included, against a sentence that ",
               "says the search yielded over {paper('design_prisma_papers')} papers meeting the ",
               "inclusion criteria"), 0)
claim("design_prisma_figure", supplementary_figures,
      str_glue("running index of the last figure the supplement contains, which is its PRISMA ",
               "flow diagram and is printed S6.{supplementary_figures}, against an article that ",
               "cites fig. S6.{paper('design_prisma_figure')}"), 0)
claim("design_evidence_map_figure", 2,
      "running index of the evidence map, printed S2.2 in the supplement and fig. S.2.2 in the article", 0)

# "We have obtained or reconstructed the microdata for 51 such papers, for a total of 57
#  separate audit studies."
claim("design_microdata_papers", NA_real_,
      "no counterpart in the deposit: no paper identifier is recorded", 0)
claim("design_studies", stat("Field studies"),
      "separate audit studies the deposit carries", 0)

# "The explosion in audit experimentation began around 2005, mainly in Europe and North
#  America."
claim("design_boom_year", boom_year,
      str_glue("earliest year from which 95 per cent of the evidence map's country-year cells ",
               "come; {sum(evidence_map$year < 2005)} of {nrow(evidence_map)} cells precede ",
               "2005"), 0)

# "In five occupation settings, no applicant (man or woman) received a callback, so in those
#  cases, we used the adjustment described in (113) to estimate an SE."
claim("design_zero_callback", sum(panels$estimate == 0, na.rm = TRUE),
      str_glue("occupation CATEs that are exactly zero; the deposit carries the estimates and ",
               "not the callback rates, and the Agresti-Coull adjustment leaves a zero-callback ",
               "cell at exactly zero only when the two groups are the same size"), 0)

# "We present CATE estimates for all 57 studies in section S4."
claim("design_s4_studies", n_distinct(panels$study_name_citation),
      "studies with a panel of occupation-level CATE estimates", 0)

# Results ----

# "Each point is sized proportionally to the weight that it receives in the meta-regression,
#  with estimates that are more precise receiving more weight than estimates that are less
#  precise. The meta-regression fit and 95% confidence region are overlaid on the data."
#
# The deposit stores the interval bounds rather than the level, so the level is recovered from
# the bounds: the Table 1 intervals are 1.96 standard errors wide on each side, which is the
# two-sided normal critical value for 95 per cent.
half_widths <- (table_1$conf.high - table_1$estimate) / table_1$std.error
claim("results_ci_level", 100 * (2 * pnorm(mean(half_widths)) - 1),
      str_glue("confidence level implied by the Table 1 intervals, ",
               "{sprintf('%.4f', mean(half_widths))} standard errors wide on each side"), 0)

# "The meta-regression estimate itself is shown in column 1 of Table 1. We find that gender
#  gradient is 8.15 with an SE of 1.50."
claim("results_gradient", coefficient(table_1, "Model 1", "fraction_female", "estimate"),
      "gender gradient, quoted from column 1 of Table 1 into the text", 2)
claim("results_gradient_se", coefficient(table_1, "Model 1", "fraction_female", "std.error"),
      "standard error of the gender gradient, quoted into the text", 2)

# "In an occupation with 0% women, we would expect the average CATE to be -3.02 percentage
#  points and in an occupation with 100% women, we would expect the average CATE to be -3.02 +
#  8.15 = +5.13 points."
#
# The intercept and the gradient appear twice in that sentence, once as quantities and once
# inside the arithmetic, so both statements are printed rather than one: a restated quantity is
# exactly the shape in which two halves of a sentence drift apart.
claim("results_cate_0", mag("Predicted CATE at 0 percent women"),
      "predicted CATE at 0 per cent women, in percentage points", 2)
claim("results_cate_0_restated", mag("Predicted CATE at 0 percent women"),
      "the same intercept, restated inside the arithmetic of the sentence", 2)
claim("results_gradient_restated", coefficient(table_1, "Model 1", "fraction_female", "estimate"),
      "the same gradient, restated inside the arithmetic of the sentence", 2)
claim("results_cate_100", mag("Predicted CATE at 100 percent women"),
      "predicted CATE at 100 per cent women, in percentage points", 2)

# "The magnitude of these biases is higher in women-dominated occupations than in men-dominated
#  occupations, by a statistically significant 2.10 points (bootstrapped SE: 0.58), perhaps
#  suggesting that pro-status quo forces operate differently across contexts."
claim("results_magnitude_difference", mag("Difference in magnitude"),
      "difference in the magnitude of the two predicted biases, in percentage points", 2)
claim("results_magnitude_se", mag("Bootstrapped SE of the difference in magnitude"),
      "bootstrapped standard error of that difference", 2)

# "In columns 2 through 5 of Table 1, we present a series of fixed-effect models that variously
#  include indicators for country, year, country and year, and study. Across these
#  specifications, the estimated gender gradient remains quite stable, indicating that the
#  overall pattern that we observe in Fig. 4 also holds within subsets of the data."
gradient_by_model <- table_1 |> filter(term == "fraction_female")
claim("results_fe_first_column", 2,
      "first Table 1 column carrying fixed effects, of the five the table prints", 0)
claim("results_fe_last_column", nrow(gradient_by_model),
      "last Table 1 column carrying fixed effects, which is the number of models the table prints", 0)
claim("results_gradient_stable",
      max(gradient_by_model$estimate) - min(gradient_by_model$estimate),
      str_glue("spread of the gender gradient across the five models, which run ",
               "{str_c(sprintf('%.2f', gradient_by_model$estimate), collapse = ', ')}"), 2)

# "The third column of Table 2 shows that while it is true that the gender gradient is estimated
#  to be steeper for minority applicants than for majority applicants, the uncertainty attending
#  to the estimates is too great to declare the difference between them statistically
#  significant."
claim("results_minority_steeper",
      as.numeric(coefficient(table_2, "Model 2", "fraction_female", "estimate") >
                   coefficient(table_2, "Model 1", "fraction_female", "estimate")),
      str_glue("1 if the minority gradient exceeds the majority gradient: ",
               "{sprintf('%.2f', coefficient(table_2, 'Model 2', 'fraction_female', 'estimate'))} ",
               "against ",
               "{sprintf('%.2f', coefficient(table_2, 'Model 1', 'fraction_female', 'estimate'))}"), 0)
claim("results_interaction_not_significant",
      as.numeric(coefficient(table_2, "Model 3", "fraction_female:ethnicity_fctMajority",
                             "p.value") > 0.05),
      str_glue("1 if the majority by gradient interaction is not significant at the 0.05 level; ",
               "its p-value is ",
               "{sprintf('%.3f', coefficient(table_2, 'Model 3', 'fraction_female:ethnicity_fctMajority', 'p.value'))}"), 0)

# Discussion ----

# "We set out to synthesize what has been learned over the past four decades from the repeated
#  application of a single research design (the employment audit experiment) to a difficult
#  measurement problem (the presence of gender-based employment discrimination)."
claim("disc_decades", (max(evidence_map$year) - min(evidence_map$year)) / 10,
      str_glue("decades the evidence map spans, {min(evidence_map$year)} to ",
               "{max(evidence_map$year)}"), 1)

# "The gender gradient is positive: Women are discriminated against where there are few women,
#  and men are discriminated against where there are few men."
claim("disc_gradient_positive",
      as.numeric(coefficient(table_1, "Model 1", "fraction_female", "estimate") > 0 &
                   coefficient(table_1, "Model 1", "fraction_female", "conf.low") > 0),
      str_glue("1 if the pooled gradient is positive with an interval clear of zero: ",
               "{sprintf('%.2f', coefficient(table_1, 'Model 1', 'fraction_female', 'estimate'))} ",
               "[{sprintf('%.2f', coefficient(table_1, 'Model 1', 'fraction_female', 'conf.low'))}, ",
               "{sprintf('%.2f', coefficient(table_1, 'Model 1', 'fraction_female', 'conf.high'))}]"), 0)

# "As described in section S5, we can obtain study-specific gender gradient estimates for 37 of
#  our studies, only 7 of which reach statistical significance. Informally speaking, this
#  analysis puts the statistical power of the average gender employment audit study to detect
#  the gender gradient at 7/37 = 19%."
claim("disc_gradient_studies", nrow(gradients),
      "studies with a study-specific gender gradient estimate", 0)
claim("disc_significant", sum(gradients$p.value <= 0.05),
      str_glue("gender gradients significant at the 0.05 level, of which ",
               "{sum(gradients$p.value <= 0.05 & gradients$estimate > 0)} are positive"), 0)
claim("disc_power_numerator", sum(gradients$p.value <= 0.05),
      "numerator of the power ratio", 0)
claim("disc_power_denominator", nrow(gradients),
      "denominator of the power ratio", 0)
claim("disc_power", 100 * sum(gradients$p.value <= 0.05) / nrow(gradients),
      "share of study-level gradients reaching significance, in per cent", 0)

# "The audit experimental record on gender bias in hiring currently stands at over 70 studies
#  and by synthesizing 57 of them, we learn that gender composition predicts gender bias."
claim("disc_synthesized", stat("Field studies"), "studies synthesized", 0)

# "Figure S2.2 shows where and when the existing studies were conducted. Most were conducted in
#  North America and Europe since 2005; only a handful of studies have been conducted in South
#  America, Asia, or Africa."
claim("disc_since_year", boom_year,
      "earliest year from which 95 per cent of the country-year cells come", 0)
claim("disc_most_europe_north_america",
      as.numeric(sum(evidence_map$continent %in% c("Europe", "N. America")) >
                   nrow(evidence_map) / 2),
      str_glue("1 if most country-year cells are in Europe or North America: ",
               "{sum(evidence_map$continent %in% c('Europe', 'N. America'))} of ",
               "{nrow(evidence_map)}"), 0)
claim("disc_other_continents",
      sum(evidence_map$continent %in% c("S. America", "Asia", "Africa")),
      str_glue("country-year cells outside Europe, North America and Oceania: ",
               "{str_c(str_glue('{count(evidence_map, continent)$continent} ",
               "{count(evidence_map, continent)$n}'), collapse = ', ')}"), 0)

# Figure and table notes ----

# Fig. 4 note: "The regression line is derived from the model reported in column 1 of Table 1."
claim("fig4_note_column", 1,
      "Table 1 column the fig. 4 regression line is derived from, the first of the five the table prints", 0)

# Fig. 5 note: "Regression lines derived from models reported in columns 1 and 2 of Table 2."
claim("fig5_note_column_1", 1, "first Table 2 column the fig. 5 lines are derived from", 0)
claim("fig5_note_column_2", 2,
      str_glue("second Table 2 column the fig. 5 lines are derived from, of the ",
               "{n_distinct(table_2$model)} the table prints"), 0)

# Table 1 note: "*P < 0.05, **P < 0.01, and ***P < 0.001."
#
# The thresholds are recoverable from the table itself: every starred coefficient's p-value sits
# under the threshold its marker names, and the largest p-value carrying each marker is what the
# note asserts a bound on.
starred <- bind_rows(table_1, table_2) |>
  mutate(stars = case_when(p.value < 0.001 ~ 3, p.value < 0.01 ~ 2, p.value < 0.05 ~ 1,
                           .default = 0))
claim("table_1_note_p05", 0.05,
      str_glue("one-star threshold; the largest p-value carrying exactly one star is ",
               "{sprintf('%.3f', max(starred$p.value[starred$stars == 1]))}"), 2)
claim("table_1_note_p01", 0.01,
      str_glue("two-star threshold; the largest p-value carrying exactly two stars is ",
               "{sprintf('%.3f', max(starred$p.value[starred$stars == 2]))}"), 2)
claim("table_1_note_p001", 0.001,
      str_glue("three-star threshold; the largest p-value carrying three stars is ",
               "{sprintf('%.6f', max(starred$p.value[starred$stars == 3]))}"), 3)

# Table 2 note: "*P < 0.05, **P < 0.01, and ***P < 0.001."
#
# The same three thresholds, restated verbatim beneath the second table. A quantity stated twice
# drifts twice, so both statements are printed rather than one.
claim("table_2_note_p05", 0.05, "one-star threshold, restated beneath Table 2", 2)
claim("table_2_note_p01", 0.01, "two-star threshold, restated beneath Table 2", 2)
claim("table_2_note_p001", 0.001, "three-star threshold, restated beneath Table 2", 3)

# Tables 1 and 2 ----
#
# Every cell of both published tables: the coefficient, its standard error and its significance
# marker, plus the two goodness-of-fit rows per model. A per-cell quote would repeat the same
# caption thirty-eight times, so the claim ids carry the location instead.
table_term_ids <- c(intercept = "intercept", fraction_female = "gradient",
                    ethnicity_fctMajority = "majority",
                    `fraction_female:ethnicity_fctMajority` = "majority_x_gradient")

stars_from_p <- function(p) {
  case_when(is.na(p) ~ NA_real_, p < 0.001 ~ 3, p < 0.01 ~ 2, p < 0.05 ~ 1, .default = 0)
}

table_claims <- function(tbl, stem, table_name) {
  for (i in seq_len(nrow(tbl))) {
    row <- tbl[i, ]
    key <- str_glue("{stem}_{str_replace(str_to_lower(row$model), ' ', '')}_",
                    "{table_term_ids[row$term]}")
    claim(str_glue("{key}_estimate"), row$estimate,
          str_glue("{table_name}, {row$model}, {row$term}"), 2)
    claim(str_glue("{key}_std_error"), row$std.error,
          str_glue("{table_name}, {row$model}, {row$term}, standard error"), 2)
    claim(str_glue("{key}_stars"), stars_from_p(row$p.value),
          str_glue("{table_name}, {row$model}, {row$term}, significance stars at p = ",
                   "{sprintf('%.4f', row$p.value)}"), 0)
  }
  gof <- distinct(tbl, model, tau.squared, nobs)
  for (i in seq_len(nrow(gof))) {
    key <- str_glue("{stem}_{str_replace(str_to_lower(gof$model[i]), ' ', '')}")
    claim(str_glue("{key}_tau_squared"), gof$tau.squared[i],
          str_glue("{table_name}, {gof$model[i]}, tau squared"), 2)
    claim(str_glue("{key}_nobs"), gof$nobs[i],
          str_glue("{table_name}, {gof$model[i]}, observations"), 0)
  }
}

table_claims(table_1, "table_1", "Table 1")
table_claims(table_2, "table_2", "Table 2")

# Section S1 ----

# "Figure S1.1 shows a standard forest plot of the 57 studies in our database. On average, the
#  effect of being a woman (versus a man) is a 1.2 percentage point increase in callback (SE:
#  0.4)."
claim("s1_studies", nrow(study_estimates), "studies plotted in fig. S1.1", 0)
claim("s1_meta_ate", one(forest_s1_1$estimate[forest_s1_1$facet == "Meta"]),
      "meta-analytic average effect printed on fig. S1.1, in percentage points", 1)
claim("s1_meta_se", one(forest_s1_1$std.error[forest_s1_1$facet == "Meta"]),
      "standard error printed beside it", 1)

# The 116 numbers printed beside the 58 points of fig. S1.1, and the 26 and 76 printed on figs.
# S3.4 and S5.62. None of them appears in any sentence of the paper.
forest_claims <- function(forest, stem, label) {
  for (i in seq_len(nrow(forest))) {
    key <- claim_slug(forest$study_name_citation[i])
    claim(str_glue("{stem}_est_{key}"), forest$estimate[i],
          str_glue("{label} plotted for {forest$study_name_citation[i]}"), 1)
    claim(str_glue("{stem}_se_{key}"), forest$std.error[i],
          str_glue("{label} standard error plotted for {forest$study_name_citation[i]}"), 1)
  }
}

forest_claims(forest_s1_1, "fig_s1_1", "Average treatment effect")

# Section S2 ----

# "Figure S2.2 presents an evidence map of audit experiments that measure gender-based hiring
#  discrimination by country and time, organized by continent. We see one early study in
#  Australia in the 1980s, followed by one study in the United States in the 1990s. We see a
#  boom in audit experiments beginning around 2005 in Europe, North America, and Oceania."
claim("s2_early_1980s", sum(evidence_map$year %/% 10 == 198),
      str_glue("country-year cells in the 1980s: ",
               "{str_c(evidence_map$country[evidence_map$year %/% 10 == 198], collapse = ', ')}"), 0)
claim("s2_early_1990s", sum(evidence_map$year %/% 10 == 199),
      str_glue("country-year cells in the 1990s: ",
               "{str_c(evidence_map$country[evidence_map$year %/% 10 == 199], collapse = ', ')}"), 0)
claim("s2_boom_year", boom_year,
      "earliest year from which 95 per cent of the country-year cells come, restated in section S2", 0)

# Table S2.1 ----

# "Table S2.1 lists the field experiments included in our meta-analysis, along with the total
#  sample size, the number of occupations, and whether the study's design allowed us to include
#  it in our analyses by majority versus minority status."
#
# The ground truth compares these three columns cell by cell against the published table. This
# instrument never reads the published page, so what it can say is how many cells the rewrite
# produces in each column.
claim("table_s2_1_studies_listed", nrow(study_manifest),
      "studies the rewrite's manifest lists", 0)
claim("table_s2_1_subjects", sum(!is.na(study_manifest$n_subjects_from_df)),
      str_glue("subject counts the rewrite reconstructs, as residual degrees of freedom plus ",
               "one summed over a study's occupations"), 0)
claim("table_s2_1_occupations", sum(!is.na(study_manifest$n_occupations)),
      "occupation counts the rewrite reconstructs", 0)
claim("table_s2_1_inclusion", sum(!is.na(study_manifest$included_majority_minority)),
      str_glue("majority / minority inclusion flags the rewrite reconstructs, of which ",
               "{sum(study_manifest$included_majority_minority == 'Yes')} are Yes"), 0)

# Section S3 ----

# "In addition to the audit field experiments discussed in the main text, we also collected 12
#  vignette survey experiments that simulated hiring settings and asked survey respondents how
#  they would evaluate hypothetical job applicants."
claim("s3_survey_experiments", nrow(forest_s3_4 |> filter(facet == "Study estimates")),
      "survey experiments plotted in fig. S3.4", 0)

# "Despite the obvious differences across experimental mode and context, we find that the gender
#  gradient (the slope with respect to gender composition) is very similar across the two
#  experimental settings."
claim("s3_gradients_similar",
      abs(fitted_gradient("Survey experiments") - fitted_gradient("Field experiments")),
      str_glue("distance between the two fitted gradients of fig. S3.3: survey ",
               "{sprintf('%.2f', fitted_gradient('Survey experiments'))} against field ",
               "{sprintf('%.2f', fitted_gradient('Field experiments'))}"), 2)

forest_claims(forest_s3_4, "fig_s3_4", "Average treatment effect")

# Section S4 ----

# "In this section, we provide study-by-study occupation-level CATEs. In the top panel of each
#  figure, we provide occupation-level CATEs. When we have a sufficient number of CATEs (3), we
#  can estimate a study-level gender gradient, which we report in the bottom panels of these
#  figures. The 37 gender gradients we are able to estimate are meta-analyzed in Figure S5.62."
claim("s4_minimum_cates", min(gradients$n_occupations),
      str_glue("occupations in the smallest study with a gender gradient; the largest has ",
               "{max(gradients$n_occupations)}"), 0)
claim("s4_gradients", nrow(gradients),
      "gender gradients estimated across the 57 studies", 0)

# The 57 published panels, figs. S4.5 to S4.61, each printing an "estimate (SE)" label, a gender
# composition and a sample size beside every occupation. The ground truth compares those labels
# against the published page; this instrument prints how many of each the rewrite writes.
#
# Which published figure a study's panel is depends on the order the supplement prints them in,
# which is not the order the rewrite draws them: the published appendix puts Capeau last and
# the two Arai panels the other way round. The mapping therefore comes from the extraction,
# which carries each panel's study label beside its figure id, rather than from a position.
panel_ids <- published_claims |>
  filter(str_detect(claim_id, "^s4_[0-9]+_entries$")) |>
  transmute(figure = str_remove(claim_id, "_entries$"),
            key = claim_slug(str_remove(claim, "^Plotted estimate and standard error labels, ")))

stopifnot(nrow(panel_ids) == n_distinct(panels$study_name_citation))

panel_counts <- panels |>
  group_by(study_name_citation) |>
  summarize(entries = sum(!is.na(entry)),
            compositions = sum(!is.na(fraction_female)),
            sample_sizes = sum(!is.na(n_from_df))) |>
  mutate(key = claim_slug(study_name_citation)) |>
  inner_join(panel_ids, by = "key")

stopifnot(nrow(panel_counts) == nrow(panel_ids))

for (i in seq_len(nrow(panel_counts))) {
  row <- panel_counts[i, ]
  claim(str_glue("{row$figure}_entries"), row$entries,
        str_glue("estimate and standard error labels the rewrite writes for ",
                 "{row$study_name_citation}"), 0)
  claim(str_glue("{row$figure}_compositions"), row$compositions,
        str_glue("gender composition labels the rewrite writes for {row$study_name_citation}"), 0)
  claim(str_glue("{row$figure}_sample_sizes"), row$sample_sizes,
        str_glue("sample size labels the rewrite writes for {row$study_name_citation}, as ",
                 "residual degrees of freedom plus one"), 0)
}

claim("s4_panels_published", n_distinct(panels$study_name_citation),
      "study panels the rewrite draws, against the 57 the published appendix draws", 0)
# Counted off the figure rather than off its data. The gender gradient estimate for Albert
# et al. (67) is 139.82, outside the panel's x range, so ggplot clips its label away and the
# facet prints a truncated interval with no point and no label, exactly as the published page
# does. panels carries 335 entries; 334 of them reach the page.
claim("s4_panel_entries_published",
      length(unlist(str_extract_all(
        pdftools::pdf_text(here::here("maintained", "output", "figure_s4_study_panels.pdf")),
        "-?\\d+\\.\\d+\\s*\\(\\d+\\.\\d+\\)"
      ))),
      "estimate and standard error labels the rewrite prints across all 57 panels", 0)

# Section S5 ----

# "We can meta-analyze the 37 study-by-study gender gradient estimates. This meta-analysis is
#  conceptually similar to the study-fixed-effects specification in model 5 in Table 1 in that
#  it averages the within-study variation. This meta-analysis operates directly on the gender
#  gradient estimates presented in the foregoing section, so can only include studies with three
#  or more occupations (37 studies). By contrast, the fixed effects estimation includes the
#  study if it distinguishes among two or more occupations."
claim("s5_gradients", nrow(gradient_estimates),
      "study-level gender gradients plotted in fig. S5.62", 0)
claim("s5_model_reference", n_distinct(table_1$model),
      "Table 1 model the gradient meta-analysis is compared to, which is the last of the five", 0)
claim("s5_minimum_occupations", min(gradients$n_occupations),
      "occupations in the smallest study entering the gradient meta-analysis", 0)
occupations_per_study <- count(panels |> filter(!is.na(fraction_female)), study_name_citation)
claim("s5_fixed_effects_minimum", min(occupations_per_study$n[occupations_per_study$n > 1]),
      str_glue("occupations in the smallest multi-occupation study, of which there are ",
               "{sum(occupations_per_study$n > 1)}"), 0)
claim("s5_studies", nrow(gradients),
      "studies with three or more occupations", 0)

# "The meta-analytic average gender gradient is 9.0 with a standard error of 2.1, which is quite
#  similar to the gender gradient estimates presented in Table 1."
claim("s5_meta_gradient", one(forest_s5_62$estimate[forest_s5_62$facet == "Meta"]),
      "meta-analytic average gender gradient", 1)
claim("s5_meta_se", one(forest_s5_62$std.error[forest_s5_62$facet == "Meta"]),
      "its standard error", 1)
claim("s5_similar_to_table_1",
      abs(one(forest_s5_62$estimate[forest_s5_62$facet == "Meta"]) -
            coefficient(table_1, "Model 1", "fraction_female", "estimate")),
      str_glue("distance from the Table 1 model 1 gradient; the five Table 1 gradients run ",
               "{sprintf('%.2f', min(gradient_by_model$estimate))} to ",
               "{sprintf('%.2f', max(gradient_by_model$estimate))}"), 2)

# "This plot also shows that the statistical power for any particular study to detect a positive
#  gender gradient is low. Only 7 of these 37 gender gradient estimates are statistically
#  significant."
claim("s5_significant", sum(gradients$p.value <= 0.05),
      "gender gradient estimates significant at the 0.05 level", 0)
claim("s5_significant_of", nrow(gradients),
      "gradient estimates the significant ones are counted out of", 0)

forest_claims(forest_s5_62, "fig_s5_62", "Gender gradient")

# Supplementary front matter ----

# "This PDF file includes: Sections S1 to S6, Figs. S1.1 to S6.63, Table S2.1, References"
claim("supp_first_figure", 1,
      "running index of the first figure the supplement lists, the fig. S1.1 forest plot", 0)
claim("supp_last_figure", supplementary_figures,
      str_glue("running index of the last figure the supplement lists, counted through its own ",
               "numbering: four figures before section S4, then one panel per each of the ",
               "{n_distinct(panels$study_name_citation)} studies, then the gender gradient ",
               "forest plot and the PRISMA diagram"), 0)
claim("supp_tables", 1,
      "tables the supplement lists, which is the study manifest of table S2.1", 0)

# Section S6, the PRISMA flow diagram ----

# "Reports/publication included in review (n = 51), from which separate audit studies included
#  in review (n = 57)"
claim("s6_included_papers", NA_real_,
      "no counterpart in the deposit: no paper identifier is recorded, only study identifiers", 0)
claim("s6_included_studies", stat("Field studies"),
      "separate audit studies the deposit carries, against the 57 the flow diagram reports", 0)
