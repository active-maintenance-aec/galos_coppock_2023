# galos_coppock_2023/ground_truth/extract_published_claims.R
# Output: ground_truth/published_claims.csv
# Depends on: the published article and supplementary materials PDFs, and pdftotext (poppler)
# Description: The extraction. Every numeric claim the article and its supplementary materials
#   make, with the string the page prints, classified by hand into the five claim types.
#
#   Two sources feed it. The prose claims, the two published tables and the front matter are
#   typed below from the published pages, one row per number. The float cells are read off the
#   supplementary PDF's own text layer: figs. S1.1, S3.4 and S5.62 print an estimate and a
#   standard error beside every point, the 57 study panels of section S4 print an "estimate (SE)"
#   label, a gender composition and a sample size beside every occupation, and table S2.1 prints
#   three columns for each of 57 studies. Typing 1,400 numbers by hand would be worse than
#   useless, so the page is parsed positionally with -layout, which keeps a plotted label and its
#   value on one line.
#
#   This file is the only place in the repository where a published number is written down
#   twice: build_ground_truth.R carries its own transcription, and its gate compares the two.
#   Two hand transcriptions of the same pages are free to drift, and nothing else compares them.
#
#   It is not part of run_all.R. The repository does not carry the published PDFs, which are the
#   publisher's to distribute, so the article directory comes from an environment variable. The
#   article and its supplementary materials are open access at
#   https://doi.org/10.1126/sciadv.ade7979. Point ARTICLE_PDF_DIR at a directory holding
#   galos_coppock_2023.pdf and galos_coppock_2023_appendix.pdf and run this file.
#
#   Nothing in maintained/ reads it. A published number is a comparison target, never an input.

library(here)
library(tidyverse)

here::i_am("ground_truth/extract_published_claims.R")

article_dir <- Sys.getenv("ARTICLE_PDF_DIR", unset = tempdir())
appendix_pdf <- file.path(article_dir, "galos_coppock_2023_appendix.pdf")

if (!file.exists(appendix_pdf)) {
  stop(str_glue(
    "Set ARTICLE_PDF_DIR to a directory containing galos_coppock_2023_appendix.pdf. ",
    "Looked in: {article_dir}"
  ))
}

stopifnot(nzchar(Sys.which("pdftotext")))

# The supplement as text, page by page, in layout order ----
# -layout is what puts a plotted study label and the estimate drawn beside it on one line.
# Reading order interleaves the axis labels with the estimates and cannot be parsed.
appendix_txt <- tempfile(fileext = ".txt")
system2("pdftotext", c("-layout", shQuote(appendix_pdf), shQuote(appendix_txt)))
pages <- str_split(read_file(appendix_txt), "\f")[[1]]

# Runs the text layer keeps and the page does not print ----
# pdftotext reads the content stream and knows nothing about clipping, so a label ggplot drew
# outside its panel comes back as text that no reader can see. Both cases here are labels that
# overflow a panel: fig. S1.1's Neumark row is right-aligned and its leading "-1" falls off the
# left edge, so the page prints 2.3 (8.4) for an estimate of -12.31 (errata entry 9), and fig.
# S4.7's gender gradient estimate of 139.82 is outside the panel's x range, so its whole label
# goes and the facet prints a truncated interval with no point and no label.
#
# These cannot be found with pdftotext, which is the tool that cannot see them. They are found
# by diffing it against a clip-aware reader, which is what ground_truth/find_clipped_runs.py
# does; re-run that after any change to the published PDFs and this table should match it.
clipped_runs <- tribble(
  ~wrong, ~right,
  "Neumark et al. (9): United States−12.3 (8.4)", "Neumark et al. (9): United States 2.3 (8.4)",
  "139.82 (43.88)", ""
)
for (i in seq_len(nrow(clipped_runs))) {
  hits <- sum(str_count(pages, fixed(clipped_runs$wrong[i])))
  stopifnot(hits == 1)
  pages <- str_replace_all(pages, fixed(clipped_runs$wrong[i]), clipped_runs$right[i])
}

# A stable id for a study label, so the extraction and build_ground_truth.R land on the same
# claim_id from two different spellings of the same name. The supplement sets the minus sign as
# U+2212 and the build script types a hyphen, and both have to reduce to one key.
claim_slug <- function(x) {
  x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_replace_all("^_|_$", "")
}

# Forest plots ----
# Each plotted point carries "label estimate (SE)" on one line. The label runs to the last
# space before the estimate.
forest_rows <- function(page_text, figure) {
  lines <- str_split(page_text, "\n")[[1]]
  matched <- str_match(lines, "^\\s*(\\S.*?)\\s*(−?[0-9]+\\.[0-9])\\s+\\((\\S+)\\)\\s*$")
  keep <- !is.na(matched[, 1])
  tibble(
    figure = figure,
    label = str_squish(matched[keep, 2]),
    estimate = str_replace(matched[keep, 3], "−", "-"),
    std_error = matched[keep, 4]
  )
}

# Fig. S5.62 wraps a long study label over two lines, with the country on the line carrying the
# estimate, so the author line above it has to be glued back on.
forest_rows_wrapped <- function(page_text, figure) {
  lines <- str_split(page_text, "\n")[[1]]
  matched <- str_match(lines, "^\\s*(\\S.*?)\\s*(−?[0-9]+\\.[0-9])\\s+\\((\\S+)\\)\\s*$")
  out <- vector("list", length(lines))
  for (i in seq_along(lines)) {
    if (is.na(matched[i, 1])) next
    tail_label <- str_squish(matched[i, 2])
    lead <- if (i > 1) str_squish(lines[i - 1]) else ""
    label <- if (str_detect(lead, "\\)$")) str_c(lead, ": ", tail_label) else tail_label
    out[[i]] <- tibble(
      figure = figure,
      label = label,
      estimate = str_replace(matched[i, 3], "−", "-"),
      std_error = matched[i, 4]
    )
  }
  list_rbind(out)
}

s1_1 <- forest_rows(pages[str_detect(pages, "Figure S1.1: Standard meta-analysis")], "fig_S1.1")
s3_4 <- forest_rows(pages[str_detect(pages, "Figure S3.4: Standard meta-analysis")], "fig_S3.4")
s5_62 <- forest_rows_wrapped(
  pages[str_detect(pages, "Figure S5.62: Meta-analysis of gender gradient")], "fig_S5.62"
)

stopifnot(nrow(s1_1) == 58, nrow(s3_4) == 13, nrow(s5_62) == 38)

forest_claims <-
  bind_rows(s1_1, s3_4, s5_62) |>
  pivot_longer(c(estimate, std_error), names_to = "part", values_to = "value_paper") |>
  transmute(
    claim_id = str_glue("{claim_slug(figure)}_{if_else(part == 'estimate', 'est', 'se')}_{claim_slug(label)}"),
    location = figure,
    claim_type = "pipeline",
    value_paper,
    needs_block = TRUE,
    claim = str_glue(
      "{if_else(part == 'estimate', 'Estimate', 'Standard error')} printed beside ",
      "{label} in {str_replace(figure, 'fig_', 'fig. ')}"
    ),
    notes = ""
  )

# The study by study panels, figs. S4.5 to S4.61 ----
# Each panel prints three labels per occupation: an "estimate (SE)" pair, the share of women in
# the occupation, and a sample size. Rather than one claim per number, which would be 931 of
# them, each panel contributes three claims recording how many labels of each kind it prints.
# That is the granularity the ground truth compares at.
#
# Two or three panels share a page, so the split is at the captions rather than at the page
# breaks: a caption sits below its own figure, so a panel is the text between the caption above
# it and its own.
appendix_text <- str_c(pages, collapse = "\n")
captions <- str_locate_all(appendix_text, "Figure S4\\.[0-9]+: [^\n]+")[[1]]
caption_text <- str_sub(appendix_text, captions[, 1], captions[, 2])
stopifnot(nrow(captions) == 57)

panel_blocks <- str_sub(appendix_text,
                        c(1, captions[-nrow(captions), 2] + 1),
                        captions[, 1] - 1)

panel_counts <-
  tibble(
    figure = str_match(caption_text, "Figure (S4\\.[0-9]+):")[, 2],
    label = str_squish(str_match(caption_text, "Figure S4\\.[0-9]+: (.*)$")[, 2]),
    entries = str_count(panel_blocks, "−?[0-9]+\\.[0-9]{2} \\([0-9]+\\.[0-9]{2}\\)"),
    compositions = str_count(panel_blocks, "[0-9]+% women in occupation"),
    sample_sizes = str_count(panel_blocks, "N = [0-9]+")
  )

panel_claims <-
  panel_counts |>
  pivot_longer(c(entries, compositions, sample_sizes),
               names_to = "part", values_to = "value_paper") |>
  transmute(
    claim_id = str_glue("{claim_slug(figure)}_{part}"),
    location = str_glue("fig_{figure}"),
    claim_type = "pipeline",
    value_paper = as.character(value_paper),
    needs_block = TRUE,
    claim = case_when(
      part == "entries" ~ str_glue("Plotted estimate and standard error labels, {label}"),
      part == "compositions" ~ str_glue("Occupation gender composition labels, {label}"),
      .default = str_glue("Occupation sample size labels, {label}")
    ),
    notes = "Count of the labels the published panel prints"
  )

# Table S2.1 ----
# 57 rows of three columns. What the ground truth compares is how many of each column's cells
# the rewrite reproduces, so the extraction records how many of each the table prints.
# The table runs over two pages, and only the first carries the column headers, so the rows are
# found by their own shape rather than by which page they sit on.
manifest_lines <- str_split(appendix_text, "\n")[[1]]
manifest_matched <- str_match(manifest_lines,
                              "^\\s*(\\S.*?)\\s+([0-9]+)\\s+([0-9]+)\\s+(Yes|No)\\s*$")
manifest_rows <- tibble(
  label = str_squish(manifest_matched[, 2]),
  n_subjects = manifest_matched[, 3],
  n_occupations = manifest_matched[, 4],
  included = manifest_matched[, 5]
) |>
  drop_na(label)

stopifnot(nrow(manifest_rows) == 57)

manifest_claims <- tribble(
  ~claim_id, ~claim,
  "table_s2_1_studies_listed", "Studies listed in table S2.1",
  "table_s2_1_subjects", "Subject counts printed in table S2.1",
  "table_s2_1_occupations", "Occupation counts printed in table S2.1",
  "table_s2_1_inclusion", "Majority / minority inclusion flags printed in table S2.1"
) |>
  mutate(
    location = "table_S2.1",
    claim_type = "pipeline",
    value_paper = as.character(nrow(manifest_rows)),
    needs_block = TRUE,
    notes = "The ground truth compares how many of these cells the rewrite reproduces"
  )

# Prose, the two published tables, and the front matter ----
# Every one of these is typed from the published page. value_paper carries the string the page
# prints, including its decimals, since that is the precision any comparison has to be made at.
prose_claims <- tribble(
  ~claim_id, ~location, ~claim_type, ~value_paper, ~needs_block, ~claim, ~notes,

  # Abstract
  "abstract_since_year", "Abstract", "pipeline", "1983", TRUE,
    "Year the audit experimental record on gender begins", "",
  "abstract_experiments", "Abstract", "structural", "70", FALSE,
    "Employment audit experiments conducted since 1983",
    "A property of the literature the search identified, not of the reanalysed data",
  "abstract_countries", "Abstract", "descriptive", "26", TRUE,
    "Countries the experiments were carried out in",
    "Classed with the continent count beside it in the same clause rather than with the study count, which is a property of the literature the search identified and is repeated verbatim in two later sections. The deposit records the countries of the 57 studies reanalysed and not those of the wider literature, so the ground truth records both numbers and returns no verdict",
  "abstract_continents", "Abstract", "descriptive", "5", TRUE,
    "Continents the experiments were carried out across", "",

  # Introduction
  "intro_riach_resumes", "Introduction, p. 1", "transcribed", "1982", FALSE,
    "Resumes sent by Riach and Rich (8)", "A fact about the cited study",
  "intro_riach_openings", "Introduction, p. 1", "transcribed", "991", FALSE,
    "Job openings applied to by Riach and Rich (8)", "A fact about the cited study",
  "intro_riach_year", "Introduction, p. 1", "transcribed", "1986", TRUE,
    "Year Riach and Rich (8) sent their applications", "",
  "intro_riach_ate", "Introduction, p. 1", "pipeline", "-2.4", TRUE,
    "Riach and Rich (8) effect of being a woman, in percentage points", "",
  "intro_riach_se", "Introduction, p. 1", "pipeline", "1.2", TRUE,
    "Riach and Rich (8) cluster-robust standard error", "",
  "intro_neumark_applicants", "Introduction, p. 1", "transcribed", "130", FALSE,
    "Applicants in Neumark et al. (9)", "A fact about the cited study",
  "intro_neumark_jobs", "Introduction, p. 1", "transcribed", "65", FALSE,
    "Restaurant jobs in Neumark et al. (9)", "A fact about the cited study",
  "intro_neumark_year", "Introduction, p. 1", "transcribed", "1994", TRUE,
    "Year Neumark et al. (9) was conducted", "",
  "intro_neumark_ate", "Introduction, p. 1", "pipeline", "-12.3", TRUE,
    "Neumark et al. (9) effect of being a woman, in percentage points", "",
  "intro_neumark_se", "Introduction, p. 1", "pipeline", "8.4", TRUE,
    "Neumark et al. (9) standard error", "",
  "intro_literature_studies", "Introduction, p. 1", "structural", "70", FALSE,
    "Employment audit studies randomizing applicant gender conducted in total",
    "A property of the literature the search identified",
  "intro_studies", "Introduction, p. 1", "pipeline", "57", TRUE,
    "Studies whose data were obtained or reconstructed", "",
  "intro_papers", "Introduction, p. 1", "structural", "51", TRUE,
    "Papers reporting the 57 studies", "",
  "intro_positive", "Introduction, p. 1", "pipeline", "37", TRUE,
    "Studies showing a positive effect of being a woman", "",
  "intro_positive_of", "Introduction, p. 1", "pipeline", "57", TRUE,
    "Studies the 37 positive effects are counted out of", "",
  "intro_two_thirds", "Introduction, p. 1", "descriptive", "0.667", TRUE,
    "Share of studies with a positive effect, described as two-thirds",
    "A spelled-out number; no token scan sees it",
  "intro_meta_ate", "Introduction, p. 1", "pipeline", "1.2", TRUE,
    "Random-effects meta-analytic average treatment effect, in percentage points", "",
  "intro_meta_se", "Introduction, p. 1", "pipeline", "0.4", TRUE,
    "Standard error of the meta-analytic average treatment effect", "",

  # Theory
  "theory_composition_min", "Theory, p. 2", "definitional", "0", TRUE,
    "Gender composition of a fully men-dominated occupation", "",
  "theory_composition_max", "Theory, p. 2", "definitional", "1", TRUE,
    "Gender composition of a fully women-dominated occupation", "",
  "theory_parity", "Theory, p. 2", "definitional", "50", FALSE,
    "Gender composition an employer correcting an imbalance is said to aim at, in per cent",
    "A description of hiring policy, not a quantity the deposit records",
  "theory_yougov", "Theory, p. 3", "transcribed", "37", FALSE,
    "Share of men in a 2019 YouGov poll who believe diversity efforts disadvantage males, in per cent",
    "Copied from the cited poll",
  "theory_yougov_year", "Theory, p. 3", "transcribed", "2019", FALSE,
    "Year of the YouGov poll", "Copied from the cited poll",
  "theory_pew", "Theory, p. 3", "transcribed", "28", FALSE,
    "Share of men in a 2020 Pew poll saying women's gains came at their expense, in per cent",
    "Copied from the cited poll",
  "theory_pew_year", "Theory, p. 3", "transcribed", "2020", FALSE,
    "Year of the Pew Research poll", "Copied from the cited poll",
  "theory_range_upper", "Theory, p. 3", "definitional", "50", FALSE,
    "Upper end of the gender composition range over which popular discourse expects a negative gradient, in per cent",
    "A statement about the claim being characterised, not about the data",

  # Mechanisms of gender discrimination
  "mech_papers", "Mechanisms, p. 4", "structural", "51", TRUE,
    "Papers whose theory sections were coded for mechanisms", "",
  "mech_studies", "Mechanisms, p. 4", "structural", "57", TRUE,
    "Studies the 51 coded papers encompass", "",
  "mech_categories", "Mechanisms, p. 4", "structural", "5", TRUE,
    "Broad mechanism categories the papers were coded into",
    "A spelled-out number; the coding is not in the deposit",
  "mech_sixth", "Mechanisms, p. 4", "structural", "6", TRUE,
    "Category index of the residual mechanism category",
    "A spelled-out number; the coding is not in the deposit",
  "mech_theory_groups", "Mechanisms, p. 4", "structural", "5", TRUE,
    "Groups of theories said to be represented in multiple disciplines",
    "A spelled-out number; the coding is not in the deposit",
  "mech_economics", "Mechanisms, p. 4", "structural", "24", TRUE,
    "Included studies published in economics journals",
    "Printed as a parenthetical, which a citation filter drops",
  "mech_sociology", "Mechanisms, p. 4", "structural", "14", TRUE,
    "Included studies published in sociology journals",
    "Printed as a parenthetical, which a citation filter drops",
  "mech_other_journals", "Mechanisms, p. 4", "structural", "13", TRUE,
    "Included studies published in interdisciplinary, psychology or management science journals",
    "Printed as a parenthetical, which a citation filter drops",

  # Design
  "design_prisma_papers", "Design, p. 5", "structural", "70", TRUE,
    "Papers the PRISMA search process is said to have yielded that met the inclusion criteria", "",
  "design_microdata_papers", "Design, p. 5", "structural", "51", TRUE,
    "Papers whose microdata were obtained or reconstructed", "",
  "design_studies", "Design, p. 5", "pipeline", "57", TRUE,
    "Separate audit studies the 51 papers give", "",
  "design_prisma_figure", "Design, p. 5", "structural", "66", TRUE,
    "Running index of the supplementary figure the article cites for the PRISMA flow diagram",
    "The article prints fig. S6.66; the supplement numbers its figures section.index and its last is S6.63",
  "design_evidence_map_figure", "Design, p. 5", "structural", "2", TRUE,
    "Running index of the supplementary figure the article cites for the evidence map",
    "Printed as fig. S.2.2, with a stray period the figure itself does not carry",
  "design_boom_year", "Design, p. 5", "descriptive", "2005", TRUE,
    "Year around which the explosion in audit experimentation began", "",
  "design_isco_version", "Design, p. 5", "definitional", "08", FALSE,
    "ISCO classification version used to code occupations",
    "A property of the coding scheme, not a quantity in the deposit",
  "design_isco_digits", "Design, p. 5", "definitional", "2", FALSE,
    "Digits of the ISCO submajor group level",
    "A spelled-out number; a property of the coding scheme",
  "design_zero_callback", "Answer strategy, p. 6", "structural", "5", TRUE,
    "Occupation settings in which no applicant of either gender received a callback",
    "A spelled-out number; the deposit carries no callback rates",
  "design_s4_studies", "Answer strategy, p. 6", "pipeline", "57", TRUE,
    "Studies whose CATE estimates section S4 presents", "",

  # Results
  "results_ci_level", "Results, p. 6", "definitional", "95", TRUE,
    "Confidence level of the region overlaid on fig. 4, in per cent", "",
  "results_gradient", "Results, p. 6", "pipeline", "8.15", TRUE,
    "Gender gradient, quoted from column 1 of Table 1 into the text", "",
  "results_gradient_se", "Results, p. 6", "pipeline", "1.50", TRUE,
    "Standard error of the gender gradient, quoted into the text", "",
  "results_cate_0", "Results, p. 6", "pipeline", "-3.02", TRUE,
    "Expected CATE in an occupation with no women, in percentage points", "",
  "results_cate_0_restated", "Results, p. 6", "pipeline", "-3.02", TRUE,
    "The same intercept, restated inside the arithmetic of the following clause", "",
  "results_gradient_restated", "Results, p. 6", "pipeline", "8.15", TRUE,
    "The same gradient, restated inside the arithmetic of the following clause", "",
  "results_cate_100", "Results, p. 6", "pipeline", "5.13", TRUE,
    "Expected CATE in an occupation with only women, in percentage points", "",
  "results_magnitude_difference", "Results, p. 6", "pipeline", "2.10", TRUE,
    "Difference in the magnitude of the two predicted biases, in percentage points", "",
  "results_magnitude_se", "Results, p. 6", "pipeline", "0.58", TRUE,
    "Bootstrapped standard error of that difference", "",
  "results_fe_first_column", "Results, p. 6", "structural", "2", TRUE,
    "First column of Table 1 carrying fixed effects", "",
  "results_fe_last_column", "Results, p. 6", "structural", "5", TRUE,
    "Last column of Table 1 carrying fixed effects", "",

  # Discussion
  "disc_decades", "Discussion, p. 7", "descriptive", "4", TRUE,
    "Decades of audit experimentation the paper synthesizes",
    "A spelled-out number: over the past four decades",
  "disc_gradient_studies", "Discussion, p. 7", "pipeline", "37", TRUE,
    "Studies with a study-specific gender gradient estimate", "",
  "disc_significant", "Discussion, p. 7", "pipeline", "7", TRUE,
    "Study-specific gender gradients reaching statistical significance", "",
  "disc_power_numerator", "Discussion, p. 7", "pipeline", "7", TRUE,
    "Numerator of the power ratio 7/37", "",
  "disc_power_denominator", "Discussion, p. 7", "pipeline", "37", TRUE,
    "Denominator of the power ratio 7/37", "",
  "disc_power", "Discussion, p. 7", "pipeline", "19", TRUE,
    "Statistical power of the average audit study to detect the gender gradient, in per cent", "",
  "disc_literature_studies", "Discussion, p. 8", "structural", "70", FALSE,
    "Audit studies the record currently stands at",
    "A property of the literature the search identified",
  "disc_synthesized", "Discussion, p. 8", "pipeline", "57", TRUE,
    "Studies synthesized here", "",
  "disc_since_year", "Discussion, p. 8", "descriptive", "2005", TRUE,
    "Year since which most studies were conducted in North America and Europe", "",

  # Figure and table notes
  "fig4_note_column", "Fig. 4 caption, p. 6", "structural", "1", TRUE,
    "Column of Table 1 the fig. 4 regression line is derived from", "",
  "fig5_note_column_1", "Fig. 5 caption, p. 8", "structural", "1", TRUE,
    "First column of Table 2 the fig. 5 regression lines are derived from", "",
  "fig5_note_column_2", "Fig. 5 caption, p. 8", "structural", "2", TRUE,
    "Second column of Table 2 the fig. 5 regression lines are derived from", "",
  "table_1_note_p05", "Table 1 note, p. 7", "definitional", "0.05", TRUE,
    "One-star significance threshold in Table 1", "",
  "table_1_note_p01", "Table 1 note, p. 7", "definitional", "0.01", TRUE,
    "Two-star significance threshold in Table 1", "",
  "table_1_note_p001", "Table 1 note, p. 7", "definitional", "0.001", TRUE,
    "Three-star significance threshold in Table 1", "",
  "table_2_note_p05", "Table 2 note, p. 8", "definitional", "0.05", TRUE,
    "One-star significance threshold in Table 2", "Restates the Table 1 note verbatim",
  "table_2_note_p01", "Table 2 note, p. 8", "definitional", "0.01", TRUE,
    "Two-star significance threshold in Table 2", "Restates the Table 1 note verbatim",
  "table_2_note_p001", "Table 2 note, p. 8", "definitional", "0.001", TRUE,
    "Three-star significance threshold in Table 2",
    "Restates the Table 1 note verbatim. A quantity stated twice is checked twice, so it gets its own block",

  # Supplementary front matter and section prose
  "supp_first_figure", "Supplementary front matter", "structural", "1", TRUE,
    "Running index of the first figure the supplementary front matter lists",
    "Printed as S1.1: section 1, figure 1",
  "supp_last_figure", "Supplementary front matter", "structural", "63", TRUE,
    "Running index of the last figure the supplementary front matter lists",
    "Printed as S6.63: section 6, figure 63",
  "supp_first_section", "Supplementary front matter", "structural", "1", FALSE,
    "First supplementary section number", "A property of the document's own numbering",
  "supp_last_section", "Supplementary front matter", "structural", "6", FALSE,
    "Last supplementary section number", "A property of the document's own numbering",
  "supp_tables", "Supplementary front matter", "structural", "1", TRUE,
    "Tables in the supplementary materials", "",
  "s1_studies", "Section S1", "pipeline", "57", TRUE,
    "Studies in the fig. S1.1 forest plot", "",
  "s1_meta_ate", "Section S1", "pipeline", "1.2", TRUE,
    "Meta-analytic average effect of being a woman, in percentage points", "",
  "s1_meta_se", "Section S1", "pipeline", "0.4", TRUE,
    "Standard error of the meta-analytic average", "",
  "s2_early_1980s", "Section S2", "descriptive", "1", TRUE,
    "Studies in the evidence map in the 1980s",
    "A spelled-out number: one early study in Australia in the 1980s",
  "s2_early_1990s", "Section S2", "descriptive", "1", TRUE,
    "Studies in the evidence map in the 1990s",
    "A spelled-out number: one study in the United States in the 1990s",
  "s2_boom_year", "Section S2", "descriptive", "2005", TRUE,
    "Year around which the evidence map shows a boom", "",
  "s3_survey_experiments", "Section S3", "pipeline", "12", TRUE,
    "Vignette survey experiments collected", "",
  "s3_scale_min", "Section S3", "definitional", "0", FALSE,
    "Lowest value of the hiring likelihood rating scale",
    "The deposit carries the dichotomized outcome, not the rating",
  "s3_scale_max", "Section S3", "definitional", "10", FALSE,
    "Highest value of the hiring likelihood rating scale",
    "The deposit carries the dichotomized outcome, not the rating",
  "s4_minimum_cates", "Section S4", "definitional", "3", TRUE,
    "Occupation CATEs a study needs before a gender gradient is estimated", "",
  "s4_gradients", "Section S4", "pipeline", "37", TRUE,
    "Gender gradients estimated across the 57 studies", "",
  "s5_gradients", "Section S5", "pipeline", "37", TRUE,
    "Study-by-study gender gradient estimates meta-analysed in section S5", "",
  "s5_model_reference", "Section S5", "structural", "5", TRUE,
    "Table 1 model the gender gradient meta-analysis is compared to", "",
  "s5_minimum_occupations", "Section S5", "definitional", "3", TRUE,
    "Occupations a study needs to enter the gender gradient meta-analysis", "",
  "s5_fixed_effects_minimum", "Section S5", "definitional", "2", TRUE,
    "Occupations a study needs to enter the study fixed effects estimation", "",
  "s5_studies", "Section S5", "pipeline", "37", TRUE,
    "Studies with three or more occupations", "",
  "s5_meta_gradient", "Section S5", "pipeline", "9.0", TRUE,
    "Meta-analytic average gender gradient", "",
  "s5_meta_se", "Section S5", "pipeline", "2.1", TRUE,
    "Standard error of the meta-analytic average gender gradient", "",
  "s5_significant", "Section S5", "pipeline", "7", TRUE,
    "Gender gradient estimates that are statistically significant", "",
  "s5_significant_of", "Section S5", "pipeline", "37", TRUE,
    "Gender gradient estimates the seven significant ones are counted out of", "",

  # Section S6, the PRISMA flow diagram. Every one of these is a count from the screening
  # process. Nothing in the deposit records the search, so none of them has a block.
  "s6_prisma_year", "Section S6", "structural", "2020", FALSE,
    "PRISMA template version used", "A property of the reporting standard",
  "s6_identified", "Fig. S6.63", "structural", "439", FALSE,
    "Total number of studies identified", "Search process; not in the deposit",
  "s6_databases", "Fig. S6.63", "structural", "238", FALSE,
    "Records identified from databases", "Search process; not in the deposit",
  "s6_prior_gender", "Fig. S6.63", "structural", "82", FALSE,
    "Records from previous meta-analyses focused on gender discrimination",
    "Search process; not in the deposit",
  "s6_prior_ethnic", "Fig. S6.63", "structural", "119", FALSE,
    "Records from previous meta-analyses focused on ethnic or racial discrimination",
    "Search process; not in the deposit",
  "s6_duplicates", "Fig. S6.63", "structural", "210", FALSE,
    "Duplicate records removed", "Search process; not in the deposit",
  "s6_automation", "Fig. S6.63", "structural", "0", FALSE,
    "Records marked ineligible by automation tools", "Search process; not in the deposit",
  "s6_other_removed", "Fig. S6.63", "structural", "0", FALSE,
    "Records removed for other reasons", "Search process; not in the deposit",
  "s6_screened", "Fig. S6.63", "structural", "229", FALSE,
    "Records screened against the eligibility criteria", "Search process; not in the deposit",
  "s6_excluded", "Fig. S6.63", "structural", "140", FALSE,
    "Records excluded at screening", "Search process; not in the deposit",
  "s6_sought", "Fig. S6.63", "structural", "89", FALSE,
    "Reports sought for retrieval", "Search process; not in the deposit",
  "s6_not_retrieved", "Fig. S6.63", "structural", "0", FALSE,
    "Reports not retrieved", "Search process; not in the deposit",
  "s6_assessed", "Fig. S6.63", "structural", "89", FALSE,
    "Reports assessed for eligibility", "Search process; not in the deposit",
  "s6_excl_not_randomized", "Fig. S6.63", "structural", "16", FALSE,
    "Reports excluded because gender was not randomized", "Search process; not in the deposit",
  "s6_excl_duplicate_experiment", "Fig. S6.63", "structural", "5", FALSE,
    "Reports excluded as the same experiment in multiple papers",
    "Search process; not in the deposit",
  "s6_excl_outcome", "Fig. S6.63", "structural", "2", FALSE,
    "Reports excluded for a different outcome variable", "Search process; not in the deposit",
  "s6_excl_data", "Fig. S6.63", "structural", "12", FALSE,
    "Reports excluded because the data could not be obtained",
    "Search process; not in the deposit",
  "s6_excl_unpublished", "Fig. S6.63", "structural", "3", FALSE,
    "Reports excluded as unpublished", "Search process; not in the deposit",
  "s6_included_papers", "Fig. S6.63", "structural", "51", TRUE,
    "Reports included in the review", "",
  "s6_included_studies", "Fig. S6.63", "structural", "57", TRUE,
    "Separate audit studies included in the review", "",

  # Descriptive claims. Where the article makes the claim in words with no number, value_paper
  # is empty and the ground truth records the evidence with no verdict; where the claim reduces
  # to a truth value, value_paper is 1, recording that the article asserts it.
  "abstract_mixed_signs", "Abstract", "descriptive", "1", TRUE,
    "Study-level effects run in both directions",
    "'The results are mixed: Some studies find discrimination against men, and others find discrimination against women'",
  "results_gradient_stable", "Results, p. 6", "descriptive", "", TRUE,
    "Gender gradient remains quite stable across the five Table 1 models",
    "No number is printed, so the spread across the five models is recorded with no verdict",
  "results_minority_steeper", "Results, p. 6", "descriptive", "1", TRUE,
    "Gender gradient steeper for minority than for majority applicants", "",
  "results_interaction_not_significant", "Results, p. 6", "descriptive", "1", TRUE,
    "Majority by gradient interaction not significant at the 0.05 level", "",
  "disc_gradient_positive", "Discussion, p. 7", "descriptive", "1", TRUE,
    "Pooled gender gradient is positive", "",
  "disc_most_europe_north_america", "Discussion, p. 8", "descriptive", "1", TRUE,
    "Most studies were conducted in North America and Europe", "",
  "disc_other_continents", "Discussion, p. 8", "descriptive", "", TRUE,
    "Only a handful of studies in South America, Asia or Africa",
    "No number is printed, so the count is recorded with no verdict",
  "s3_gradients_similar", "Section S3", "descriptive", "", TRUE,
    "Survey and field gender gradients are very similar",
    "No number is printed, so the distance between the two fits is recorded with no verdict",
  "s5_similar_to_table_1", "Section S5", "descriptive", "", TRUE,
    "Gradient meta-analysis is quite similar to the Table 1 estimates",
    "No number is printed, so the distance from the Table 1 model 1 gradient is recorded with no verdict",

  # Coverage of the section S4 panels, which is what the ground truth records for the deposit
  "s4_panels_published", "Figs. S4.5 to S4.61", "structural", "57", TRUE,
    "Study panels the published appendix draws", "",
  # 334 rather than 335: fig. S4.7's gradient label is clipped out of its panel, so the
  # page does not print it. See clipped_runs at the top of this file.
  "s4_panel_entries_published", "Figs. S4.5 to S4.61", "structural", "334", TRUE,
    "Estimate and standard error labels printed across the 57 published panels", ""
)

# Tables 1 and 2 ----
# Typed from the typeset pages, three claims per coefficient (the estimate, its standard error
# and its significance marker) and two per model. The marker is recorded as the number of stars,
# which is what the ground truth compares.
table_1_cells <- tribble(
  ~model, ~term, ~estimate, ~std_error, ~stars, ~tau_squared, ~nobs,
  "model1", "intercept", "-3.02", "0.81", "3", "12.57", "298",
  "model1", "gradient", "8.15", "1.50", "3", "12.57", "298",
  "model2", "intercept", "-2.48", "1.58", "0", "11.71", "298",
  "model2", "gradient", "8.33", "1.58", "3", "11.71", "298",
  "model3", "intercept", "-5.43", "1.91", "2", "12.56", "298",
  "model3", "gradient", "8.69", "1.56", "3", "12.56", "298",
  "model4", "intercept", "-5.20", "1.82", "2", "10.22", "298",
  "model4", "gradient", "7.97", "1.57", "3", "10.22", "298",
  "model5", "intercept", "1.05", "1.96", "0", "8.89", "298",
  "model5", "gradient", "7.51", "1.54", "3", "8.89", "298"
)

table_2_cells <- tribble(
  ~model, ~term, ~estimate, ~std_error, ~stars, ~tau_squared, ~nobs,
  "model1", "intercept", "-1.55", "1.31", "0", "6.60", "91",
  "model1", "gradient", "5.70", "2.50", "1", "6.60", "91",
  "model2", "intercept", "-3.72", "1.13", "0", "6.28", "96",
  "model2", "gradient", "9.08", "2.26", "3", "6.28", "96",
  "model3", "intercept", "-3.73", "1.14", "2", "6.44", "187",
  "model3", "gradient", "9.11", "2.28", "3", "6.44", "187",
  "model3", "majority", "2.19", "1.74", "0", "6.44", "187",
  "model3", "majority_x_gradient", "-3.42", "3.37", "0", "6.44", "187"
)

term_labels <- c(intercept = "Intercept", gradient = "Gender gradient",
                 majority = "Majority status", majority_x_gradient = "Majority x gradient")

table_coefficient_claims <- function(cells, stem, table_name) {
  cells |>
    pivot_longer(c(estimate, std_error, stars), names_to = "part", values_to = "value_paper") |>
    transmute(
      claim_id = str_glue("{stem}_{model}_{term}_{part}"),
      location = str_glue("{stem}_{model}"),
      claim_type = "pipeline",
      value_paper,
      needs_block = TRUE,
      claim = case_when(
        part == "estimate" ~ str_glue("{table_name} {model}, {term_labels[term]}"),
        part == "std_error" ~ str_glue("{table_name} {model}, {term_labels[term]} standard error"),
        .default = str_glue("{table_name} {model}, {term_labels[term]} significance stars")
      ),
      notes = ""
    )
}

table_gof_claims <- function(cells, stem, table_name) {
  cells |>
    distinct(model, tau_squared, nobs) |>
    pivot_longer(c(tau_squared, nobs), names_to = "part", values_to = "value_paper") |>
    transmute(
      claim_id = str_glue("{stem}_{model}_{part}"),
      location = str_glue("{stem}_{model}"),
      claim_type = "pipeline",
      value_paper,
      needs_block = TRUE,
      claim = str_glue("{table_name} {model}, ",
                       "{if_else(part == 'nobs', 'observations', 'tau squared')}"),
      notes = ""
    )
}

table_claims <- bind_rows(
  table_coefficient_claims(table_1_cells, "table_1", "Table 1"),
  table_gof_claims(table_1_cells, "table_1", "Table 1"),
  table_coefficient_claims(table_2_cells, "table_2", "Table 2"),
  table_gof_claims(table_2_cells, "table_2", "Table 2")
)

# Assemble ----
published_claims <-
  bind_rows(prose_claims, table_claims, manifest_claims, forest_claims, panel_claims) |>
  mutate(across(everything(), as.character),
         needs_block = needs_block == "TRUE")

stopifnot(!anyDuplicated(published_claims$claim_id),
          !any(is.na(published_claims$value_paper)),
          all(published_claims$claim_type %in%
                c("pipeline", "descriptive", "definitional", "structural", "transcribed")))

write_csv(published_claims, here::here("ground_truth", "published_claims.csv"))

print(count(published_claims, claim_type, needs_block))
print(str_glue("{nrow(published_claims)} claims, ",
               "{sum(published_claims$needs_block)} of which need a block"))
