# galos_coppock_2023/run_all.R
# Runs the whole reproduction in order: fetch and verify the deposited archive, re-derive the
# study-level gender gradients, then every published table and figure, then the in-text numbers.
# Every script is self-contained apart from the two that read gender_gradients.csv.

library(here)
here::i_am("run_all.R")

# Deposited archive ----
# Downloads from Dataverse on a fresh clone; verifies checksums either way.
source(here::here("download_original.R"))

# Cleaning ----
# Re-derives what the archive ships as data/gender_gradients_df.rds. figure_s5 and
# text_summary_stats.R both read it, so it runs first.
source(here::here("maintained", "clean_gender_gradients.R"))

# Tables ----
source(here::here("maintained", "table_1_meta_regression.R"))
source(here::here("maintained", "table_2_ethnicity_meta_regression.R"))
source(here::here("maintained", "table_s2_1_study_manifest.R"))

# Figures ----
source(here::here("maintained", "figure_4_cates_gender_gradient.R"))
source(here::here("maintained", "figure_5_cates_ethnicity.R"))
source(here::here("maintained", "figure_s1_1_field_ates_forest.R"))
source(here::here("maintained", "figure_s2_2_evidence_map.R"))
source(here::here("maintained", "figure_s3_3_survey_field_comparison.R"))
source(here::here("maintained", "figure_s3_4_survey_ates_forest.R"))
source(here::here("maintained", "figure_s4_study_panels.R"))
source(here::here("maintained", "figure_s5_62_gender_gradient_forest.R"))

# In-text quantities ----
# text_difference_in_magnitude.R bootstraps the meta-regression 500 times and takes about
# two minutes; it is the slow step.
source(here::here("maintained", "text_summary_stats.R"))
source(here::here("maintained", "text_difference_in_magnitude.R"))

# Figure timestamps ----
# R's pdf() device stamps a wall-clock /CreationDate and /ModDate into every figure it
# writes, and those two fields are the only reason two runs of this pipeline produce
# differing files. Blanking them lets the determinism check cover every file the
# pipeline writes rather than all but the figures.
source(here::here("maintained", "helpers.R"))
walk(
  list.files(here::here("maintained", "output"), pattern = "\\.pdf$", full.names = TRUE),
  blank_pdf_timestamps
)

# Ground truth ----
# Joins the published values to what the scripts above wrote, so the comparison table is
# generated rather than transcribed. It has to run after everything it reads. Its gates run
# maintained/in_text_claims.R under capture.output and count what it printed, so the file below
# necessarily runs twice: once for the gate, once for the human-readable log.
source(here::here("ground_truth", "build_ground_truth.R"))

# In-text claims ----
# The second instrument. Every claim the article makes, recomputed from maintained/output/ by a
# separate path from the one the ground truth takes, printed with the article's own sentence
# beside it in the source.
source(here::here("maintained", "in_text_claims.R"))

# Deposited archive, again ----
# The check at the top of this file is a precondition: it says original/ was intact
# before anything ran. Nothing above writes to original/, and this second pass is what
# demonstrates it rather than assuming it. Nothing is downloaded; the files are already
# present and are re-checked against the manifest on checksum, byte size and membership.
source(here::here("download_original.R"))
