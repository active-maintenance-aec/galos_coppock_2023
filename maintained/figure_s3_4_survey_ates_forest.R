# galos_coppock_2023/maintained/figure_s3_4_survey_ates_forest.R
# Output: output/figure_s3_4_survey_ates_forest.pdf, .png, .csv
# Depends on: helpers.R
# Description: Forest plot of the 12 survey experiment ATEs and their meta-analysis (fig. S3.4).

source(here::here("maintained", "helpers.R"))

survey_studies <- read_rds(file.path(data_dir, "survey_studies.rds")) |> ungroup()

survey_meta_fit <- rma.uni(yi = estimate, sei = std.error, method = "REML", data = survey_studies)

survey_studies$weight <- weights(survey_meta_fit)

survey_meta_ests <- tidy(survey_meta_fit, conf.int = TRUE) |>
  mutate(
    study_id = "Meta-analysis",
    study_name_citation = "Meta-analysis",
    facet = "Meta",
    weight = mean(survey_studies$weight)
  )

gg_df <- bind_rows(survey_studies, survey_meta_ests) |>
  mutate(
    study_name_fct = fct_reorder(study_name_citation, estimate),
    entry = make_se_entry(estimate, std.error, 1),
    facet = factor(facet, levels = c("Study estimates", "Meta"))
  )

g <- ggplot(gg_df, aes(estimate, study_name_fct)) +
  geom_point(aes(size = weight)) +
  geom_text(x = -22, aes(label = entry), hjust = 1, size = 2.5) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high), alpha = 0.3) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  geom_vline(data = select(survey_meta_ests, -facet), aes(xintercept = estimate)) +
  # A fixed ymin/ymax on a discrete scale TRAINS that scale's continuous range in
  # ggplot2 4.0.3, so the authors' ymin = 0, ymax = 100 (written to run the band
  # past the rows and rely on the panel to clip it) makes the range 0-100 and
  # `space = "free"` sizes every panel alike. The rows then collapse into a
  # fraction of their panel. Infinite limits are not trained: the band still
  # fills the panel and the rows keep their own range. Same defect in
  # figure_s5_62_gender_gradient_forest.R and figure_s1_1_field_ates_forest.R.
  geom_rect(
    data = select(survey_meta_ests, -facet),
    aes(xmin = conf.low, xmax = conf.high, x = NULL, y = NULL),
    ymin = -Inf, ymax = Inf, alpha = 0.1
  ) +
  scale_size_area(max_size = 3) +
  theme_bw() +
  facet_grid(rows = vars(facet), scales = "free_y", space = "free") +
  coord_cartesian(xlim = c(-25, 25)) +
  theme(
    strip.background = element_blank(),
    strip.text = element_blank(),
    panel.grid = element_line(color = gray(0.95)),
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 5),
    legend.position = "none"
  ) +
  labs(x = "Estimated average treatment effect in percentage points")

# The authors' own canvas, appendix_survey_ates.R:104. A ggplot canvas is not a
# scale: the type holds its point size while the panel stretches, so a figure
# saved at another aspect reflows and stops being the published drawing.
ggsave(file.path(out_dir, "figure_s3_4_survey_ates_forest.pdf"), plot = g, width = 6.5, height = 3)
ggsave(file.path(out_dir, "figure_s3_4_survey_ates_forest.png"), plot = g, width = 6.5, height = 3, dpi = 300)

# Each estimate and SE is printed beside its point in the published figure.
gg_df |>
  arrange(facet, desc(estimate)) |>
  select(facet, study_name_citation, estimate, std.error, conf.low, conf.high, weight) |>
  write_csv(file.path(out_dir, "figure_s3_4_survey_ates_forest.csv"))
