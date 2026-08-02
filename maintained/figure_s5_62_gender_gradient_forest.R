# galos_coppock_2023/maintained/figure_s5_62_gender_gradient_forest.R
# Output: output/figure_s5_62_gender_gradient_forest.pdf, .png, .csv
# Depends on: clean_gender_gradients.R output, helpers.R
# Description: Forest plot of the 37 study-level gender gradients and their meta-analysis (fig. S5.62).

source(here::here("maintained", "helpers.R"))

gender_gradients <- read_csv(file.path(out_dir, "gender_gradients.csv"), show_col_types = FALSE)

meta_fit <- rma.uni(yi = estimate, sei = std.error, method = "REML", data = gender_gradients)

gender_gradients$weight <- weights(meta_fit)

meta_ests <- tidy(meta_fit, conf.int = TRUE) |>
  mutate(
    study_id = "Meta-analysis",
    study_name_citation = "Meta-analysis",
    facet = "Meta",
    weight = mean(gender_gradients$weight)
  )

gg_df <- bind_rows(mutate(gender_gradients, facet = "Study gender gradient estimates"), meta_ests) |>
  mutate(
    study_name_fct = fct_reorder(study_name_citation, estimate),
    entry = make_se_entry(estimate, std.error, 1),
    facet = factor(facet, levels = c("Study gender gradient estimates", "Meta"))
  )

g <- ggplot(gg_df, aes(estimate, study_name_fct)) +
  geom_point(aes(size = weight)) +
  geom_text(x = -80, aes(label = entry), hjust = 1, size = 2.5) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high), alpha = 0.3) +
  geom_vline(xintercept = 0, alpha = 0.25, linetype = "dashed") +
  geom_vline(data = select(meta_ests, -facet), aes(xintercept = estimate)) +
  geom_rect(
    data = select(meta_ests, -facet),
    aes(xmin = conf.low, xmax = conf.high, x = NULL, y = NULL),
    ymin = 0, ymax = 100, alpha = 0.1
  ) +
  scale_size_area(max_size = 3) +
  theme_bw() +
  facet_grid(rows = vars(facet), scales = "free_y", space = "free") +
  coord_cartesian(xlim = c(-100, 100)) +
  theme(
    strip.background = element_blank(),
    strip.text = element_blank(),
    panel.grid = element_line(color = gray(0.95)),
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 5),
    legend.position = "none"
  ) +
  labs(x = "Estimated gender gradient")

ggsave(file.path(out_dir, "figure_s5_62_gender_gradient_forest.pdf"), plot = g, width = 7, height = 10)
ggsave(file.path(out_dir, "figure_s5_62_gender_gradient_forest.png"), plot = g, width = 7, height = 10, dpi = 300)

# Each estimate and SE is printed beside its point in the published figure.
gg_df |>
  arrange(facet, desc(estimate)) |>
  select(facet, study_id, study_name_citation, estimate, std.error, conf.low, conf.high, weight) |>
  write_csv(file.path(out_dir, "figure_s5_62_gender_gradient_forest.csv"))
