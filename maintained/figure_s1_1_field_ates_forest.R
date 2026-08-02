# galos_coppock_2023/maintained/figure_s1_1_field_ates_forest.R
# Output: output/figure_s1_1_field_ates_forest.pdf, .png, .csv
# Depends on: helpers.R
# Description: Forest plot of the 57 field study ATEs and their random-effects meta-analysis (fig. S1.1).

source(here::here("maintained", "helpers.R"))

ates <- read_rds(file.path(data_dir, "estimated_ates.rds")) |> ungroup()

field_meta_fit <- rma.uni(yi = estimate, sei = std.error, method = "REML", data = ates)

ates$weight <- weights(field_meta_fit)

field_meta_ests <- tidy(field_meta_fit, conf.int = TRUE) |>
  mutate(
    study_id = "Meta-analysis",
    study_name_citation = "Meta-analysis",
    facet = "Meta",
    weight = mean(ates$weight)
  )

gg_df <- bind_rows(ates, field_meta_ests) |>
  mutate(
    study_name_fct = fct_reorder(study_name_citation, estimate),
    entry = make_se_entry(estimate, std.error, 1),
    facet = factor(facet, levels = c("Study estimates", "Meta"))
  )

g <- ggplot(gg_df, aes(estimate, study_name_fct)) +
  geom_point(aes(size = weight)) +
  geom_text(x = -22, aes(label = entry), hjust = 1, size = 2.5) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high), alpha = 0.3) +
  geom_vline(xintercept = 0, alpha = 0.25, linetype = "dashed") +
  geom_vline(data = select(field_meta_ests, -facet), aes(xintercept = estimate)) +
  geom_rect(
    data = select(field_meta_ests, -facet),
    aes(xmin = conf.low, xmax = conf.high, x = NULL, y = NULL),
    ymin = 0, ymax = 100, alpha = 0.1
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
    axis.text.y = element_text(size = 6),
    legend.position = "none"
  ) +
  labs(x = "Estimated average treatment effect in percentage points")

ggsave(file.path(out_dir, "figure_s1_1_field_ates_forest.pdf"), plot = g, width = 7, height = 10)
ggsave(file.path(out_dir, "figure_s1_1_field_ates_forest.png"), plot = g, width = 7, height = 10, dpi = 300)

# The figure prints each estimate and its SE beside the point, so all 58 plotted values
# are published numbers. Write them out so they can be checked against the appendix.
# Rows come out in the order the figure reads: study estimates descending, then the
# meta-analysis in its own facet at the foot.
gg_df |>
  arrange(facet, desc(estimate)) |>
  select(facet, study_name_citation, estimate, std.error, conf.low, conf.high, weight) |>
  write_csv(file.path(out_dir, "figure_s1_1_field_ates_forest.csv"))
