# galos_coppock_2023/maintained/figure_s3_3_survey_field_comparison.R
# Output: output/figure_s3_3_survey_field_comparison.pdf, .png, .csv
# Depends on: helpers.R
# Description: Gender gradient estimated separately from the survey and field experimental
#   records, plotted on common axes (fig. S3.3).

source(here::here("maintained", "helpers.R"))

survey_df <- read_rds(file.path(data_dir, "survey_occupation_cates.rds")) |> ungroup()
field_df <- read_rds(file.path(data_dir, "occupation_cates.rds")) |> ungroup()

field_fit <- rma.uni(yi = estimate, sei = std.error, mods = ~ fraction_female, data = field_df)
survey_fit <- rma.uni(yi = estimate, sei = std.error, mods = ~ fraction_female, data = survey_df)

field_df$weight <- weights(field_fit)
survey_df$weight <- weights(survey_fit)

gg_df <- bind_rows(
  `Field experiments` = field_df,
  `Survey experiments` = survey_df,
  .id = "experiment_type"
) |>
  mutate(
    experiment_type = factor(experiment_type,
                             levels = c("Survey experiments", "Field experiments"))
  )

preds_df <- bind_rows(
  `Field experiments` = tidy_predictions(field_fit),
  `Survey experiments` = tidy_predictions(survey_fit),
  .id = "experiment_type"
) |>
  mutate(
    experiment_type = factor(experiment_type,
                             levels = c("Survey experiments", "Field experiments"))
  )

g <- ggplot(gg_df, aes(fraction_female, estimate,
                       color = experiment_type, group = experiment_type,
                       shape = experiment_type)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.35) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) +
  geom_point(alpha = 0.2, aes(size = weight), stroke = 0) +
  geom_ribbon(
    data = preds_df,
    aes(ymin = conf.low, ymax = conf.high, fill = experiment_type),
    alpha = 0.35
  ) +
  geom_line(data = preds_df, alpha = 0.55) +
  coord_cartesian(xlim = c(0, 1), ylim = c(-25, 25)) +
  scale_size_continuous(range = c(1, 4), guide = "none") +
  scale_y_continuous(breaks = seq(-10, 10, 5)) +
  scale_x_continuous(label = percent_format()) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.85, 0.2),
    legend.background = element_rect(fill = "transparent"),
    legend.title = element_blank(),
    text = element_text(size = 8)
  ) +
  labs(
    x = "Status quo gender composition (share of women)",
    y = "Conditional average treatment effect"
  )

ggsave(file.path(out_dir, "figure_s3_3_survey_field_comparison.pdf"), plot = g, width = 7, height = 5)
ggsave(file.path(out_dir, "figure_s3_3_survey_field_comparison.png"), plot = g, width = 7, height = 5, dpi = 300)

# The two fitted lines are the point of the figure; write their coefficients out so the
# claim that the gradient is "very similar" across modes can be checked.
bind_rows(
  `Field experiments` = tidy(field_fit, conf.int = TRUE),
  `Survey experiments` = tidy(survey_fit, conf.int = TRUE),
  .id = "experiment_type"
) |>
  select(experiment_type, term, estimate, std.error, statistic, p.value, conf.low, conf.high) |>
  write_csv(file.path(out_dir, "figure_s3_3_survey_field_comparison.csv"))
