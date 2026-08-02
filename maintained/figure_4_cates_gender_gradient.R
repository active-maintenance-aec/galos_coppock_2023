# galos_coppock_2023/maintained/figure_4_cates_gender_gradient.R
# Output: output/figure_4_cates_gender_gradient.pdf, .png, .csv
# Depends on: helpers.R
# Description: Occupation-level CATEs against gender composition, with the Table 1 model 1
#   meta-regression line and its 95 percent confidence region (Fig. 4).

source(here::here("maintained", "helpers.R"))

occupation_cates <- read_rds(file.path(data_dir, "occupation_cates.rds")) |> ungroup()

meta_fit <- rma.uni(
  yi = estimate,
  sei = std.error,
  mods = ~ fraction_female,
  data = occupation_cates
)

occupation_cates$weight <- weights(meta_fit)

preds_df <- tidy_predictions(meta_fit)

g <- ggplot(occupation_cates) +
  aes(
    fraction_female,
    estimate,
    color = occupation_gender_distribution,
    shape = occupation_gender_distribution
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.35) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), alpha = 0.25) +
  geom_point(alpha = 0.5, aes(size = weight), stroke = 0) +
  geom_ribbon(
    data = preds_df,
    aes(ymin = conf.low, ymax = conf.high, color = NULL, shape = NULL),
    alpha = 0.55
  ) +
  geom_line(data = preds_df, alpha = 0.55, aes(color = NULL, shape = NULL)) +
  coord_cartesian(xlim = c(0, 1), ylim = c(-25, 25)) +
  scale_size_continuous(range = c(1, 4)) +
  scale_y_continuous(breaks = seq(-10, 10, 5)) +
  scale_x_continuous(label = percent_format()) +
  scale_color_brewer(palette = "Set2") +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none"
  ) +
  labs(
    x = "Status quo gender composition (share of women in occupation)",
    y = "Conditional average treatment effect"
  )

ggsave(file.path(out_dir, "figure_4_cates_gender_gradient.pdf"), plot = g, width = 7, height = 5)
ggsave(file.path(out_dir, "figure_4_cates_gender_gradient.png"), plot = g, width = 7, height = 5, dpi = 300)

# The figure prints no numbers, so the plotted values are written out instead: the 298
# points and the 100 evaluations of the fitted line and its confidence region.
bind_rows(
  point = occupation_cates |>
    select(study_id, occupation_observed, fraction_female, estimate, std.error,
           conf.low, conf.high, weight),
  fit = preds_df |>
    select(fraction_female, estimate, std.error, conf.low, conf.high),
  .id = "layer"
) |>
  write_csv(file.path(out_dir, "figure_4_cates_gender_gradient.csv"))
