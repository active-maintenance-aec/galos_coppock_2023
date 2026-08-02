# galos_coppock_2023/maintained/figure_5_cates_ethnicity.R
# Output: output/figure_5_cates_ethnicity.pdf, .png, .csv
# Depends on: helpers.R
# Description: Occupation-level CATEs against gender composition, separately for majority and
#   minority status applicants, with the Table 2 models 1 and 2 regression lines (Fig. 5).

source(here::here("maintained", "helpers.R"))

occupation_cates <- read_rds(file.path(data_dir, "occupation_cates_ethnicity.rds")) |> ungroup()

minority_df <- filter(occupation_cates, ethnicity_fct == "Minority")
majority_df <- filter(occupation_cates, ethnicity_fct == "Majority")

minority_fit <- rma.uni(yi = estimate, sei = std.error, mods = ~ fraction_female, data = minority_df)
majority_fit <- rma.uni(yi = estimate, sei = std.error, mods = ~ fraction_female, data = majority_df)

minority_df$weight <- weights(minority_fit)
majority_df$weight <- weights(majority_fit)

gg_df <- bind_rows(
  "Minority" = minority_df,
  "Majority" = majority_df,
  .id = "ethnicity_fct"
)

preds_df <- bind_rows(
  "Minority" = tidy_predictions(minority_fit),
  "Majority" = tidy_predictions(majority_fit),
  .id = "ethnicity_fct"
)

g <- ggplot(gg_df, aes(fraction_female, estimate,
                       color = ethnicity_fct, group = ethnicity_fct, shape = ethnicity_fct)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.35) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) +
  geom_point(alpha = 0.2, aes(size = weight), stroke = 0) +
  geom_ribbon(
    data = preds_df,
    aes(ymin = conf.low, ymax = conf.high, fill = ethnicity_fct),
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
    legend.position.inside = c(0.875, 0.2),
    legend.background = element_rect(fill = "transparent"),
    legend.title = element_blank()
  ) +
  labs(
    x = "Status quo gender composition (share of women)",
    y = "Conditional average treatment effect"
  )

ggsave(file.path(out_dir, "figure_5_cates_ethnicity.pdf"), plot = g, width = 7, height = 5)
ggsave(file.path(out_dir, "figure_5_cates_ethnicity.png"), plot = g, width = 7, height = 5, dpi = 300)

# The plotted values, since the figure itself prints none: the 187 points and the two
# fitted lines evaluated across the range of gender composition.
bind_rows(
  point = gg_df |>
    select(ethnicity_fct, study_id, occupation_observed, fraction_female, estimate,
           std.error, conf.low, conf.high, weight),
  fit = preds_df |>
    select(ethnicity_fct, fraction_female, estimate, std.error, conf.low, conf.high),
  .id = "layer"
) |>
  write_csv(file.path(out_dir, "figure_5_cates_ethnicity.csv"))
