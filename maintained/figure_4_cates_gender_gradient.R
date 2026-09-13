# galos_coppock_2023/maintained/figure_4_cates_gender_gradient.R
# Output: output/figure_4_cates_gender_gradient.pdf, .png, .csv
# Depends on: helpers.R, author_classification.csv
# Description: Occupation-level CATEs against gender composition, with the Table 1 model 1
#   meta-regression line and its 95 percent confidence region (Fig. 4).
#
#   THE CATEGORIES ARE THE ORIGINAL STUDY AUTHORS' AND NOBODY ELSE'S. The deposited
#   occupation_cates.rds gives all 298 cells one of the three categories, but only 174 of
#   them were coded by the study's own authors; the other 124 were filled in here from
#   where the occupation fell in the ILO gender distribution. The published caption says so
#   ("as given by study authors where available and by us otherwise"), so the article is
#   accurate about what it did, but a reader cannot tell the two apart on the page, and a
#   colour is a claim about the occupation. The successor project drops the filled-in
#   categories rather than plotting them, and this figure now does the same: the 124 cells
#   keep their position, their interval and their weight and are drawn in grey.
#
#   Nothing about the estimates moves. The meta-regression is on fraction_female, which is
#   continuous and is not the category, so Table 1, Table 2 and every in-text quantity are
#   unchanged; what changes is which of 298 points is which colour.

source(here::here("maintained", "helpers.R"))

author_classification <- read_csv(
  here::here("maintained", "author_classification.csv"),
  show_col_types = FALSE
)

occupation_cates <- read_rds(file.path(data_dir, "occupation_cates.rds")) |>
  ungroup() |>
  left_join(author_classification, by = c("study_id", "occupation_observed"))

# A cell that lost its classification in the join would be plotted grey for the wrong
# reason, and a cell duplicated by it would be plotted twice.
stopifnot(
  nrow(occupation_cates) == 298,
  !anyNA(occupation_cates$author_classification)
)

occupation_cates <- occupation_cates |>
  mutate(
    author_category = if_else(author_classification == "yes",
                              occupation_gender_distribution, NA_character_)
  )

meta_fit <- rma.uni(
  yi = estimate,
  sei = std.error,
  mods = ~ fraction_female,
  data = occupation_cates
)

occupation_cates$weight <- weights(meta_fit)

preds_df <- tidy_predictions(meta_fit)

# The three categories keep the published figure's own colours and shapes: Set2 in the
# alphabetical order of the levels is green, orange and blue, and ggplot's default discrete
# shapes are circle, triangle and square, which is exactly what the caption describes. Both
# scales are spelled out rather than left to the default because an NA level is present
# now, and a discrete shape scale drops its NA points ENTIRELY unless told otherwise.
set2 <- RColorBrewer::brewer.pal(3, "Set2")

g <- ggplot(occupation_cates) +
  aes(
    fraction_female,
    estimate,
    color = author_category,
    shape = author_category
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
  scale_color_manual(
    values = c(men_dominated = set2[1], mixed = set2[2], women_dominated = set2[3]),
    na.value = "grey70"
  ) +
  scale_shape_manual(
    values = c(men_dominated = 16, mixed = 17, women_dominated = 15),
    na.value = 16
  ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none"
  ) +
  labs(
    x = "Status quo gender composition (share of women in occupation)",
    y = "Conditional average treatment effect"
  )

# ggplot drops a point whose colour or shape is unmapped, and it does it with a warning
# nobody reads. Count what the built plot actually carries.
built <- ggplot_build(g)$data[[3]]
stopifnot(
  nrow(built) == 298,
  sum(built$colour == "grey70") == sum(occupation_cates$author_classification == "no")
)

ggsave(file.path(out_dir, "figure_4_cates_gender_gradient.pdf"), plot = g, width = 7, height = 5)
ggsave(file.path(out_dir, "figure_4_cates_gender_gradient.png"), plot = g, width = 7, height = 5, dpi = 300)

# The figure prints no numbers, so the plotted values are written out instead: the 298
# points and the 100 evaluations of the fitted line and its confidence region. The two
# category columns are both carried, because which of them a point is drawn by is the
# thing this figure changed.
bind_rows(
  point = occupation_cates |>
    select(study_id, occupation_observed, fraction_female, estimate, std.error,
           conf.low, conf.high, weight,
           occupation_gender_distribution, author_classification, author_category),
  fit = preds_df |>
    select(fraction_female, estimate, std.error, conf.low, conf.high),
  .id = "layer"
) |>
  write_csv(file.path(out_dir, "figure_4_cates_gender_gradient.csv"))
