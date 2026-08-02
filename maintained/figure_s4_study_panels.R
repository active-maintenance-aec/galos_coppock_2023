# galos_coppock_2023/maintained/figure_s4_study_panels.R
# Output: output/figure_s4_study_panels.pdf, .csv
# Depends on: helpers.R
# Description: The 57 study-by-study panels of occupation-level CATEs, figs. S4.5 to S4.61,
#   one page per study, with the study's own gender gradient in the lower facet where the
#   study distinguishes three or more occupations.
#
#   These are the densest published numbers in the paper: each panel prints the estimate and
#   standard error beside every occupation, 298 pairs in all, plus the 37 gradients. The
#   deposited loop that draws them has its ggsave() commented out, so the archive as
#   deposited produces none of the 57. They are written here as one paginated PDF and, more
#   usefully, as a CSV of every plotted value.
#
#   The published panels label each occupation with three lines: its name, the share of women
#   in it, and a sample size. The first two come from the deposited data and both reproduce.
#   The third is only partly recoverable. The deposit carries the residual degrees of freedom
#   of each occupation's own regression, and df + 1 equals the published N exactly for 167 of
#   the 298 occupations, which is every occupation of 35 of the 57 studies. For the other 22
#   studies the published N is several times larger, which is what a design with many
#   applications per job opening would give, and the deposit gives no way to recover the
#   multiplier. n_from_df is written to the CSV so the ground truth can count how far it gets;
#   the panels themselves are labelled with what the deposit has, name and composition.

source(here::here("maintained", "helpers.R"))

occupation_cates <- read_rds(file.path(data_dir, "occupation_cates.rds")) |> ungroup()
gender_gradients <- read_csv(file.path(out_dir, "gender_gradients.csv"), show_col_types = FALSE)
study_citations <- read_rds(file.path(data_dir, "estimated_ates.rds")) |>
  ungroup() |>
  select(study_id, study_name_citation)

occupation_rows <- occupation_cates |>
  left_join(study_citations, by = "study_id") |>
  mutate(
    facet = "Occupation estimates",
    panel_label = str_glue("{occupation_observed}\n{round(fraction_female * 100)}% women in occupation"),
    n_from_df = df + 1
  ) |>
  select(study_id, study_name_citation, facet, panel_label, occupation_observed,
         fraction_female, n_from_df, estimate, std.error, conf.low, conf.high)

gradient_rows <- gender_gradients |>
  mutate(
    facet = "Gender gradient",
    panel_label = "Gender gradient",
    occupation_observed = NA_character_,
    fraction_female = NA_real_,
    n_from_df = NA_real_
  ) |>
  select(study_id, study_name_citation, facet, panel_label, occupation_observed,
         fraction_female, n_from_df, estimate, std.error, conf.low, conf.high)

gg_df <- bind_rows(occupation_rows, gradient_rows) |>
  mutate(
    entry = make_se_entry(estimate, std.error),
    facet = factor(facet, levels = c("Occupation estimates", "Gender gradient")),
    # The gradient row has no gender composition of its own; sorting it to the foot of the
    # panel is what puts it in the lower facet, as the published figures show it.
    sort_key = if_else(is.na(fraction_female), -Inf, fraction_female)
  )

draw_panel <- function(dat) {
  ggplot(dat, aes(estimate, fct_reorder(panel_label, sort_key))) +
    geom_vline(xintercept = 0, color = "red", linetype = "dashed", alpha = 0.5) +
    geom_point() +
    geom_linerange(aes(xmin = conf.low, xmax = conf.high)) +
    geom_text(aes(label = entry), nudge_y = 0.3, size = 3) +
    facet_grid(rows = vars(facet), space = "free", scales = "free_y") +
    coord_cartesian(xlim = c(-100, 100)) +
    theme_bw() +
    theme(
      strip.text = element_blank(),
      text = element_text(size = 8),
      axis.title.y = element_blank()
    ) +
    labs(
      x = "CATE estimate (top facet) or gender gradient estimate (bottom facet)",
      title = unique(dat$study_name_citation)
    )
}

panels <- gg_df |>
  arrange(study_name_citation) |>
  group_split(study_name_citation) |>
  map(draw_panel)

pdf(file.path(out_dir, "figure_s4_study_panels.pdf"), width = 6.5, height = 8, onefile = TRUE)
walk(panels, print)
dev.off()

gg_df |>
  arrange(study_name_citation, facet, desc(sort_key)) |>
  select(study_id, study_name_citation, facet, occupation_observed, fraction_female,
         n_from_df, estimate, std.error, conf.low, conf.high, entry) |>
  write_csv(file.path(out_dir, "figure_s4_study_panels.csv"))
