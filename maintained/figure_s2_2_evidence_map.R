# galos_coppock_2023/maintained/figure_s2_2_evidence_map.R
# Output: output/figure_s2_2_evidence_map.pdf, .png, .csv
# Depends on: helpers.R
# Description: Evidence map of which country-years the collected audit experiments cover (fig. S2.2).

source(here::here("maintained", "helpers.R"))

evidence_map <- read_rds(file.path(data_dir, "evidence_map.rds"))

g <- ggplot(evidence_map) +
  aes(x = year, y = country, fill = present) +
  geom_hline(
    data = filter(evidence_map, present == 1),
    aes(yintercept = country),
    alpha = 0.5,
    linetype = "dotted",
    linewidth = 0.2
  ) +
  geom_tile() +
  theme_minimal() +
  scale_fill_manual(values = c("transparent", "black")) +
  # The geom_hline layer is drawn first and so trains the discrete y scale first, which under
  # today's ggplot2 puts the 25 present == 1 countries at the head of each facet's scale and
  # the remaining countries after them, a second alphabetical run the published page does not
  # have. The limits are pinned to the country factor's own level order, which is the order
  # geom_tile alone would train.
  scale_y_discrete(limits = \(x) intersect(levels(evidence_map$country), x)) +
  facet_grid(rows = vars(continent), scales = "free_y", space = "free") +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 3),
    axis.title = element_blank(),
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_text(size = 7)
  )

# The authors drew this one 6.5 by 9 (appendix_evidence_map.R:85), and a ggplot canvas is
# not a scale: the type keeps its point size while the panel stretches, so the same plot at
# a different aspect reflows and stops being the published drawing at another size. Measured
# against published p. 7, 6.5 by 9 puts every word where the page puts it.
ggsave(file.path(out_dir, "figure_s2_2_evidence_map.pdf"), plot = g, width = 6.5, height = 9)
ggsave(file.path(out_dir, "figure_s2_2_evidence_map.png"), plot = g, width = 6.5, height = 9, dpi = 300)

# The map's content is which country-years are covered, so the filled cells are the
# plotted values.
evidence_map |>
  filter(present == 1) |>
  arrange(continent, country, year) |>
  select(continent, country, year) |>
  write_csv(file.path(out_dir, "figure_s2_2_evidence_map.csv"))
