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
  facet_grid(rows = vars(continent), scales = "free_y", space = "free") +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 3),
    axis.title = element_blank(),
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_text(size = 7)
  )

ggsave(file.path(out_dir, "figure_s2_2_evidence_map.pdf"), plot = g, width = 7, height = 14)
ggsave(file.path(out_dir, "figure_s2_2_evidence_map.png"), plot = g, width = 7, height = 14, dpi = 300)

# The map's content is which country-years are covered, so the filled cells are the
# plotted values.
evidence_map |>
  filter(present == 1) |>
  arrange(continent, country, year) |>
  select(continent, country, year) |>
  write_csv(file.path(out_dir, "figure_s2_2_evidence_map.csv"))
