# galos_coppock_2023/maintained/text_difference_in_magnitude.R
# Output: output/text_difference_in_magnitude.csv
# Depends on: helpers.R
# Description: Predicted CATEs at 0 and 100 percent women, the difference in their magnitudes,
#   and the bootstrapped standard error of that difference (Results, main text).
#
#   The archive's analyze_cates.R sets sims <- 5 with a comment saying to raise it to 500
#   to recover the published SE. 500 resamples is what the paper used and what runs here;
#   it takes about two minutes.

source(here::here("maintained", "helpers.R"))

set.seed(343)

occupation_cates <- read_rds(file.path(data_dir, "occupation_cates.rds")) |> ungroup()

difference_in_magnitude <- function(dat) {
  fit <- rma.uni(yi = estimate, sei = std.error, mods = ~ fraction_female, data = dat)
  preds <- predict(fit, newmods = c(fraction_female = c(0, 1)))
  abs(preds$pred[2]) - abs(preds$pred[1])
}

meta_fit <- rma.uni(
  yi = estimate,
  sei = std.error,
  mods = ~ fraction_female,
  data = occupation_cates
)

preds <- predict(meta_fit, newmods = c(fraction_female = c(0, 1)))

boot_estimates <- bootstraps(occupation_cates, times = 500)$splits |>
  map_dbl(function(split) difference_in_magnitude(analysis(split)))

results <- tibble(
  quantity = c(
    "Predicted CATE at 0 percent women",
    "Predicted CATE at 100 percent women",
    "Difference in magnitude",
    "Bootstrapped SE of the difference in magnitude"
  ),
  value = c(
    preds$pred[1],
    preds$pred[2],
    abs(preds$pred[2]) - abs(preds$pred[1]),
    sd(boot_estimates)
  )
)

write_csv(results, file.path(out_dir, "text_difference_in_magnitude.csv"))
print(results)
