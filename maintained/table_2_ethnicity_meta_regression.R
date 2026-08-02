# galos_coppock_2023/maintained/table_2_ethnicity_meta_regression.R
# Output: output/table_2_ethnicity_meta_regression.tex, .csv
# Depends on: helpers.R
# Description: Meta-regression of occupation CATEs on gender composition among majority status
#   applicants, among minority status applicants, and interacted across both (Table 2).

source(here::here("maintained", "helpers.R"))

occupation_cates <- read_rds(file.path(data_dir, "occupation_cates_ethnicity.rds")) |> ungroup()

fits <- list(
  `Model 1` = rma.uni(yi = estimate, sei = std.error,
                      mods = ~ fraction_female,
                      data = filter(occupation_cates, ethnicity_fct == "Majority")),
  `Model 2` = rma.uni(yi = estimate, sei = std.error,
                      mods = ~ fraction_female,
                      data = filter(occupation_cates, ethnicity_fct == "Minority")),
  `Model 3` = rma.uni(yi = estimate, sei = std.error,
                      mods = ~ fraction_female * ethnicity_fct,
                      data = occupation_cates)
)

sample_row <- tibble(
  term = "Sample",
  `Model 1` = "Majority status group",
  `Model 2` = "Minority status group",
  `Model 3` = "Both groups"
)
attr(sample_row, "position") <- 9

modelsummary(
  fits,
  output = file.path(out_dir, "table_2_ethnicity_meta_regression.tex"),
  coef_map = c(
    intercept = "Intercept",
    fraction_female = "Gender gradient",
    ethnicity_fctMajority = "Majority status",
    `fraction_female:ethnicity_fctMajority` = "Majority x gradient"
  ),
  gof_map = rma_gof_map,
  add_rows = sample_row,
  stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  fmt = 2,
  title = "Meta-regression estimates of the gender gradient by majority / minority status",
  escape = FALSE
)

coefs <- imap(fits, function(fit, label) {
  tidy(fit, conf.int = TRUE) |> mutate(model = label)
}) |>
  list_rbind()

gofs <- imap(fits, function(fit, label) {
  glance(fit) |>
    select(tau.squared, nobs) |>
    mutate(model = label)
}) |>
  list_rbind()

coefs |>
  left_join(gofs, by = "model") |>
  select(model, term, estimate, std.error, statistic, p.value,
         conf.low, conf.high, tau.squared, nobs) |>
  write_csv(file.path(out_dir, "table_2_ethnicity_meta_regression.csv"))
