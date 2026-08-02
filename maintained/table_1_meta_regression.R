# galos_coppock_2023/maintained/table_1_meta_regression.R
# Output: output/table_1_meta_regression.tex, .csv
# Depends on: helpers.R
# Description: Meta-regression of occupation CATEs on gender composition, with no fixed
#   effects and with country, year, country-and-year, and study fixed effects (Table 1).

source(here::here("maintained", "helpers.R"))

occupation_cates <- read_rds(file.path(data_dir, "occupation_cates.rds")) |> ungroup()
studies <- read_rds(file.path(data_dir, "studies.rds"))

occupation_cates <- left_join(occupation_cates, studies, by = "study_id")

fits <- list(
  `Model 1` = rma.uni(yi = estimate, sei = std.error,
                      mods = ~ fraction_female,
                      data = occupation_cates),
  `Model 2` = rma.uni(yi = estimate, sei = std.error,
                      mods = ~ fraction_female + as.factor(country),
                      data = occupation_cates),
  `Model 3` = rma.uni(yi = estimate, sei = std.error,
                      mods = ~ fraction_female + as.factor(first_year),
                      data = occupation_cates),
  `Model 4` = rma.uni(yi = estimate, sei = std.error,
                      mods = ~ fraction_female + as.factor(first_year) + as.factor(country),
                      data = occupation_cates),
  `Model 5` = rma.uni(yi = estimate, sei = std.error,
                      mods = ~ fraction_female + as.factor(study_id),
                      data = occupation_cates)
)

fixed_effects_row <- tibble(
  term = "Fixed effects",
  `Model 1` = "None",
  `Model 2` = "Country",
  `Model 3` = "Year",
  `Model 4` = "Country and year",
  `Model 5` = "Study"
)
attr(fixed_effects_row, "position") <- 5

modelsummary(
  fits,
  output = file.path(out_dir, "table_1_meta_regression.tex"),
  coef_map = c(intercept = "Intercept", fraction_female = "Gender gradient"),
  gof_map = rma_gof_map,
  add_rows = fixed_effects_row,
  stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  fmt = 2,
  title = "Meta-regression estimates of the gender gradient",
  escape = FALSE
)

# The same coefficients as a plain table, so the published values can be checked
# without parsing LaTeX.
coefs <- imap(fits, function(fit, label) {
  tidy(fit, conf.int = TRUE) |>
    filter(term %in% c("intercept", "fraction_female")) |>
    mutate(model = label)
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
  write_csv(file.path(out_dir, "table_1_meta_regression.csv"))
