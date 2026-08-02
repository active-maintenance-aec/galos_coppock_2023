# galos_coppock_2023/maintained/helpers.R
# Output: none
# Depends on: nothing
# Description: Packages, paths and shared helpers sourced by every script in maintained/.

library(here)
library(tidyverse)
library(metafor)
library(broom)
library(scales)
library(rsample)
library(knitr)
library(kableExtra)
library(modelsummary)

# Anchors a script run on its own rather than through run_all.R.
here::i_am("maintained/helpers.R")

options(modelsummary_format_numeric_latex = "plain")
options(modelsummary_factory_latex = "kableExtra")

data_dir <- here::here("original", "replication_archive", "data")
out_dir <- here::here("maintained", "output")

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Meta-regression predictions across the full range of gender composition, renamed to
# the tidy column names the plotting code expects.
tidy_predictions <- function(fit) {
  fit |>
    predict(newmods = seq(0, 1, length.out = 100)) |>
    as.matrix() |>
    as.data.frame() |>
    `colnames<-`(c("estimate", "std.error", "conf.low", "conf.high",
                   "pred.low", "pred.high")) |>
    mutate(fraction_female = seq(0, 1, length.out = 100))
}

format_num <- function(x, digits = 3) {
  sprintf(paste0("%.", digits, "f"), as.numeric(x))
}

add_parens <- function(x, digits = 3) {
  paste0("(", format_num(x, digits = digits), ")")
}

# The "estimate (SE)" label printed beside each point in the forest plots.
make_se_entry <- function(est, se, digits = 2) {
  paste0(format_num(est, digits = digits), " ", add_parens(se, digits = digits))
}

# The published tables report tau-squared below the coefficients. modelsummary reads
# goodness-of-fit for rma objects through performance rather than broom, and performance
# does not report tau-squared, so it is supplied here.
glance_custom.rma <- function(x, ...) {
  tibble(tau.squared = x$tau2)
}

rma_gof_map <- tribble(
  ~raw,          ~clean,        ~fmt,
  "tau.squared", "Tau.squared", 2,
  "nobs",        "Nobs",        0
)
