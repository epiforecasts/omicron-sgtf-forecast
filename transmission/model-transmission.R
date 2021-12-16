# Fit two-strain model

# Sourcing from inside this script will build and save model
if (!exists("run_model")) {run_model <- TRUE}
if (!exists("data_type")) {data_type <- "data-raw"}

# Load packages
library(here)
library(dplyr)
library(tidyr)
library(forecast.vocs)
library(ggplot2)
library(patchwork)
library(purrr)
library(loo)
library(scoringutils)
library(knitr)
options(mc.cores = 4)

# Load daily data, already filtered to specified dates
source(here("utils", "load-data.R"))
daily_sgt <- daily_raw %>%
  transmute(date = date,
            cases = total_cases,
            cases_available = date,
            seq_total = total_sgt,
            seq_voc = sgtf,
            share_voc = sgtf / total_sgt,
            seq_available = date)

# Remove day-of-week in daily data for all cases (not applied to SGTF)
daily_sgt_detrend <- daily_sgt %>%
  mutate(cases = zoo::rollmean(cases, k = 7, align = "center", fill = NA),
         share_voc = seq_voc / seq_total) %>%
  filter(!is.na(cases))

# Use specified data, raw or deseasonalised
datasets <- list("data-raw" = daily_sgt,
                 "data-smooth" = daily_sgt_detrend)
obs <- datasets[[data_type]]

# Load parameters
source(here("utils", "load-parameters.R"))
variant_relationships <- c("scaled", "correlated")

# Build models and save
if (run_model) {
  # Model with 1) scaled and 2) time-dependent relationship between variants
  source(here("utils", "build-models.R"))
  save_to <- here("transmission", "fit")
  build_models(save_to, variant_relationships, parameters)
}
