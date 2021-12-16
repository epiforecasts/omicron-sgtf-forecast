# Fit two-strain model

# Sourcing from inside this script will build and save model
if (!exists("run_model")) {run_model <- TRUE}

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

# Load parameters
source(here("utils", "load-parameters.R"))
variant_relationships <- c("scaled", "correlated")

# Load daily data, already filtered to specified dates
source(here("utils", "load-data.R"))
daily_sgt <- daily_raw %>%
  transmute(nhs_region = nhs_region,
            date = date,
            cases = total_cases,
            cases_available = date,
            seq_total = total_sgt,
            seq_voc = sgtf,
            share_voc = sgtf / total_sgt,
            seq_available = date)

# Remove day-of-week in daily data for all cases (not applied to SGTF)
daily_sgt_detrend <- daily_sgt %>%
  group_by(nhs_region) %>%
  mutate(cases = zoo::rollmean(cases, k = 7, align = "center", fill = NA),
         share_voc = seq_voc / seq_total) %>%
  filter(!is.na(cases))

# Use specified data, raw or deseasonalised
datasets <- list("data-raw" = daily_sgt,
                 "data-smooth" = daily_sgt_detrend)

if (!exists("data_type")) {
  data_type <- "raw"
}
obs <- datasets[[data_type]]
nhs_regions <- unique(obs$nhs_region)

# Build models and save
# Model with 1) scaled and 2) time-dependent relationship between variants
if (run_model) {
  source(here("utils", "build-models.R"))
  map(nhs_regions,
      ~ build_models(obs,
                     region = .x,
                     save_to = here("transmission", "nhs_region"),
                     variant_relationships,
                     parameters))
}
