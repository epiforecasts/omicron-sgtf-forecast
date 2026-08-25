##############################
# Setup
##############################
# Load packages
library(here)
library(dplyr)
library(tidyr)
library(zoo)
library(purrr)
library(readr)
library(future)
library(future.callr)
library(future.apply)
library(forecast.vocs)
library(data.table)

# Load functions
source(here("R", "build-models-by-region-and-age.R"))
source(here("R", "load-parameters.R"))
source(here("R", "load-local-data.R"))
source(here("R", "munge-data.R"))
source(here("R", "postprocess.R"))

# Set up in parallel estimation
plan("callr", workers = floor(future::availableCores() / 2))

##############################
# Load data and settings
##############################

# Estimation start date
start_date <- as.Date("2021-11-16")
start_seq_date <- as.Date("2021-11-23")
target_date <- as.Date("2021-12-23")

# Load data for the target date
daily_regional <- read_csv(
    here("data", "public", "seq-by-region-and-age.csv")
  ) %>%
  filter(date_specimen >= start_date) %>%
  filter(date_specimen <= target_date)

# Load settings
seq_parameters <- load_sgft_age_parameters()

##############################
# Estimate Omicron using SGTF by region
##############################

seq_regional <- daily_regional %>%
  age_seq_data_to_fv() %>%
  group_by(age_group, region) %>%
  filter_seq_threshold(frac_thres = 0.01, n_thres = 5) %>%
  ungroup() %>%
  filter(!(is.na(cases) & is.na(seq_voc)))

# Estimate models for sequence data
region_omicorn_forecasts <- build_models_by_region_and_age(
  seq_regional, seq_parameters,
  variant_relationships = c("correlated"),
  cores_per_model = 2, chains = 2,
  samples_per_chain = 2000,
  keep_fit = FALSE, loo = FALSE
)

omicron_results <- list(
  data = seq_regional,
  posterior = summary(
    region_omicorn_forecasts, target = "posterior", type = "all"
  ),
  diagnostics = summary(region_omicorn_forecasts, target = "diagnostics")
)

save_results(omicron_results, "seq-by-age", target_date)
