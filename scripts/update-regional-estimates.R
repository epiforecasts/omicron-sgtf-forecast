##############################
# Setup
##############################
# Load packages
library(here)
library(dplyr)
library(tidyr)
library(zoo)
library(purrr)
library(future)
library(future.callr)
library(future.apply)
library(forecast.vocs)

# Load functions
source(here("R", "build-models-by-region.R"))
source(here("R", "load-parameters.R"))
source(here("R", "load-local-data.R"))
source(here("R", "munge-data.R"))
source(here("R", "postprocess.R"))

# Set up in parallel estimation
plan("callr", workers = floor(future::availableCores() / 2))

##############################
# Load data and settings
##############################

# Target date
target_date <- get_latest_date() 

# Estimation start date
start_date <- as.Date("2021-11-23")

# Load data for the target date
daily_regional <- load_local_data(target_date) %>%
  filter(date >= start_date)

# Load settings
sgtf_parameters <- load_sgtf_parameters()
bias_parameters <- load_bias_parameters()

##############################
# Estimate Omicron using SGTF by region
##############################

sgtf_regional <- daily_regional %>%
  truncate_cases(days = 2) %>%
  sgtf_data_to_fv()

# Estimate models for SGTF data
region_omicorn_forecasts <- build_models_by_region(
  sgtf_regional, sgtf_parameters,
  variant_relationships = c("scaled", "correlated"),
  cores_per_model = 2, chains = 2, samples_per_chain = 2000,
  keep_fit = FALSE
)

omicron_results <- list(
  posterior = summary(region_omicorn_forecasts, target = "posterior")[,
   loo := NULL
  ],
  diagnostics = summary(region_omicorn_forecasts, target = "diagnostics")[,
   loo := NULL
  ],
  loo = extract_loo(region_omicorn_forecasts)
)

save_results(omicron_results, "sgtf", target_date)

##############################
# Estimate Bias in SGTF by region
##############################

bias_regional <- daily_regional %>%
  truncate_cases(days = 2) %>%
  bias_data_to_fv()

# Estimate models for SGTF data
region_bias_forecasts <- build_models_by_region(
  bias_obs, bias_parameters,
  variant_relationships = c("correlated"),
  cores_per_model = 2, chains = 2, iter_sampling = 2000,
  keep_fit = FALSE, loo = FALSE
)

bias_results <- list(
  posterior = summary(region_bias_forecasts, target = "posterior"),
  diagnostics = summary(region_bias_forecasts, target = "diagnostics")
)

save_results(bias_results, "bias", target_date)
