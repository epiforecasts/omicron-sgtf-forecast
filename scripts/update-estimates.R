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

# Set up in parallel estimation
plan("callr", workers = floor(future::availableCores() / 4))

##############################
# Load data and settings
##############################

# Target date
target_date <- as.Date("2021-12-13")

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
  sgtf_data_to_fv() %>%
  truncate_cases(days = 2)

# Estimate models for SGTF data
region_omicorn_posteriors <- build_models_by_region(
  obs, parameters,
  variant_relationships = c("scaled", "correlated"),
  cores_per_model = 4
)

##############################
# Estimate Bias in SGTF by region
##############################

bias_regional <- daily_regional %>%
  bias_data_to_fv() %>%
  truncate_cases(days = 2)

# Estimate models for SGTF data
region_bias_posteriors <- build_models_by_region(
  obs, parameters,
  variant_relationships = c("correlated"),
  cores_per_model = 4
)
