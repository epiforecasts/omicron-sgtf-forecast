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
start_sgtf_date <- as.Date("2021-11-23")
target_date <- as.Date("2021-12-23")

# Load data for the target date
daily_regional <- read_csv(
    here("data", "private", "sgtf-by-region-and-age.csv")
  ) %>%
  summarise_by_15_year_age_group() %>%
  filter(date >= start_date) %>%
  filter(date <= target_date)

# Load settings
sgtf_parameters <- load_sgtf_age_parameters()

##############################
# Estimate Omicron using SGTF by region
##############################

sgtf_regional <- daily_regional %>%
  truncate_sequences(start_date = start_sgtf_date) %>%
  truncate_cases(days = 0) %>%
  age_sgtf_data_to_fv() %>%
  filter(!(is.na(cases) & is.na(seq_voc)))

# Estimate models for SGTF data
region_omicron_forecasts <- build_models_by_region_and_age(
  sgtf_regional, sgtf_parameters,
  variant_relationships = c("correlated"),
  cores_per_model = 2, chains = 2,
  samples_per_chain = 2000,
  keep_fit = FALSE, loo = FALSE
)

omicron_results <- list(
  data = sgtf_regional,
  posterior = summary(
    region_omicron_forecasts, target = "posterior", type = "all"
  ),
  diagnostics = summary(region_omicron_forecasts, target = "diagnostics")
)

save_results(omicron_results, "sgtf-by-age", target_date)
