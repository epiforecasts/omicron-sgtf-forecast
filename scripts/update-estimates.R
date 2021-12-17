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

daily_raw <- load_local_data(as.Date("2021-12-13"))

# Set up data for forecast.vocs
obs <- daily_raw %>%
  transmute(region = region,
            date = date,
            cases = total_cases,
            cases_available = date,
            seq_total = total_sgt,
            seq_voc = sgtf,
            share_voc = sgtf / total_sgt,
            seq_available = date)

obs <- truncate_cases(obs, days = 2)


# Set up in parallel estimation
plan("callr", workers = floor(future::availableCores() / 4))



region_omicorn_posteriors <- build_models_by_region(
  obs, parameters,
  variant_relationships = c("scaled", "correlated"),
  cores_per_model = 4
)