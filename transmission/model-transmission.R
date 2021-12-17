# Fit two-strain model for transmission advantage
# Specify some defaults
if (!exists("run_model")) {run_model <- TRUE} # FALSE to create objects without running model
if (!exists("data_type")) {data_type <- "raw"} # "smooth" to get 7-day MA
if (!exists("variant_relationships")) {
  variant_relationships <- c("scaled", "correlated")}

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

# Load parameters
source(here("utils", "load-parameters.R"))

# Load daily data, already filtered to specified dates
source(here("utils", "load-data.R"))

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

# Smooth with 7-day MA if specified
if (data_type == "smooth") {
  # Remove day-of-week in daily data for all cases (not applied to SGTF)
  obs <- daily_sgt %>%
    group_by(region) %>%
    mutate(cases = zoo::rollmean(cases, k = 7, align = "center", fill = NA),
           share_voc = seq_voc / seq_total) %>%
    filter(!is.na(cases))
}

# define regions
regions <- unique(obs$region)

# set up working in parallel assuming using 4 cores inside loop
plan("callr", workers = floor(future::availableCores() / 4))

# Build models and save
if (run_model) {
  # make sure models are compiled
  model_1 <- fv_model(strains = 1)
  model_2 <- fv_model(strains = 2)

  source(here("utils", "build-models.R"))
  future.apply::future_lapply(X = regions,
      FUN = build_models,
      obs = obs,
      # region = X,
      save_to = here("transmission", "region"),
      parameters = parameters,
      variant_relationships = variant_relationships,
      cores = 4)
}

# clean up env (all from load-data.R)
rm(sgtf_fills, england, regional,
   data_sgt, data_tot, data_path_sgt, data_path_tot,
   subregions)
