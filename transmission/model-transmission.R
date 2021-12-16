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
library(data.table)
options(mc.cores = 4)

# Load parameters
source(here("utils", "load-parameters.R"))

# Load daily data, already filtered to specified dates
source(here("utils", "load-data.R"))

# Set up data for forecast.vocs
obs <- daily_raw %>%
  transmute(nhs_region = nhs_region,
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
    group_by(nhs_region) %>%
    mutate(cases = zoo::rollmean(cases, k = 7, align = "center", fill = NA),
           share_voc = seq_voc / seq_total) %>%
    filter(!is.na(cases))
}

# define regions
nhs_regions <- unique(obs$nhs_region)

# Model run and save
if (run_model) {
  source(here("utils", "build-models.R"))
  map(nhs_regions,
      ~ build_models(obs,
                     region = .x,
                     save_to = here("transmission", "nhs_region"),
                     variant_relationships,
                     parameters))
}

# clean up env
rm(sgtf_fills, england, regional, data_path)
