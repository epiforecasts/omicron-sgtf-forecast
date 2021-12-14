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
library(data.table)
library(loo)
library(scoringutils)
library(knitr)
options(mc.cores = 4)

# Load daily data
source(here("code", "load-data.R"))

# Cut off data before last 3 weeks and remove last day
obs <- daily_detrend
start_date <- max(obs$date) - weeks(3)
end_date <- max(obs$date) # - 1
obs <- filter(obs, between(date, start_date, end_date))
obs <- data.table(obs)

# Load parameters
source(here("code", "load-parameters.R"))
variant_relationships <- c("scaled", "pooled")

# Build models and save
if (run_model) {
  # Model with 1) scaled and 2) time-dependent relationship between variants
  source(here("code", "build-models.R"))
  save_to <- here("transmission", "fit")
  build_models(save_to, variant_relationships, parameters)
}

