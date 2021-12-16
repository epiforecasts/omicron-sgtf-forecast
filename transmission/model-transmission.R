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
source(here("utils", "load-data.R"))
# Format
daily_sgtf <- daily_raw %>%
  transmute(date = date,
            cases = total_cases,
            cases_available = date,
            seq_total = total_sgt,
            seq_voc = sgtf,
            share_voc = sgtf / total_sgt,
            seq_available = date)
# Remove day-of-week in daily data for all cases (not applied to SGTF)
daily_sgtf_detrend <- daily_sgtf %>%
  mutate(cases = zoo::rollmean(cases, k = 7, align = "center", fill = NA),
         share_voc = seq_voc / seq_total) %>%
  filter(!is.na(cases))

# Use raw data
obs <- daily_sgtf

# Cut off data before last 3 weeks and remove last day
start_date <- max(obs$date) - weeks(3)
end_date <- max(obs$date) - truncate_days
obs <- filter(obs, between(date, start_date, end_date))
obs <- data.table(obs)

# Load parameters
source(here("utils", "load-parameters.R"))
variant_relationships <- c("scaled", "correlated")

datasets <- list("data-raw" = daily_sgtf,
                 "data-smooth" = daily_sgtf_detrend)

# Build models and save
if (run_model) {
  # Model with 1) scaled and 2) time-dependent relationship between variants
  source(here("utils", "build-models.R"))
  save_to <- here("transmission", "fit")
  build_models(save_to, variant_relationships, parameters)
}

