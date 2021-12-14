# Fit two-strain model with fixed variant relationship
# As if all cases had been "sequenced" (considered for SGT),
#   with "positive" those that have any SGT result at all (SGTF/not)

# Sourcing from inside this script will build and save model
if (!exists("run_model")) {run_model <- TRUE}

# Load packages
library(here)
library(dplyr)
library(data.table)

# Load daily data
source(here("code", "load-data.R"))
daily_sgt <- daily_raw %>%
  transmute(date = date,
            cases = total_cases,
            seq_total = total_cases,
            seq_voc = total_sgt,
            share_voc = total_sgt / total_cases,
            cases_available = date,
            seq_available = date)

# Cut off data before last 3 weeks and remove last day
obs <- daily_sgt
start_date <- max(obs$date) - weeks(3)
end_date <- max(obs$date)  - 2
obs <- filter(obs, between(date, start_date, end_date))
obs <- data.table(obs)

# Load parameters
source(here("code", "load-parameters.R"))
variant_relationships <- "scaled"

# Build models and save
if (run_model) {
  save_to <- here("sampling", "fit")
  source(here("code", "build-models.R"))

  # Adjust some parameters
  parameters$voc_scale <- c(0, 0.2)
  parameters$voc_label <- "SGT-result"

  build_models(save_to, variant_relationships, parameters)
}

