# Fit two-strain model with fixed variant relationship to explore sampling bias
#   between cases with an S-gene target result and those without.
# Model if all cases had been "sequenced" (potential for SGT),
#   with "VoC sequences" those that have any SGT result
#   and "non-VoC" those with an NA result for SGT

# Sourcing from an empty environment inside this script will build and save model
if (!exists("run_model")) {run_model <- TRUE}

# Load packages
library(here)
library(dplyr)
library(zoo)
library(data.table)

# Load parameters
source(here("code", "load-parameters.R"))
parameters$voc_scale <- c(0, 0.2)
parameters$voc_label <- "SGT-result"
variant_relationships <- "scaled"

# Load daily data and reshape to compare any SGT against SGT + NA-SGT
source(here("code", "load-data.R"))
daily_sgt <- daily_raw %>%
  transmute(date = date,
            cases = total_cases,
            seq_total = total_cases,
            seq_voc = total_sgt,
            share_voc = total_sgt / total_cases,
            cases_available = date + 1,
            seq_available = date + 1)

# Fit to both raw data and 7-day MA smoothed data
daily_sgt_detrend <- daily_sgt %>%
  mutate(cases = zoo::rollmean(cases, k = 7, align = "center", fill = NA),
         seq_total = cases,
         seq_voc = zoo::rollmean(seq_voc, k = 7, align = "center", fill = NA),
         share_voc = seq_voc / seq_total)

datasets <- list("data-raw" = daily_sgt,
                 "data-smooth" = daily_sgt_detrend)

# Build models and save
if (run_model) {
  source(here("code", "build-models.R"))
  for (data_type in names(datasets)) {
    save_to <- here("sampling", data_type, "fit")

    # Cut off data before last 3 weeks and remove last day
    obs <- datasets[[data_type]]
    start_date <- max(obs$date) - weeks(3)
    end_date <- max(obs$date) - 1 # or 3: same date range for MA/raw data
    obs <- filter(obs, between(date, start_date, end_date))
    obs <- data.table(obs)

    # fit and save
    build_models(save_to, variant_relationships, parameters)
  }
}
