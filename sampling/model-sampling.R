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
source(here("utils", "load-parameters.R"))
parameters$voc_scale <- c(0, 0.2)
parameters$voc_label <- "SGT-result"
variant_relationships <- "independent"

# Load daily data, already filtered to specified dates
# - reshape to compare any SGT against SGT + NA-SGT
source(here("utils", "load-data.R"))
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

datasets <- list("raw" = data.table(daily_sgt),
                 "smooth" = data.table(daily_sgt_detrend))

# Build models and save
if (run_model) {
  source(here("utils", "build-models.R"))
  for (data_type in names(datasets)) {
    save_to <- here("sampling", data_type, "fit")

    obs <- datasets[[data_type]]
    # fit and save
    build_models(save_to, variant_relationships, parameters)
  }
}
