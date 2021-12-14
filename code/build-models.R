# Fit two-strain model with fixed and varying relationship between strains

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
timespan <- 1
horizon <- 1
obs <- daily

# Cut off data before last 3 weeks and remove last day
start_date <- max(obs$date) - weeks(3)
end_date <- max(obs$date) - 1
obs <- filter(obs, between(date, start_date, end_date))
obs <- data.table(obs)

# Load parameters
source(here("code", "load-parameters.R"))
variant_relationships <- c("scaled", "pooled")

# Build models and save
interactive <- FALSE

if (interactive) {
  # Model with 1) scaled and 2) time-dependent relationship between variants
  forecast_fits <- map(variant_relationships,
                       ~ forecast(obs,
                                  # scaled or pooled relationship
                                  variant_relationship = .x,
                                  # variant options
                                  voc_scale = parameters$voc_scale,
                                  scale_r = parameters$scale_r,
                                  strains = parameters$strains,
                                  r_init = parameters$r_init,
                                  overdispersion = parameters$overdispersion,
                                  timespan = timespan,
                                  horizon = horizon,
                                  voc_label = "Omicron",
                                  # processing options
                                  output_loglik = TRUE,
                                  adapt_delta = 0.99,
                                  max_treedepth = 15,
                                  refresh = 0,
                                  show_messages = FALSE))
  names(forecast_fits) <- variant_relationships
  saveRDS(forecast_fits, here("output", "forecast-fits.rds"))

  # Unnest posterior
  forecasts <- map(forecast_fits,
                   ~ unnest_posterior(.x) %>%
                     mutate(across(mean:q95, round, 2)))
  saveRDS(forecasts, here("output", "forecasts.rds"))
}

