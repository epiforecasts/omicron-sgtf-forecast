# Load required packages
library(cmdstanr)
library(data.table)
library(purrr)
library(here)
library(ggplot2)
library(future)
library(future.apply)
library(future.callr)

# Set up in parallel estimation
plan("callr", workers = floor(future::availableCores() / 4))

# Load functions
source(here("R", "load-local-data.R"))
source(here("R", "estimate-generation-time.R"))
source(here("R", "gt-helpers.R"))
source(here("R", "est-plan-b.R"))

# Load growth estimates
region_target_date <- get_latest_date()
region_growth <- load_growth(
  region_target_date, min_date = "2022-01-12", max_date = "2022-02-06"

# Set up estimation grid
grid <- growth_grid(
  seq_growth, prior_source = "abbott2022"
)[, source := "sequence"]


# Compile the stan model
model <- gt_load_model()

# Fit each model in turn
estimates <- lapply(
  split(grid, by = "id"),
  function(.) {
    gt_estimate(
      growth = .$growth[[1]], by = .$by[[1]], gt = .$gt_prior[[1]],
      gt_diff = .$gt_diff[[1]], model = model, adapt_delta = 0.99,
      max_treedepth = 15, debug = FALSE,
      parallel_chains = 4
    )
  }
)
estimates <- rbindlist(estimates)
estimates <- cbind(grid, estimates)


# Extract and combine summaries
posterior_summary <- unnest_estimates(estimates, target = "summary")

posterior_samples <- unnest_estimates(estimates, target = "samples")

posterior_predictions <- unnest_estimates(estimates, target = "r_pp")
reproduction_no_pp <- unnest_estimates(estimates, target = "R_pp")


pp_samples <- unnest_estimates(estimates, target = "r_pp_samples")
reproduction_no_pp_samples <- unnest_estimates(
  estimates, target = "R_pp_samples"
)

posterior_locations <- unnest_estimates(estimates, target = "locations")
posterior_locations <- unique(posterior_locations[, date := NULL])

# Save results
fwrite(
  posterior_summary,
  here::here(
    "data", "estimates", "ba2", "retrospective", "posterior_summary.csv"
  )
)

fwrite(
  posterior_samples,
  here::here(
    "data", "estimates", "ba2", "retrospective", "posterior_samples.csv"
  )
)

fwrite(
  posterior_predictions,
  here::here(
    "data", "estimates", "ba2", "retrospective", "posterior_predictions.csv"
  )
)

fwrite(
  reproduction_no_pp,
  here::here(
    "data", "estimates", "ba2", "retrospective", "reproduction_no_pp.csv"
  )
)

fwrite(
  posterior_locations,
  here::here(
    "data", "estimates", "ba2", "retrospective", "posterior_locations.csv"
  )
)