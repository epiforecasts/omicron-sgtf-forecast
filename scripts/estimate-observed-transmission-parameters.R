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

# Load growth estimates
seq_growth <- load_growth(
  as.Date("2021-12-23"), type = "seq-by-age",
  min_date = "2021-12-01", max_date = "2021-12-23"
)

sgtf_growth <- load_sgtf_growth(
  region_date = as.Date("2022-01-06"), age_date = as.Date("2021-12-23"),
  min_date = "2021-12-01", max_date = "2021-12-23"
)

grid_seq <- growth_grid(seq_growth)[, source := "sequence"]
grid_sgtf <- growth_grid(sgtf_growth)[, source := "sgtf"]
grid <- rbind(grid_seq, grid_sgtf)
grid[, id := 1:.N]
grid <- grid[source == "sequence"][gt_diff == TRUE]

# Compile the stan model
model <- gt_load_model()

# Fit each model in turn
estimates <- future.apply::future_lapply(
  split(grid, by = "id"),
  function(.) {
    gt_estimate(
      growth = .$growth[[1]], by = .$by[[1]], gt = .$gt_prior[[1]],
      gt_diff = .$gt_diff[[1]], model = model, adapt_delta = 0.99,
      max_treedepth = 15, debug = FALSE,
      parallel_chains = 4
    )
  },
  future.seed = TRUE
)
estimates <- rbindlist(estimates)
estimates <- cbind(grid, estimates)

# extract and add vars
unnest_estimates <- function(estimates, target = "pp") {
  estimates[,
  (target) := pmap(
    list(get(target), stratification, gt_type, gt_diff, source),
    function(x, y, z, a, b) {
        x[, `:=`(stratification = y, gt_type = z, gt_diff = a, source = b)]
      }
    )
  ][, rbindlist(get(target), fill = TRUE)]
}
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
  here::here("data", "retrospective", "posterior_summary.csv")
)

fwrite(
  posterior_samples,
  here::here("data", "retrospective", "posterior_samples.csv")
)

fwrite(
  posterior_predictions,
  here::here("data", "retrospective", "posterior_predictions.csv")
)

fwrite(
  reproduction_no_pp,
  here::here("data", "retrospective", "reproduction_no_pp.csv")
)

fwrite(
  pp_samples[sample <= 1000],
  here::here("data", "retrospective", "posterior_predictions_samples.csv")
)

fwrite(
  reproduction_no_pp_samples[sample <= 1000],
  here::here("data", "retrospective", "reproduction_no_pp_samples.csv")
)


fwrite(
  posterior_locations,
  here::here("data", "retrospective", "posterior_locations.csv")
)