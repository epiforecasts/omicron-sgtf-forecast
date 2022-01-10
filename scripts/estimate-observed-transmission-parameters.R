# Load required packages
library(cmdstanr)
library(data.table)
library(purrr)
library(here)
library(ggplot2)

# Set cmdstanr options
options(mc.cores = 4)
options(cmdstanr_max_rows = 100)

# Load functions
source(here("R", "load-local-data.R"))
source(here("R", "estimate-generation-time.R"))

# Load growth estimates
region_target_date <- as.Date("2022-01-06")
region_growth <- load_growth(
  region_target_date, min_date = "2021-12-01", max_date = "2021-12-23"
)
region_growth <- region_growth[!(region %in% "England")]

age_region_growth <- load_growth(
  as.Date("2021-12-23"), type = "sgtf-by-age",
  min_date = "2021-12-01", max_date = "2021-12-23"
)

growth <- data.table(
  stratification = c("region", "age", "age and region"),
  growth = list(
    region_growth, age_region_growth[region %in% "England"],
    age_region_growth[!(region %in% "England")]
  ),
  by = c(list("region"), list("age_group"), list(c("age_group", "region")))
)

# Set up estimation grid
grid <- CJ(
  stratification = c("region", "age", "age and region"),
  gt_type = c("intrinsic", "household")
)

grid[, gt_prior := purrr::map(
  gt_type, ~ gt_prior(source = "hart2021", type = .x))
]

grid <- merge(grid, growth, by = "stratification")
grid[, id := 1:.N]

# Compile the stan model
model <- gt_load_model()

# Fit each model in turn
estimates <- purrr::map(
  split(grid, by = "id"),
  ~ gt_estimate(
      growth = .$growth[[1]], by = .$by[[1]], gt = .$gt_prior[[1]],
      model = model, adapt_delta = 0.95, max_treedepth = 15
    )
)
estimates <- rbindlist(estimates)
estimates <- cbind(grid, estimates)

# extract and add vars
unnest_estimates <- function(estimates, target = "pp") {
  estimates[,
  (target) := pmap(
    list(get(target), stratification, gt_type),
    function(x, y, z) {
        x[, `:=`(stratification = y, gt_type = z)]
      }
    )
  ][, rbindlist(get(target), fill = TRUE)]
}
# Extract and combine summaries
posterior_summary <- unnest_estimates(estimates, target = "summary")

posterior_samples <- unnest_estimates(estimates, target = "samples")

posterior_predictions <- unnest_estimates(estimates, target = "pp")

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