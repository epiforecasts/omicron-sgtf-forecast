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
region_target_date <- get_latest_date()
region_growth <- load_growth(
  region_target_date, min_date = "2022-01-12", max_date = "2022-02-06"
)
region_growth <- region_growth[!(region %in% "England")]

growth <- data.table(
  stratification = "region",
  growth = list(region_growth),
  by = c(list("region"))
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
      model = model, adapt_delta = 0.95
    )
)
estimates <- rbindlist(estimates)
estimates <- cbind(grid, estimates)

# Extract and combine summaries
posterior_summary <- estimates[,
 rbindlist(summary), by = c("stratification", "gt_type")
]

posterior_predictions <- estimates[,
 rbindlist(pp), by = c("stratification", "gt_type")
]

# Save results
fwrite(
  posterior_summary,
  here::here("data", "retrospective", "posterior_summary.csv")
)

fwrite(
  posterior_predictions,
  here::here("data", "retrospective", "posterior_predictions.csv")
)