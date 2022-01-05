library(cmdstanr)
library(data.table)
library(here)
library(ggplot2)

# Set cmdstanr options
options(mc.cores = 4)
options(cmdstanr_max_rows = 100)

# Load functions
source(here("R", "load-local-data.R"))
source(here("R", "estimate-generation-time.R"))

# Load results
target_date <- get_latest_date()
growth <- load_growth(target_date, max_date = "2021-12-23")
growth <- growth[!(region %in% "England")]

# Compile the stan model
model <- gt_load_model()

estimates <- gt_estimate(
  growth, model, by = "region",
  gt = gt_prior(source = "hart2021", type = "household"),
  adapt_delta  = 0.95
)

# plot posterior predictions
gt_plot_pp(estimates$pp[[1]]) +
  facet_wrap(~region)
