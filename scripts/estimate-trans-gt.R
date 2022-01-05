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

# Data for stan
stan_dt <- gt_dt(
  growth, by = "region",
  gt = gt_prior(type = "intrinsic", source = "hart2021")
)

# Set initial conditions based on priors
# Fit model (initially a little stroppy)
fit <- model$sample(
  data = stan_dt, adapt_delta = 0.95, max_treedepth = 15,
  init = gt_inits(stan_dt)
)

# summarise variables of interest
voi_summary <- gt_summarise_posterior(fit)

# summmarise posterior predictions
r_pp <- gt_summarise_growth_pp(fit, growth, by = "region")

# plot posterior predictions
gt_plot_pp(r_pp) +
  facet_wrap(~region)
