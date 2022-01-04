library(cmdstanr)
library(data.table)
library(here)

# Set cmdstanr options
options(mc.cores = 4)
options(cmdstanr_max_rows = 100)

# Load functions
source(here("R", "load-local-data.R"))

# Load results
target_date <- get_latest_date()
results <- load_results(target_date)
growth <- summary(results$posterior, type = "growth")
growth <- growth[type %in% c("Omicron", "non-Omicron")]
growth <- growth[variant_relationship %in% "correlated"]
growth <- dcast(growth, region + date ~ type, value.var = "median")
growth <- growth[!(region %in% "England")]
growth <- growth[!is.na(Omicron)]
growth <- growth[date <= "2021-12-31"]

# Compile the stan model
model <- cmdstan_model("stan/generation-time.stan", include_paths = "stan")


# Data for stan
stan_dt <- list(
  t = nrow(growth),
  voc_r = growth$Omicron,
  nvoc_r = growth$`non-Omicron`,
  gt_mean = 6,
  gt_sd = 0
)

# Fit model
fit <- model$sample(data = stan_dt)
