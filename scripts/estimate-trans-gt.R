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
growth <- growth[!(region %in% "England")]
growth <- growth[date <= "2021-12-31"]

growth_sd <- dcast(growth, region + date ~ type, value.var = "sd")
growth_sd <- growth_sd[!is.na(Omicron)]

growth <- dcast(growth, region + date ~ type, value.var = "mean")
growth <- growth[!is.na(Omicron)]

# Compile the stan model
model <- cmdstan_model("stan/generation-time.stan", include_paths = "stan")

# Data for stan
stan_dt <- list(
  t = nrow(growth),
  voc_r = growth$Omicron,
  voc_sd2 = growth_sd$Omicron^2,
  nvoc_r_mean = growth$`non-Omicron`,
  nvoc_r_sd = growth_sd$`non-Omicron`,
  # From Hart et al.
  # https://www.medrxiv.org/content/10.1101/2021.10.21.21265216v1
  # Assuming symmetric normal which is incorrect but an approximation
  gt_mean_mean = 4.6,
  gt_mean_sd = 0.36,
  gt_sd_mean = 3.1,
  gt_sd_sd = 0.18
)

# Fit model
fit <- model$sample(data = stan_dt, adapt_delta = 0.9, max_treedepth = 15)

# summarise variables of interest
vars_of_interest <- c("gt_mean", "gt_sd", "voc_gt_mean_mod", "voc_gt_sd_mod",
                      "voc_gt_mean", "voc_gt_sd", "ta", "sigma")

fit$summary(variables = vars_of_interest)
