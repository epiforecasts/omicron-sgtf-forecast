library(cmdstanr)
library(data.table)
library(here)
library(ggplot2)

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
growth <- growth[date <= "2021-12-23"]

growth_sd_wide <- dcast(growth, region + date ~ type, value.var = "sd")
growth_sd_wide <- growth_sd_wide[!is.na(Omicron)]

growth_mean_wide <- dcast(growth, region + date ~ type, value.var = "mean")
growth_mean_wide <- growth_mean_wide[!is.na(Omicron)]

# Compile the stan model
model <- cmdstan_model("stan/generation-time.stan", include_paths = "stan")

# Data for stan
stan_dt <- list(
  t = nrow(growth_mean_wide),
  voc_r = growth_mean_wide$Omicron,
  voc_sd2 = growth_sd_wide$Omicron^2,
  nvoc_r_mean = growth_mean_wide$`non-Omicron`,
  nvoc_r_sd = growth_sd_wide$`non-Omicron`,
  # From Hart et al.
  # https://www.medrxiv.org/content/10.1101/2021.10.21.21265216v1
  # Assuming symmetric normal which is incorrect but an approximation
  gt_mean_mean = 4.6,
  gt_mean_sd = 0.36,
  gt_sd_mean = 3.1,
  gt_sd_sd = 0.18
)

# Set initial conditions based on priors
stan_inits <- function(data) {
  function() {
    data <- list(
      nvoc_r = purrr::map2(
        data$nvoc_r_mean, data$nvoc_r_sd, ~ rnorm(1, .x, .y * 0.1)
      ),
      gt_mean = rnorm(1, data$gt_mean_mean, data$gt_mean_sd * 0.1),
      gt_sd = rnorm(1, data$gt_sd_mean, data$gt_sd_sd * 0.1),
      voc_gt_mean_mod = rnorm(1, 1, 0.01),
      voc_gt_sd_mod = rnorm(1, 1, 0.01),
      sigma = rnorm(1, 0.1, 0.01),
      ta = rnorm(1, 1, 0.1)
    )
    data$voc_gt_mean <- data$gt_mean
    data$voc_gt_sd <- data$gt_sd
    return(data)
  }
}

# Fit model
fit <- model$sample(
  data = stan_dt, adapt_delta = 0.95, max_treedepth = 15,
  init = stan_inits(stan_dt)
)

# summarise variables of interest
vars_of_interest <- c("gt_mean", "gt_sd", "voc_gt_mean_mod", "voc_gt_sd_mod",
                      "voc_gt_mean", "voc_gt_sd", "ta", "sigma")
voi_posterior <- fit$summary(variables = vars_of_interest)
voi_posterior

# summmarise posterior predictions
voc_r_pp <- fit$summary(
  variables  = "pp_voc_r", posterior::quantile2,
  .args = list(probs = c(0.05, 0.2, 0.8, 0.95))
)
voc_r_pp <- as.data.table(voc_r_pp)[, type := "Posterior prediction"]
voc_r_pp <- cbind(growth[type %in% "Omicron", .(region, date)], voc_r_pp)
voc_r_pp <- rbind(
  growth[type %in% "Omicron"][, type := "Estimate"][!is.na(mean)],
  voc_r_pp, fill = TRUE, use.names = TRUE
)

# plot posterior predictions
voc_r_pp |>
  ggplot() +
  aes(x = date, y = median, fill = type) +
  geom_ribbon(aes(ymin = q5, ymax = q95), alpha = 0.3) +
  geom_ribbon(aes(ymin = q20, ymax = q80), alpha = 0.3) +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(fill = "Growth rate source", y = "Growth rate") +
  scale_fill_brewer(palette = "Dark2") +
  facet_wrap(~region)