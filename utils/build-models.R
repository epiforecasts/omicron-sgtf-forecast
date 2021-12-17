# Build and save a model run
library(forecast.vocs)
library(purrr)

build_models <- function(obs,
                         region,
                         save_to,
                         parameters,
                         variant_relationships = c("scaled", "correlated"),
                         cores = 4
                         ) {

  # Use only one region
  if (missing(region)) {obs <- obs
  } else {
    obs <- obs[obs$nhs_region == region,]}

  # build model for each variant relationship
  forecast_fits <- map(variant_relationships,
                       ~ forecast(obs = obs,
                                  # variant relationship
                                  variant_relationship = .x,
                                  # variant options
                                  voc_scale = parameters$voc_scale,
                                  scale_r = parameters$scale_r,
                                  strains = parameters$strains,
                                  r_init = parameters$r_init,
                                  r_step = parameters$r_step,
                                  overdispersion = parameters$overdispersion,
                                  timespan = parameters$timespan,
                                  horizon = parameters$horizon,
                                  voc_label = parameters$voc_label,
                                  parallel_chains = cores,
                                  # processing options
                                  output_loglik = TRUE,
                                  adapt_delta = 0.99,
                                  max_treedepth = 15,
                                  refresh = 0,
                                  show_messages = FALSE))
  names(forecast_fits) <- variant_relationships

  # Unnest posterior
  forecasts <- map(forecast_fits,
                   ~ unnest_posterior(.x))
  # Create files if not already
  if (!dir.exists(here(save_to, region))) {
    dir.create(here(save_to, region, "figures"), recursive = TRUE)
  }
  # Save output
  saveRDS(forecast_fits, here(save_to, region, "forecast-fits.rds"))
  saveRDS(forecasts, here(save_to, region, "forecasts.rds"))
}
