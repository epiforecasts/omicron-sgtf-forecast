# Build and save a model run
library(forecast.vocs)
library(purrr)
options(mc.cores = 4)

build_models <- function(save_to,
                         variant_relationships,
                         parameters) {
  # build model for each variant relationship
  forecast_fits <- map(variant_relationships,
                       ~ forecast(obs,
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
  # Save output
  saveRDS(forecast_fits, here(save_to, "forecast-fits.rds"))
  saveRDS(forecasts, here(save_to, "forecasts.rds"))
}
