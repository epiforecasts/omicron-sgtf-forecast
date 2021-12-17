# Build and save a model run
build_models <- function(obs, parameters,
                         variant_relationships = c("scaled", "correlated"),
                         cores = 4, chains = 4, samples_per_chain = 1000,
                         keep_fit = TRUE) {

  # build model for each variant relationship
  forecasts <- purrr::map_dfr(variant_relationships,
                       ~ forecast.vocs::forecast(obs = obs,
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
                                  chains = chains,
                                  parallel_chains = cores,
                                  # processing options
                                  output_loglik = TRUE,
                                  adapt_delta = 0.99,
                                  max_treedepth = 15,
                                  refresh = 0,
                                  show_messages = FALSE,
                                  iter_sampling = samples_per_chain))
  forecasts <- forecasts %>%
    dplyr::mutate(loo = purrr::map(fit, ~ .$loo()))
  if (!keep_fit) {
    forecasts <- forecasts %>%
      select(-fit)
  }
  return(forecasts)
}
