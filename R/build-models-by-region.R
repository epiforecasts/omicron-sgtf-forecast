source(here("R", "build-models.R"))

build_models_by_region <- function(
    obs, parameters,
    variant_relationships = c("scaled", "correlated"),
    cores_per_model = 4, chains = 4, samples_per_chain = 1000,
    keep_fit = FALSE) {
  regions <- unique(obs$region)

  # make sure models are compiled
  model_1 <- forecast.vocs::fv_model(strains = 1)
  model_2 <- forecast.vocs::fv_model(strains = 2)

  obs <- obs %>%
    dplyr::group_by(region) %>%
    dplyr::group_split()

  names(obs) <- regions
  forecasts <- future.apply::future_lapply(obs,
        FUN = build_models,
        parameters = parameters,
        variant_relationships = variant_relationships,
        cores = cores_per_model,
        chains = chains,
        samples_per_chain = samples_per_chain)
  forecasts <- purrr::map2(
    forecasts, regions, ~ dplyr::mutate(.x, region = .y)
  )
  forecasts <- dplyr::bind_rows(forecasts)
  forecasts <- forecasts %>%
    dplyr::mutate(loo = purrr::map(fit, .$loo()))
  if (!keep_fit) {
    forecasts <- forecasts %>%
      select(-fit)
  }
  return(forecasts)
}