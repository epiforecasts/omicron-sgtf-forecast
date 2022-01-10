source(here::here("R", "build-models.R"))

build_models_by_region_and_age <- function(
    obs, parameters,
    variant_relationships = c("scaled", "correlated"),
    cores_per_model = 4, chains = 4, samples_per_chain = 1000,
    keep_fit = TRUE, loo = TRUE) {
  # make sure models are compiled
  model_1 <- forecast.vocs::fv_model(strains = 1)
  model_2 <- forecast.vocs::fv_model(strains = 2)

  obs_split <- obs %>%
    dplyr::group_by(region, age_group) %>%
    dplyr::group_split()

  forecasts <- future.apply::future_lapply(obs_split,
        FUN = build_models,
        parameters = parameters,
        variant_relationships = variant_relationships,
        cores = cores_per_model,
        chains = chains,
        samples_per_chain = samples_per_chain,
        keep_fit = keep_fit, loo = loo,
        future.seed = TRUE)

  forecasts <- dplyr::bind_rows(forecasts)
  forecasts <- dplyr::bind_cols(
    obs %>%
      select(region, age_group) %>%
      unique(),
    forecasts
  )

  forecasts <- data.table::as.data.table(forecasts)
  class(forecasts) <- c("fv_forecast", class(forecasts))
  return(forecasts)
}