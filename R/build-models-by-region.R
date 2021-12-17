source(here("R", "build-models.R"))

build_models_by_region <- function(
    obs, parameters,
    variant_relationships = c("scaled", "correlated"),
    cores_per_model = 4) {
  regions <- unique(obs$region)

  # set up working in parallel assuming using 4 cores inside loop
  plan("callr", workers = floor(future::availableCores() / 4))

  # make sure models are compiled
  model_1 <- fv_model(strains = 1)
  model_2 <- fv_model(strains = 2)

  obs <- obs %>%
    dplyr::group_by(region) %>%
    dplyr::group_split()

  names(obs) <- regions
  posteriors <- future.apply::future_lapply(obs,
        FUN = build_models,
        parameters = parameters,
        variant_relationships = variant_relationships,
        cores = cores_per_model)
  posteriors <- dplyr::bind_rows(posteriors)
  return(posteriors)
}