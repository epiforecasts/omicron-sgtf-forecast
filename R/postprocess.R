extract_loo <- function(forecasts) {
  forecasts %>%
    dplyr::select(region, forecast_date, variant_relationship, loo) %>%
    tidyr::pivot_wider(names_from = variant_relationship, values_from = loo) %>%
    mutate(loo = purrr::map2(
    scaled, correlated,
    ~ loo::loo_compare(.x, .y) %>%
        dplyr::as_tibble() %>%
        select(elpd_diff, se_diff) %>%
        dplyr::filter(elpd_diff != 0) %>%
        dplyr::mutate(across(c(elpd_diff, se_diff), round, 2)))
    ) %>%
    tidyr::unnest(loo) %>%
    dplyr::select(region, forecast_date, elpd_diff, se_diff)
}

save_results <- function(results, type, date) {
  path <- here::here("data", "estimates", "sgtf", date)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  purrr::walk2(
    results, names(results),
     ~ saveRDS(
        .x, file.path(path, paste0(.y, ".rds"))
      )
  )
  return(invisible(NULL))
}
