# Get England SGTF data and clean
library(here)
library(readr)
library(dplyr)
library(tidyr)

source(here::here("R", "munge-data.R"))

load_local_data <- function(date = Sys.Date(), path = "data/public") {
  cases <- read_csv(here::here(path, paste0(date, "-cases-by-sgtf.csv")))
  cases <- tidy_data(cases)
  return(cases)
}

get_available_dates <- function(path = "data/public/") {
  csv_obs <- list.files(here::here(path))
  dates <- gsub("-cases-by-sgtf.csv", "", csv_obs)
  dates <- as.Date(dates)
  return(dates)
}

get_latest_date <- function(path = "data/public/") {
  dates <- get_available_dates(path = path)
  date <- max(dates)
  return(date)
}

load_results <- function(date, type = "sgtf", path = "data/estimates") {
  path <- here::here(path, type, date)
  files <- list.files(path)
  file_names <- gsub(".rds", "", files)
  results <- purrr::map(files, ~ readRDS(file.path(path, .)))
  names(results) <- file_names
  if (!is.null(results$posterior)) {
    class(results$posterior) <- c("fv_posterior", class(results$posterior))
    # Re-define growth using Rt - 1
    growth_from_rt <- filter(results$posterior, value_type == "rt") %>%
      mutate(across(mean:q95, ~ (. - 1)),
             value_type = "growth_from_rt")
    results$posterior <- bind_rows(results$posterior, growth_from_rt)
  }
  return(results)
}

simplify_posterior <- function(posterior, value_type,
                               dates_between = NULL,
                               variant_relationship = "correlated") {
  results <- posterior %>%
    filter(value_type %in% !!value_type &
             variant_relationship %in% !!variant_relationship) %>%
    select(value_type, date, region, type, mean, median, q5, q20, q80, q95)

  if (length(dates_between) == 2) {
    results <- filter(results, between(date,
                                       min(dates_between), max(dates_between)))
  }
  return(results)
}
