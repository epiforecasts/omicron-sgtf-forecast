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
  csv_obs <- list.files(path)
  dates <- gsub("-cases-by-sgtf.csv", "", csv_obs)
  dates <- as.Date(dates)
  return(dates)
}

get_latest_date <- function(path = "data/public/") {
  dates <- get_available_dates(path = path)
  date <- max(dates)
  return(date)
}