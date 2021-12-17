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