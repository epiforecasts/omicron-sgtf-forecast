# Get England SGTF data and clean
library(here)
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(zoo)
library(ggplot2)

source(here::here("R", "load-public-data.R"))
source(here::here("R", "load-private-data.R"))
source(here::here("R", "munge-data.R"))

load_data <- function(open_data = TRUE, data_type = "specimen", ...) {
  if (open_data) {
    if (data_type == "onset") {
      stop("Public data are by specimen date only")
    }
    regional <- load_public_data()
  } else {
    regional <- load_private_data(data_type = data_type, ...)
  }
  regional <- add_england_totals(regional)
  return(regional)
}
