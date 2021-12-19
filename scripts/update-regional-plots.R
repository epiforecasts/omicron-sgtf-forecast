# load packags
library(ggplot2)
library(scales)
library(dplyr)
library(here)
library(readr)

# load functions
source(here("R", "load-local-data.R"))

# load results
date <- get_latest_date()
sgtf <- load_results(date)
bias <- load_results(date, type = "bias")