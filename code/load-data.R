# Get SGTF data and clean
library(here)
library(readr)
library(dplyr)
library(lubridate)
library(zoo)

# England
daily_raw <- read_csv(here("data", "private", "sgtf_daily_england.csv"),
                    show_col_types = FALSE) %>%
  rename(date = date_specimen) %>%
  mutate(location = "England") %>%
  rowwise() %>%
  mutate(total_sgt = sum(non_sgtf, sgtf, na.rm = TRUE),
         total_cases = sum(non_sgtf, sgtf, `NA`, na.rm = TRUE)) %>%
  ungroup()

available_date <- Sys.Date()
