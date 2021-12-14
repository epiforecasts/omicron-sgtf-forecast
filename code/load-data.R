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

# Format required variables:
#   cases, seq_voc, seq_total, date, cases_available, seq_available
daily <- daily_raw %>%
  transmute(date = date,
            cases = total_cases,
            cases_available = date,
            seq_total = total_sgt,
            seq_voc = sgtf,
            share_voc = sgtf / total_sgt,
            seq_available = date)

# Remove day-of-week in daily data for all cases (not applied to SGTF)
daily_detrend <- daily %>%
  mutate(cases = zoo::rollmean(cases, k = 7, align = "center", fill = NA),
         share_voc = seq_voc / seq_total) %>%
  filter(!is.na(cases))
