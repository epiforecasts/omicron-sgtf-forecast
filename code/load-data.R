# Get SGTF data and clean
library(here)
library(readr)
library(dplyr)
library(lubridate)

# England
daily <- read_csv(here("data", "private", "sgtf_daily_england.csv"),
                    show_col_types = FALSE) %>%
  rename(date = date_specimen) %>%
  mutate(location = "England") %>%
  rowwise() %>%
  mutate(total = sum(non_sgtf, sgtf, na.rm = TRUE)) %>%
  ungroup()

max_date_eng <- max(daily$date)

# Format
# - required variables:
#   cases, seq_voc, seq_total, date, cases_available, seq_available
daily <- daily %>%
  transmute(date = date,
            cases = total,
            cases_available = date,
            seq_total = total,
            seq_voc = sgtf,
            share_voc = sgtf / total,
            seq_available = date)

# Join and aggregate to weekly
weekly <- daily %>%
  mutate(week = ceiling_date(date, unit = "week",
                             week_start = wday(max_date_eng))) %>%
  group_by(week) %>%
  summarise(cases = sum(cases, na.rm = TRUE),
            seq_voc = sum(seq_voc, na.rm = TRUE),
            share_voc = seq_voc / cases,
            date = max(week, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(seq_total = cases,
         cases_available = date,
         seq_available = date,
         week = NULL)
