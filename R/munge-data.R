# Get England SGTF data and clean
library(here)
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(zoo)
library(ggplot2)


add_england_totals <- function(regional) {
  england <- regional %>%
    group_by(date) %>%
    summarise(across(c(total_cases, sgtf_unknown, total_sgt, sgtf, non_sgtf),
                    sum, na.rm = TRUE),
              source = source[1]) %>%
    mutate(region = "England")

  daily_raw <- bind_rows(england, regional)
  return(daily_raw)
}

tidy_data <- function(regional, region = NULL) {
  # remove "unknown" region (still counted to england total)
  if ("Unknown" %in% unique(regional$region)) {
    regional <- filter(regional, !region == "Unknown")
  }

  subregions <- unique(regional$region)[
    !grepl("^England$", unique(regional$region))
  ]
  regional <- regional %>%
    mutate(region = factor(region, c("England", subregions)))

  # Filter to region, optional
  if (!is.null(region)) {
    region_name <- region
    if (!is.na(region_name)) {
      regional <- filter(regional, region == region_name)}
  }
  return(regional)
}

truncate_cases <- function(regional, days = 0) {
  case_end_date <- max(regional$date) - lubridate::days(days)
  regional <- regional %>%
    mutate(total_cases = ifelse(date > case_end_date, NA, total_cases),
          sgtf_unknown = ifelse(date > case_end_date, NA, sgtf_unknown))
  return(regional)
}

sgtf_data_to_fv <- function(obs) {
  obs %>%
    transmute(region = region,
              date = date,
              cases = total_cases,
              cases_available = date,
              seq_total = total_sgt,
              seq_voc = sgtf,
              share_voc = sgtf / total_sgt,
              seq_available = date)
}

bias_data_to_fv <- function(obs) {
  obs %>%
    transmute(region = region,
              date = date,
              cases = total_cases,
              seq_total = total_cases,
              seq_voc = total_sgt,
              share_voc = total_sgt / total_cases,
              cases_available = date,
              seq_available = date)
}