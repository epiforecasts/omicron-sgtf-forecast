# Get England SGTF data and clean
library(here)
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)


add_england_totals <- function(regional) {
  england <- regional %>%
    group_by(date) %>%
    summarise(across(c(total_cases, sgtf_unknown, total_sgt, sgtf, non_sgtf),
                    sum, na.rm = TRUE),
              source = source[1]) %>%
    mutate(region = "England") %>%
    mutate(total_sgt = ifelse(total_sgt == 0, NA, total_sgt)) %>%
    mutate(
      sgtf = ifelse(is.na(total_sgt), NA, sgtf),
      non_sgtf = ifelse(is.na(total_sgt), NA, non_sgtf)
    )

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
          sgtf_unknown = ifelse(date > case_end_date, NA, sgtf_unknown)) %>%
  return(regional)
}

truncate_sequences <- function(regional, start_date) {
  regional <- regional %>%
    mutate(sgtf = ifelse(date < start_date, NA, sgtf),
           total_sgt = ifelse(date < start_date, NA, total_sgt)) %>%
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
              seq_available = date) %>%
    filter(!is.na(seq_total)) %>%
    mutate(seq_total = ifelse(is.na(seq_voc), NA, seq_total))
}

cumulative_percentage <- function(cases, pop) {
  cases_pop <- cases %>%
    left_join(pop, by = c("region")) %>%
    group_by(region) %>%
    arrange(date) %>%
    mutate(c_median = cumsum(median) / population,
           c_q5 = cumsum(q5) / population,
           c_q95 = cumsum(q95) / population,
           c_q20 = cumsum(q20) / population,
           c_q80 = cumsum(q80) / population)
  return(cases_pop)
}