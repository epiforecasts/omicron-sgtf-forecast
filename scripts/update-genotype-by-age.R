# This script requires the english_pillars_raw.rds file to be present in
# data/private
# It takes this linelist and produces aggregated data by SGTF status, age and
# region

# Load packages
library(data.table)
library(dplyr)
library(here)
library(readr)
library(lubridate)
library(janitor)
library(tidyr)

ep_raw <- readRDS(here::here("data", "private", "english_pillars_raw.rds"))

sgtf_all <- ep_raw %>%
  count(date_specimen, nhser_name, sgtf_under30ct) %>%
  mutate(sgtf_under30ct = recode(sgtf_under30ct, `1` = "sgtf", `0` = "non_sgtf")) %>%
  pivot_wider(names_from = "sgtf_under30ct", values_from = "n") %>%
  replace_na(list(sgtf = 0, non_sgtf = 0, `NA` = 0)) %>%
  mutate(prop = sgtf / (non_sgtf + sgtf))
  
ep_omi <- ep_raw %>%
  filter(is.na(case_def) | grepl("confirmed", case_def, ignore.case = TRUE)) %>%
  mutate(omicron = if_else(is.na(case_def), "non-omicron", "confirmed")) %>%
  count(nhser_name, date_specimen, omicron) %>%
  pivot_wider(names_from = "omicron", values_from = "n") %>%
  replace_na(list(confirmed = 0, probable = 0, possible = 0))


 ep_omi_clean <- ep_raw %>%
  filter(!is.na(onsetdate), pillar == "Pillar 2") %>%
  filter(is.na(case_def) | grepl("confirmed", case_def, ignore.case = TRUE)) %>%
  mutate(omicron = if_else(is.na(case_def), "non-omicron", "confirmed")) %>%
  filter((is.na(overall_travel) | !(overall_travel == "Yes"))) %>%
  count(nhser_name, date_specimen, omicron) %>%
  pivot_wider(names_from = "omicron", values_from = "n") %>%
  replace_na(list(confirmed = 0, probable = 0, possible = 0))

# Load pillars data
english_pillars <- readRDS(
  here("data", "private", "english_pillars.rds")
) %>%
  as_tibble()

# Summarise to aggregate counts by NHSE region
sgtf_by_region_and_age <- english_pillars %>%
  filter(!is.na(age_group)) %>%
  select(-lower_age_limit, -positive, -total) %>%
  group_by(pillar, date_specimen, nhser_name, age_group) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  ungroup() %>%
  mutate(total = sgene_n_a + sgene_positive + sgene_negative,
         total_sgt = sgene_positive + sgene_negative) %>%
  select(-lft_flag, -negative, date = date_specimen, region = nhser_name,
         age_group, total_cases = total, sgtf_unknown = sgene_n_a,
         total_sgt, sgtf = sgene_negative, non_sgtf = sgene_positive)

# Summarise for England
sgtf_by_age <- sgtf_by_region_and_age %>%
  group_by(pillar, date, age_group) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  ungroup() %>%
  mutate(region = "England")

# Combined regions + national
sgtf_by_region_and_age <- bind_rows(
  sgtf_by_region_and_age, sgtf_by_age
)

# Filter for just pillar 2
sgtf_by_region_and_age <- sgtf_by_region_and_age %>%
  filter(pillar == 2)

# Save as csv
write_csv(sgtf_by_region_and_age,
          here("data", "private", "sgtf-by-region-and-age.csv")
)
