# This script requires the english_pillars.rds file to be present in
# data/private
# It takes this linelist and produces aggregated data by SGTF status, age and
# region

# Load packages
library(dplyr)
library(tidyr)
library(readr)
library(here)

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
write_csv(here("data", "private", "sgtf-by-region-and-age.csv"))
