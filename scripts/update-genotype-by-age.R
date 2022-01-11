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


ep_omi <- ep_raw %>%
  filter(date_specimen > as.Date("2021-11-16"), pillar %in% "Pillar 2") %>%
  mutate(
    variant = case_when(
      grepl("confirmed", case_def, ignore.case = TRUE) ~ "omicron",
      variant_alt_name == "Lineage B.1.617.2 (India)" ~ "non_omicron",
      TRUE ~ "unknown"
    ),
    age_group =  cut(
      age, c(seq(0, 75, by = 15), Inf), include.lowest = TRUE,
      ordered_result = FALSE,
      labels = c(paste0(seq(0, 60, by = 15), "-", seq(14, 74, by = 15)), "75+")
      )
  ) %>%
  count(nhser_name, date_specimen, age_group, variant) %>%
  pivot_wider(names_from = "variant", values_from = "n") %>%
  mutate(across(where(is.numeric), replace_na, replace = 0)) %>%
  mutate(total = omicron + non_omicron + unknown,
         total_seq = omicron + non_omicron,
         share_omicron = omicron / total_seq
  )
    
all_age <- ep_omi %>%
  group_by(nhser_name, date_specimen) %>%
  summarise(across(where(is.numeric), sum), .groups = "drop") %>%
  mutate(age_group = "Overall" %>% as.factor(),
         share_omicron = omicron / non_omicron
        )

ep_combined_age <- bind_rows(ep_omi, all_age)

national <- ep_combined_age %>%
  group_by(age_group, date_specimen) %>%
  summarise(across(where(is.numeric), sum), .groups = "drop") %>%
  mutate(
    nhser_name = "England",
    share_omicron = omicron / non_omicron
  )

  ep_com <- bind_rows(national, ep_combined_age) %>%
    mutate()


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
