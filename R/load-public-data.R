# Get England SGTF data and clean
library(here)
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)

load_public_sgtf_data <- function() {
  data_path_sgt <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1041666/sgtf_regionepicurve_2021-12-15.csv" # nolint

   raw_data_sgt <- read_csv(data_path_sgt)

   data_sgt <- raw_data_sgt %>%
    mutate(date = dmy(specimen_date),
           sgtf_type = recode(sgtf,
                              "Cases with confirmed S-gene" = "non_sgtf",
                              "Cases with confirmed SGTF" = "sgtf")) %>%
    select(date, region = UKHSA_region, sgtf_type, n, total_sgt = total) %>%
    pivot_wider(id_cols = c(date, region, total_sgt),
                names_from = sgtf_type, values_from = n) %>%
    mutate(sgtf = replace_na(sgtf, 0))
  return(data_sgt)
}

load_public_case_data <- function() {
  data_path_tot <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=region&metric=newCasesBySpecimenDate&format=csv" # nolint
  raw_data_tot <- read_csv(data_path_tot)

  data_tot <- raw_data_tot %>%
    select(date, region_code = areaCode, region = areaName,
           total_cases = newCasesBySpecimenDate) %>%
    mutate(region = gsub("The ", "", region))
  return(data_tot)
}

link_public_data <- function(sgt, cases) {
  regional <- full_join(sgt, cases, by = c("region", "date")) %>%
    mutate(sgtf_unknown = ifelse(is.na(total_sgt), total_cases,
                                 total_cases - total_sgt),
           source = "public")
  return(regional)
}

load_public_data <- function() {
  sgt <- load_public_sgtf_data()
  cases <- load_public_case_data()
  linked_data <- link_public_data(sgt, cases)
  return(linked_data)
}

load_population <- function() {
  read_csv("https://coronavirus.data.gov.uk/downloads/supplements/ONS-population_2021-08-05.csv") %>%
    filter(grepl("^E120", areaCode) & category == "ALL") %>%
    select(region_code = areaCode, population) %>%
    full_join(load_public_case_data(), by = "region_code") %>%
    filter(date == max(date)) %>%
    select(region, population) %>%
    bind_rows(summarise(., population = sum(population), region = "England"))
}