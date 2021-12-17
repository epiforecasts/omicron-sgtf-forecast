library(here)
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(zoo)
library(ggplot2)


load_private_date <- function(path = "data/private", data_type = "specimen") {
  data_path <- here(path,
                    paste0("sgtf_",
                           ifelse(data_type == "onset", data_type,
                                  "daily"),
                           "_england.csv"))

  regional_raw <- readr::read_csv(data_path, show_col_types = FALSE)

  regional <- regional_raw %>%
    select(-prop) %>%
    rename(date = grep("date", names(.), value = TRUE),
           region = nhser_name,
           sgtf_unknown = "NA") %>%
    rowwise() %>%
    mutate(total_sgt = sum(non_sgtf, sgtf, na.rm = TRUE),
           total_cases = sum(non_sgtf, sgtf, sgtf_unknown, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(region = tidyr::replace_na(region, "Unknown"),
           source = "private")
  return(regional)
}