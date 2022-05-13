library(here)
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(zoo)
library(ggplot2)


load_private_data <- function(path = "data/private", id_type = "sgtf", data_type = "specimen", clean = TRUE, start_date = NULL) {
  data_path <- here(path,
                    paste0(id_type, "_",
                           data_type,
                           "_england",
			   ifelse(clean, "_clean", ""),
			   ".csv"))

  regional_raw <- readr::read_csv(data_path, show_col_types = FALSE)

  if ("prop" %in% colnames(regional_raw)) {
    regional_raw <- regional_raw %>%
    select(-prop)
  }
  if (id_type == "omicron") {
    regional_raw <- regional_raw %>%
      rename(sgtf = Omicron, non_sgtf = `Omicron BA.2`)
  }
  regional <- regional_raw %>%
    rename(date = grep("date", names(.), value = TRUE),
           region = nhser_name,
           sgtf_unknown = "NA") %>%
    replace_na(list(sgtf_unknown = 0, non_sgtf = 0, sgtf = 0)) %>%
    mutate(total_sgt = non_sgtf + sgtf,
           total_cases = non_sgtf+ sgtf + sgtf_unknown) %>%
    mutate(region = tidyr::replace_na(region, "Unknown"),
           source = "private")

  if (!is.null(start_date)) {
    regional <- regional %>%
      filter(date >= start_date)
  }
  return(regional)
}
