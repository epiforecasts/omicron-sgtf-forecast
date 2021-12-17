# Get England SGTF data and clean
library(here)
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(zoo)
library(ggplot2)

# decisions, with some defaults if not present in env
# - public or confidential data
if (!exists("open_data")) {open_data <- TRUE}
region_type <- if (open_data) {"PHE region"} else {"NHS region"}

# - which dataset to use: by onset or specimen date
if (!exists("date_type")) {date_type <- "specimen"}
if (date_type == "onset" & open_data) {
  date_type <- "specimen"
  cat("Public data are by specimen date")}

# - truncation
if (!exists("truncate_days")) {
  if (date_type == "onset") {truncate_days <- 3
  } else {truncate_days <- 1}}

# - length of dataset
if (!exists("weeks_data")) {weeks_data <- 3}


# Public data -------------------------------------------------------------
if (open_data) {
  # Get SGT data
  #TODO change this to dynamic date - can't see online file history so unclear what format yet
  data_path_sgt <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1041666/sgtf_regionepicurve_2021-12-15.csv"
  data_sgt <- read_csv(data_path_sgt) %>%
    mutate(date = dmy(specimen_date),
           sgtf_type = recode(sgtf,
                              "Cases with confirmed S-gene" = "non_sgtf",
                              "Cases with confirmed SGTF" = "sgtf")) %>%
    select(date, region = PHEC_name, sgtf_type, n, total_sgt = total) %>%
    pivot_wider(id_cols = c(date, region, total_sgt),
                names_from = sgtf_type, values_from = n) %>%
    mutate(sgtf = replace_na(sgtf, 0))

  # Get total case data by specimen date
  data_path_tot <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=region&metric=newCasesBySpecimenDate&format=csv"
  data_tot <- read_csv(data_path_tot) %>%
    select(date, region = areaName, total_cases = newCasesBySpecimenDate) %>%
    mutate(region = gsub("The ", "", region))

  # join
  regional <- left_join(data_sgt, data_tot, by = c("region", "date")) %>%
    mutate(sgtf_unknown = total_cases - total_sgt,
           source = "public")

}

# Private data, by specimen or onset --------------------------------------
if (!open_data) {
  # Get regional data
  data_path <- here("data", "private",
                    paste0("sgtf_",
                           ifelse(date_type == "onset", date_type,
                                  "daily"),
                           "_england.csv"))

  regional <- read_csv(data_path, show_col_types = FALSE) %>%
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

  rm(data_path)
}

# join all-england data ---------------------------------------------------
england <- regional %>%
  group_by(date) %>%
  summarise(across(c(total_cases, sgtf_unknown, total_sgt, sgtf, non_sgtf),
                   sum, na.rm = TRUE),
            source = source[1]) %>%
  mutate(region = "England")

daily_raw <- bind_rows(england, regional)

# remove "unknown" region (still counted to england total)
if ("Unknown" %in% unique(daily_raw$region)) {
  daily_raw <- filter(daily_raw,
                      !region == "Unknown")
}

# Common settings -----------------------------------------------------------
# Date cut offs
start_date <- max(daily_raw$date) - weeks(weeks_data)
end_date <- max(daily_raw$date) - days(truncate_days)
daily_raw <- filter(daily_raw,
                    between(date, start_date, end_date))
# Set england to come at top of plots
subregions <- unique(daily_raw$region)[!grepl("^England$", unique(daily_raw$region))]
daily_raw <- mutate(daily_raw,
                    region = factor(region,
                                    c("England", subregions)))
# Filter to region, optional
if (exists("region_name")) {
  if (!is.na(region_name)) {
    daily_raw <- filter(daily_raw, region == region_name)}
}

# Plot --------------------------------
sgtf_fills <- c("non_sgtf" = "#c994c7", "sgtf" = "#dd1c77",
           "sgtf_unknown" = "#e7e1ef")

plot_daily_cases <- daily_raw %>%
  tidyr::pivot_longer(cols = c(non_sgtf, sgtf, sgtf_unknown),
                      names_to = "S-gene result") %>%
  ggplot(aes(x = date, y = value, fill = `S-gene result`)) +
  geom_col(position = "stack") +
  geom_line(aes(y = total_cases), col = "grey 20") +
  labs(x = NULL, y = NULL,
       caption = paste("data are", open_data, "by", date_type,
                       "date \n excludes most recent",
                       truncate_days, "days")) +
  scale_fill_manual(values = sgtf_fills) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() +
  theme(legend.position = "bottom") +
  facet_wrap(~ region, scales = "free")

ggsave(here("transmission", "figures", "daily-cases.png"),
       plot_daily_cases, dpi = "print", width = 7, height = 7)
