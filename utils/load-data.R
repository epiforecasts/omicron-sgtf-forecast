# Get England SGTF data and clean
library(here)
library(readr)
library(dplyr)
library(lubridate)
library(zoo)
library(ggplot2)

# decisions:
# - which dataset to use: by onset or specimen date
date_type <- "specimen"
# - length of dataset
weeks_data <- 3
# - truncation
if (date_type == "onset") {
  truncate_days <- 3
} else {truncate_days <- 1}

# Get data
data_path <- here("data", "private",
                  paste0("sgtf_",
                         ifelse(date_type == "onset", date_type,
                                "daily"),
                         "_england.csv"))

regional <- read_csv(data_path, show_col_types = FALSE) %>%
  rename(date = grep("date", names(.), value = TRUE),
         nhs_region = nhser_name,
         sgtf_unknown = "NA") %>%
  rowwise() %>%
  mutate(total_sgt = sum(non_sgtf, sgtf, na.rm = TRUE),
         total_cases = sum(non_sgtf, sgtf, sgtf_unknown, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(nhs_region = tidyr::replace_na(nhs_region, "Unknown"))

# Cut off data before last 3 weeks and remove truncation
start_date <- max(regional$date) - weeks(weeks_data)
end_date <- max(regional$date) - days(truncate_days)
regional <- filter(regional,
                    between(date, start_date, end_date))

# england data
england <- regional %>%
  group_by(date) %>%
  summarise(across(c(total_cases, sgtf_unknown, total_sgt, sgtf, non_sgtf),
                     sum, na.rm = TRUE)) %>%
  mutate(prop = sgtf / total_sgt,
         nhs_region = "England")

daily_raw <- bind_rows(england, regional)

# optionally filter to only one region
if (exists("nhs_region_name")) {
  if (!is.na(nhs_region_name)) {
    daily_raw <- filter(daily_raw, nhs_region == nhs_region_name)}
}

# --------------------------------
#  Plot

sgtf_fills <- c("non_sgtf" = "#c994c7", "sgtf" = "#dd1c77",
           "sgtf_unknown" = "#e7e1ef")

plot_daily_cases <- daily_raw %>%
  select(-c(prop)) %>%
  tidyr::pivot_longer(cols = c(non_sgtf, sgtf, sgtf_unknown),
                      names_to = "S-gene result") %>%
  ggplot(aes(x = date, y = value, fill = `S-gene result`)) +
  geom_col(position = "stack") +
  geom_line(aes(y = total_cases), col = "grey 20") +
  labs(x = NULL, y = NULL,
       caption = paste("Data by", date_type,
                       "date \n Excludes most recent",
                       truncate_days, "days")) +
  scale_fill_manual(values = sgtf_fills) +
  theme_bw() +
  theme(legend.position = "bottom") +
  facet_wrap(~ nhs_region, scales = "free")


