# Get SGTF data and clean
library(here)
library(readr)
library(dplyr)
library(lubridate)
library(zoo)
library(ggplot2)

# England
data_path <- here("data", "private", "sgtf_onset_england.csv")
date_type <- ifelse(grepl("onset", data_path), "onset", "specimen")

daily_raw <- read_csv(data_path, show_col_types = FALSE) %>%
  rename(date = onsetdate) %>%
  mutate(location = "England") %>%
  rowwise() %>%
  mutate(total_sgt = sum(non_sgtf, sgtf, na.rm = TRUE),
         total_cases = sum(non_sgtf, sgtf, `NA`, na.rm = TRUE)) %>%
  ungroup()

truncate_days <- 3

plot_daily_cases <- daily_raw %>%
  filter(date >= max(date)-weeks(3) &
           date <= max(date) - truncate_days) %>%
  select(-c(prop, location)) %>%
  rename(sgtf_unknown = `NA`) %>%
  tidyr::pivot_longer(cols = c(non_sgtf, sgtf, sgtf_unknown),
                      names_to = "S-gene result") %>%
  ggplot(aes(x = date, y = value, fill = `S-gene result`)) +
  geom_col(position = "stack") +
  geom_line(aes(y = total_cases)) +
  labs(x = NULL, y = NULL,
       caption = paste("Excludes most recent", truncate_days, "days' reports")) +
  theme_bw() +
  theme(legend.position = "bottom")





