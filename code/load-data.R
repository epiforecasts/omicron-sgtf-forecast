# Get SGTF data and clean
library(here)
library(readr)
library(dplyr)
library(lubridate)

# Load daily data (private, local)

# England
england <- read_csv(here("data", "private", "sgtf_weekly_england.csv"),
                    show_col_types = FALSE) %>%
  rename(date = date_specimen) %>%
  mutate(location = "England")

# scotland <- read_tsv(here("data", "private", "lh-sgtf.tsv")) %>%
#   group_by(date) %>%
#   summarise(sgtf = sum(c_across(ends_with("sgtf"))),
#             non_sgtf = sum(c_across(ends_with("pos"))),
#             location = "Scotland",
#             .groups = "drop") %>%
#   left_join(select(england, date, week_ending))

# Join and aggregate to weekly
if (exists("scotland")) {data <- bind_rows(england, scotland)
} else {data <- england}
data <- data %>%
  mutate(epiyear = epiyear(date),
         epiweek = epiweek(date)) %>%
  group_by(epiyear, epiweek) %>%
  summarise(total = sum(non_sgtf, sgtf, na.rm = TRUE),
            sgtf = sum(sgtf, na.rm = TRUE),
            non_sgtf = sum(non_sgtf, na.rm = TRUE),
            n_days = max(date) - min(date) + 1,
            date = max(week_ending, na.rm = TRUE), # week may be incomplete
            .groups = "drop")

# # truncation:
# data <- data %>%
#  filter(n_days == 7)

# Format
# - required variables:
#   cases, seq_voc, seq_total, date, cases_available, seq_available
obs <- data %>%
  transmute(date = date,
            cases = total,
            cases_available = date,
            seq_total = total,
            seq_voc = sgtf,
            share_voc = sgtf / total,
            seq_available = date)
obs <- data.table::data.table(obs)
