library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(zoo)
library(forecast)

pop_link <- "https://coronavirus.data.gov.uk/downloads/supplements/ONS-population_2021-08-05.csv"
pop <- read_csv(pop_link) %>%
  filter(gender == "ALL" & category == "AGE_ONLY" &
           areaCode %in% unique(cases$areaCode)) %>%
  select(areaCode, age, population)

cases_link <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=region&metric=newCasesBySpecimenDateAgeDemographics&format=csv"
cases <- read_csv(cases_link) %>%
  left_join(pop, by = c("areaCode", "age")) %>%
  filter(between(date, as.Date("2021-10-01"), Sys.Date() - 4))

london <- filter(cases, areaName == "London") %>%
  filter(!grepl("\\+|00_59|unassigned", age)) %>%
  mutate(decade = as.numeric(substr(age, 1,1)),
         age_group = case_when(decade <= 1 ~ "0-19",
                               between(decade, 2, 3) ~ "20-39",
                               between(decade, 4, 5) ~ "40-59",
                               decade > 5 ~ "60+")) %>%
  group_by(areaName, date, age_group) %>%
  summarise(cases = sum(cases, na.rm = TRUE),
            population = sum(population),
            .groups = "drop") %>%
  mutate(cases_100k = cases / population * 100000,
         age_group = as.factor(age_group),
         week_day = factor(weekdays(date),
                           levels = c("Monday", "Tuesday", "Wednesday",
                                      "Thursday", "Friday", "Saturday",
                                      "Sunday"),
                           ordered = TRUE),
         iso_week = isoweek(date))


# % total weekly cases reported on each day

ci <- function(x, n) {
  qt(0.975, df = (n - 1) * sd(x) / sqrt(n))
}

london_week_day <- london %>%
  group_by(areaName, age_group, iso_week) %>%
  mutate(cases_total = sum(cases),
         n_days = n()) %>%
  ungroup() %>%
  filter(n_days == 7) %>%
  mutate(weekly_perc = cases / cases_total,
         # difference from expected (all days equal) to observed % each weekday
         weekly_perc_diff = (1/7) - weekly_perc)
# example
london_week_day %>%
  ggplot(aes(x = week_day, y = weekly_perc_diff, fill = iso_week)) +
  geom_col() +
  geom_hline(yintercept = 0, lty = 2) +
  labs(x = NULL, y = "Difference from evenly distributed daily cases") +
  facet_wrap(~ age_group, ncol = 1) +
  theme_bw() +
  theme(legend.position = "bottom")

# mean/CI
day_week_diffs <- Rmisc::group.CI(x = weekly_perc_diff ~ age_group + week_day,
                data = london_week_day, ci = 0.95)
day_week_diffs %>%
  ggplot(aes(x = week_day, col = age_group)) +
  geom_point(aes(y = weekly_perc_diff.mean)) +
  geom_linerange(aes(ymin = weekly_perc_diff.lower,
                     ymax = weekly_perc_diff.upper)) +
  geom_hline(yintercept = 0, lty = 2) +
  scale_colour_brewer(type = "div") +
  labs(x = NULL, y = "% difference",
       subtitle = "Difference from cases evenly distributed across a week",
       caption = paste("Cases by specimen date:",
                       min(london_week_day$date), "to",
                       max(london_week_day$date)),
       col = "Age group") +
  theme_bw() +
  theme(legend.position = "bottom")

