# Create a summary plot for each region
# Key measures for the latest date before forecast
# load packages
library(ggplot2)
library(scales)
library(dplyr)
library(here)
library(readr)

# Set date for summary
date_latest


# omicron transmission advantage
advantage <- simplify_posterior(sgtf$posterior, value_type = "voc_advantage") %>%
  mutate(value_type = "Transmission advantage of Omicron")

# growth
growth <- simplify_posterior(sgtf$posterior, value_type = "growth_from_rt") %>%
  mutate(value_type = "Growth")

# date at which Omicron accounts for 95% all cases
voc_frac <- simplify_posterior(sgtf$posterior, value_type = "voc_frac")
omicron95 <- plot_omicron_95(voc_frac = voc_frac,
                             forecast_start = date_forecast_start,
                             forecast_end = date_forecast_end)$data %>%
  mutate(value_type = "Omicron at 95% cases",
         date = date_latest,
         region = as.character(region))

# cumulative pop
cases <- simplify_posterior(sgtf$posterior, value_type = "cases") %>%
  filter(type == "Omicron")
cumpercent <- plot_cumulative_percent(cases = cases,
                                      forecast_start = date_forecast_start,
                                      data_start = date_data_start)$data %>%
  mutate(value_type = "Omicron cumulative % population",

         region = as.character(region))

# bias
sampling <- simplify_posterior(bias$posterior, value_type = "voc_advantage") %>%
  mutate(value_type = "Transmission advantage of S-gene result")

# Join summary data
region_factor <- unique(sgtf$posterior$region)
names(region_factor) <- region_factor

summary_data <- bind_rows(advantage, growth, cumpercent, sampling) %>%
  mutate(region = recode_factor(region, !!!region_factor)) %>%
  filter(date == date_latest & !type %in% c("Combined", "non-Omicron")) %>%
  select(region, value_type, type, median, q5, q95)

summary_plot <- summary_data %>%
  ggplot(aes(y = region)) +
  geom_linerange(aes(xmin = q5, xmax = q95)) +
  geom_point(aes(x = median)) +
  facet_wrap(~ value_type, scales = "free_x") +
  labs(y = NULL, x = NULL)

forecast.vocs::plot_theme(summary_plot)

