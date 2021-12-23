# Cumulative cases as % population
library(dplyr)
library(ggplot2)
library(forecast.vocs)
library(readr)
source(here::here("R", "load-public-data.R"))

plot_cumulative_percent <- function(cases_pop, forecast_start, data_start) {

  plot_cumulative_pop <- cases_pop %>%
    ungroup() %>%
    mutate(region = factor(region, ordered = TRUE)) %>%
    ggplot(aes(x = date)) +
    geom_ribbon(aes(ymin = c_q5, ymax = c_q95), alpha = 0.3) +
    geom_ribbon(aes(ymin = c_q20, ymax = c_q80), alpha = 0.3) +
    geom_line(aes(y = c_median), alpha = 0.3, linetype = 2) +
    geom_vline(xintercept = forecast_start, lty = 5, lwd = 1, col = "black") +
    scale_y_continuous(labels = scales::label_percent()) +
    facet_wrap(~ region) +
    labs(x = NULL,
         y = "Cumulative % of the population with a reported Omicron case")

  plot_cumulative_pop <- forecast.vocs:::plot_theme(plot_cumulative_pop)
  return(plot_cumulative_pop)
}
