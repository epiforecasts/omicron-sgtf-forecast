# load packags
library(ggplot2)
library(scales)
library(dplyr)
library(here)
library(readr)

# takes a dataframe voc_frac : date, region, median, q5, q95 estimates

# Earliest date at 95% Omicron ---------------------------------------
plot_omicron_95 <- function(voc_frac, forecast_start, forecast_end) {

  omicron_95 <- voc_frac %>%
    select(date, region, q_median = median, q5, q95) %>%
    pivot_longer(cols = starts_with("q"),
                 names_to = "quantile", values_to = "omicron_prop") %>%
    group_by(region, quantile) %>%
    filter(omicron_prop >= 0.95) %>%
    slice_min(date) %>%
    select(region, quantile, date) %>%
    pivot_wider(id_cols = region, names_from = quantile, values_from = date) %>%
    ungroup() %>%
    arrange(q_median) %>%
    mutate(q5 = as.Date(ifelse(is.na(q5), forecast_end, q5),
                        origin = lubridate::origin),
           region = factor(region, ordered = TRUE))

  plot_95_percent <- omicron_95 %>%
    ggplot(aes(y = region)) +
    geom_linerange(aes(xmin = q5, xmax = q95)) +
    geom_point(aes(x = q_median)) +
    geom_vline(xintercept = forecast_start, lty = 3) +
    xlim(x = c(forecast_start - 1, forecast_end)) +
    labs(y = NULL, x = "Date that 95% cases are Omicron")

  plot_95_percent <- forecast.vocs:::plot_theme(plot_95_percent)
  return(plot_95_percent)
}

