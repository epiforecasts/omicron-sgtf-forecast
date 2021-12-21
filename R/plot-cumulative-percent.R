# Cumulative cases as % population
library(dplyr)
library(ggplot2)
library(forecast.vocs)
library(readr)
source(here::here("R", "load-public-data.R"))

plot_cumulative_percent <- function(cases, forecast_start, data_start) {

  pop <- read_csv("https://coronavirus.data.gov.uk/downloads/supplements/ONS-population_2021-08-05.csv") %>%
    filter(grepl("^E120", areaCode) & category == "ALL") %>%
    select(region_code = areaCode, population) %>%
    full_join(load_public_case_data(), by = "region_code") %>%
    filter(date == max(date)) %>%
    select(region, population) %>%
    bind_rows(summarise(., population = sum(population), region = "England"))

  cases_pop <- cases %>%
    left_join(pop, by = c("region")) %>%
    group_by(region, type) %>%
    arrange(date) %>%
    mutate(c_median = cumsum(median) / population,
           c_q5 = cumsum(q5) / population,
           c_q95 = cumsum(q95) / population)

  plot_cumulative_pop <- cases_pop %>%
    ggplot(aes(x = date, fill = type, colour = type)) +
    geom_ribbon(aes(ymin = c_q5, ymax = c_q95), alpha = 0.7) +
    geom_vline(xintercept = forecast_start, lty = 5, lwd = 1, col = "black") +
    scale_y_continuous(labels = scales::label_percent()) + #, limits = c(0,0.01)
    facet_wrap(~ region, scales = "free_y") +
    labs(x = NULL, y = "% population",
         fill = "Variant", colour = "Variant",
         caption = paste("Cumulative since", format(data_start, "%d %b %y")))

  plot_cumulative_pop <- forecast.vocs:::plot_theme(plot_cumulative_pop)
  return(plot_cumulative_pop)
}
