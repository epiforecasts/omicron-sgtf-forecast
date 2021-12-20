# Cases by 100k population
library(dplyr)
library(ggplot2)
library(forecast.vocs)
library(readr)

source(here::here("R", "load-public-data.R"))

plot_inc_100k <- function(cases, forecast_start) {
  pop <- read_csv("https://coronavirus.data.gov.uk/downloads/supplements/ONS-population_2021-08-05.csv") %>%
    filter(grepl("^E120", areaCode) & category == "ALL") %>%
    select(region_code = areaCode, population) %>%
    full_join(load_public_case_data(), by = "region_code") %>%
    filter(date == max(date)) %>%
    select(region, population) %>%
    bind_rows(summarise(., population = sum(population), region = "England"))

  cases_pop <- cases %>%
    left_join(pop, by = c("region")) %>%
    mutate(p_median = median / population,
           p_q5 = q5 / population,
           p_q95 = q95 / population,
           p100k_median = median / (population / 100000),
           p100k_q5 = q5 / (population / 100000),
           p100k_q95 = q95 / (population / 100000))

  plot_inc_100k <- cases_pop %>%
    filter(p100k_q95 < 100000) %>%
    ggplot(aes(x = date, fill = type)) +
    geom_ribbon(aes(ymin = p100k_q5, ymax = p100k_q95), alpha = 0.7) +
    geom_vline(xintercept = forecast_start, lty = 2, lwd = 1) +
    facet_wrap(~ region, scales = "free_y") +
    labs(x = NULL, y = "Notifications per 100,000",
         fill = "Variant")

  plot_inc_100k <- forecast.vocs:::plot_theme(plot_inc_100k)
  return(plot_inc_100k)
}
