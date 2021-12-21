# load packages
library(ggplot2)
library(dplyr)

# load functions
source(here("R", "load-local-data.R"))
source(here("R", "plot-cumulative-percent.R"))

plot_summary <- function(sgtf, bias, sgtf_cases,
                         date_forecast_start, date_data_start) {

# get each output ---------------------------------------------------------

  # omicron transmission advantage
  advantage <- simplify_posterior(sgtf$posterior, value_type = "voc_advantage") %>%
    mutate(value_type = "Omicron: transmission advantage")

  # growth
  growth <- simplify_posterior(sgtf$posterior, value_type = "growth",
                               variant_type = "Omicron") %>%
    mutate(value_type = "Omicron: Growth rate")

  # cumulative pop
  cumpercent <- simplify_posterior(sgtf$posterior, value_type = "cases",
                                   variant_type = "Omicron")
  cumpercent <- plot_cumulative_percent(cases = sgtf_cases,
                                        forecast_start = date_forecast_start,
                                        data_start = date_data_start)$data %>%
    mutate(value_type = "Omicron: cumulative population proportion",
           region = as.character(region))

  # bias
  sampling <- simplify_posterior(bias$posterior, value_type = "voc_advantage") %>%
    mutate(value_type = "Bias: transmission advantage of an S-gene result")

  # join --------------------------------------------------------------------
  region_factor <- as.character(sgtf$posterior$region)
  region_factor <- unique(region_factor)
  names(region_factor) <- region_factor

  summary_data <- bind_rows(advantage, growth, cumpercent, sampling) %>%
    filter(date == date_latest & type %in% c("Omicron", "SGT-result")) %>%
    mutate(region = recode_factor(region, !!!region_factor))

  summary_plot <- summary_data %>%
    ggplot(aes(y = region)) +
    geom_linerange(aes(xmin = q5, xmax = q95)) +
    geom_point(aes(x = median)) +
    facet_wrap(~ value_type, scales = "free_x") +
    labs(y = NULL, x = NULL)
  summary_plot <- forecast.vocs::plot_theme(summary_plot) +
    scale_x_continuous()

  return(summary_plot)
}
