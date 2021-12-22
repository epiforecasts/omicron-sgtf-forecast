# load packages
library(ggplot2)
library(dplyr)

# load functions
source(here("R", "load-local-data.R"))
source(here("R", "plot-cumulative-percent.R"))

plot_summary <- function(sgtf_posterior, cum_per, bias_posterior,
                         date_forecast_start) {

# get each output ---------------------------------------------------------

  # omicron transmission advantage
  advantage <- summary(sgtf_posterior, type = "voc_advantage") %>%
    mutate(value_type = "Omicron: transmission advantage")

  # growth
  growth <- summary(sgtf_posterior, type = "growth") %>%
    filter(type == "Omicron") %>%
    mutate(value_type = "Omicron: growth rate")

  # cumulative pop
  cumpercent <- cum_per %>%
    select(region, date, median = c_median, q5 = c_q5, q95 = c_q95) %>%
    mutate(value_type = "Omicron: cumulative % population",
           region = as.character(region),
           type = "Omicron",
           across(median:q95, ~ . * 100))

  # bias
  sampling <- summary(bias_posterior, type = "voc_advantage") %>%
    mutate(value_type = "Bias: any-SGT vs no-SGT result")

  # join --------------------------------------------------------------------
  region_factor <- as.character(sgtf$posterior$region)
  region_factor <- unique(region_factor)
  names(region_factor) <- region_factor

  reference_lines <- c("Omicron: transmission advantage" = "1",
                       "Omicron: growth rate" = "0",
                       "Omicron: cumulative population proportion" = "",
                       "Bias: any-SGT vs no-SGT result" = "1")

  summary_data <- bind_rows(advantage, growth, cumpercent, sampling) %>%
    filter(date == date_forecast_start & type %in% c("Omicron", "SGT-result")) %>%
    mutate(region = recode_factor(region, !!!region_factor),
           ref_line = as.numeric(recode(value_type, !!!reference_lines)))

  summary_plot <- summary_data %>%
    ggplot(aes(y = region)) +
    geom_linerange(aes(xmin = q5, xmax = q95)) +
    geom_point(aes(x = median)) +
    geom_vline(aes(xintercept = ref_line), lty = 2) +
    facet_wrap(~ value_type, scales = "free_x") +
    labs(y = NULL, x = NULL, caption = paste("Estimates for", date_forecast_start))
  summary_plot <- forecast.vocs::plot_theme(summary_plot) +
    scale_x_continuous(labels = scales::label_number(accuracy = 0.1))

  return(summary_plot)
}
