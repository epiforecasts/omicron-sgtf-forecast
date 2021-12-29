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
    filter(date < date_forecast_start) %>%
    mutate(value_type = "Omicron: current transmission advantage") %>%
    group_by(region) %>%
    slice_max(date) %>%
    ungroup()

  # growth
  growth <- summary(sgtf_posterior, type = "growth") %>%
    filter(type == "Omicron") %>%
    filter(date < date_forecast_start) %>%
    mutate(value_type = "Omicron: current growth rate") %>%
    group_by(region) %>%
    slice_max(date) %>%
    ungroup()

  # cumulative pop
  cumpercent <- cum_per %>%
    select(region, date, q5 = c_q5, q95 = c_q95,
           q20 = c_q20, q80 = c_q80
    ) %>%
    mutate(value_type = "Omicron: cumulative % population",
           region = as.character(region),
           type = "Omicron",
           across(starts_with("q"), ~ . * 100)) %>%
    filter(date < date_forecast_start) %>%
    group_by(region) %>%
    slice_max(date) %>%
    ungroup()

  # bias
  sampling <- summary(bias_posterior, type = "voc_advantage") %>%
    mutate(value_type = "Current bias: any-SGT vs no-SGT result") %>%
    filter(date < date_forecast_start) %>%
    group_by(region) %>%
    slice_max(date) %>%
    ungroup()

  # join --------------------------------------------------------------------
  region_factor <- as.character(sgtf_posterior$region)
  region_factor <- unique(region_factor)
  names(region_factor) <- region_factor

  reference_lines <- c("Omicron: current transmission advantage" = "1",
                       "Omicron: current growth rate" = "0",
                       "Omicron: cumulative population proportion" = "",
                       "Current bias: any-SGT vs no-SGT result" = "1")

  summary_data <- bind_rows(advantage, growth, cumpercent, sampling) %>%
    filter(
        type %in% c("Omicron", "SGT-result")
    ) %>%
    mutate(region = recode_factor(region, !!!region_factor),
           ref_line = as.numeric(recode(value_type, !!!reference_lines)))

  summary_plot <- summary_data %>%
    mutate(region = forcats::fct_rev(region)) %>%
    ggplot(aes(y = region)) +
    geom_linerange(aes(xmin = q5, xmax = q95), alpha = 0.3, size = 2) +
    geom_linerange(aes(xmin = q20, xmax = q80), alpha = 0.3, size = 2) +
    geom_vline(aes(xintercept = ref_line), lty = 2) +
    facet_wrap(~ value_type, scales = "free_x") +
    labs(y = NULL, x = NULL,
        caption = paste("Estimates using data available up to: ",
                        date_forecast_start))
  summary_plot <- forecast.vocs::plot_theme(summary_plot) +
    scale_x_continuous(labels = scales::label_number(accuracy = 0.1))

  return(summary_plot)
}
