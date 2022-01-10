library(ggplot2)
library(tidyr)
library(scales)

plot_daily_cases <- function(raw, caption,
                             truncate_date, start_date, smooth_total = TRUE,
                             by = "region") {

  if (smooth_total) {
    smooth <- raw %>%
      filter(date < as.Date(truncate_date)) %>%
      filter(!is.na(total_cases)) %>%
      arrange(date) %>%
      group_by(.data[[by]]) %>%
      mutate(total_cases_smooth = zoo::rollmean(total_cases, k = 7,
                                         align = "center", fill = NA)) %>%
      ungroup() %>%
      select(date, {{ by }}, total_cases_smooth)
    raw <- raw %>%
      select(-total_cases) %>%
      left_join(smooth, by = c("date", by)) %>%
      mutate(total_cases = total_cases_smooth)
  }

  sgtf_fills <- c("Detected" = "#c994c7", "Failure" = "#dd1c77",
                  "Unknown" = "#e7e1ef")

  raw %>%
    filter(date >= start_date) %>%
    pivot_longer(cols = contains("sgtf"), names_to = "S-gene result") %>%
    mutate("S-gene result" = factor(`S-gene result`,
                            levels = c("sgtf", "non_sgtf", "sgtf_unknown"),
                            labels = c("Failure", "Detected", "Unknown"))) %>%
    ggplot(aes(x = date, y = value, fill = `S-gene result`)) +
    geom_col(position = "stack") +
    geom_line(aes(y = total_cases), col = "grey 20") +
    geom_vline(xintercept = as.Date(truncate_date), lty = 2) +
    labs(x = NULL, y = NULL, caption = caption) +
    scale_fill_manual(values = sgtf_fills) +
    scale_y_continuous(labels = scales::comma) +
    theme_bw() +
    theme(legend.position = "bottom")
}
