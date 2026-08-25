library(ggplot2)
library(tidyr)
library(scales)

plot_seq_with_cases <- function(raw) {


  seq_fills <- c("Omicron" = "#c994c7", "non-Omicron" = "#dd1c77",
                 "Unknown" = "#e7e1ef")

  plot <- raw %>%
    rename(omicron_unknown = unknown) %>%
    pivot_longer(cols = contains("omicron"), names_to = "Variant") %>%
    mutate("Variant" = factor(
      `Variant`,
       levels = c("omicron", "non_omicron", "omicron_unknown"),
       labels = c("Omicron", "non-Omicron", "Unknown"))) %>%
    ggplot(aes(x = date_specimen, y = value, fill = `Variant`)) +
    geom_col(position = "stack")

    plot <- plot +
    labs(x = "Specimen date", y = "Notified pillar 2 cases") +
    scale_fill_manual(values = seq_fills) +
    scale_y_continuous(labels = scales::comma) +
    theme_bw() +
    theme(legend.position = "bottom")
    return(plot)
}
