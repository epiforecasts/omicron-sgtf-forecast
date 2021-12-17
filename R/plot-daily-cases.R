library(ggplot2)
library(tidyr)
library(scales)

plot_daily_cases <- function(raw, caption) {
  sgtf_fills <- c("non_sgtf" = "#c994c7", "sgtf" = "#dd1c77",
                  "sgtf_unknown" = "#e7e1ef")
  raw %>%
    tidyr::pivot_longer(cols = c(non_sgtf, sgtf, sgtf_unknown),
                        names_to = "S-gene result") %>%
    ggplot(aes(x = date, y = value, fill = `S-gene result`)) +
    geom_col(position = "stack") +
    geom_line(aes(y = total_cases), col = "grey 20") +
    labs(x = NULL, y = NULL, caption = caption) +
    scale_fill_manual(values = sgtf_fills) +
    scale_y_continuous(labels = scales::comma) +
    theme_bw() +
    theme(legend.position = "bottom") +
    facet_wrap(~ region, scales = "free")
}
