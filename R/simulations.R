library(ggplot2)

plot_syn_scenarios <- function(sims) {
  ggplot(sims) +
    aes(x = time, y = r_sim, col = factor(alpha), group = factor(alpha)) +
    geom_hline(yintercept = 0, linetype = 2, col = "grey") +
    geom_line() +
    geom_line(aes(y = r), col = "black") +
    theme_bw() +
    theme(legend.position = "bottom") +
    scale_color_brewer(palette = "Dark2") +
    facet_grid(G_sd_v ~ G_v) +
    labs(x = "Day", y = "Daily growth rate", color = "Transmission advantage")
}