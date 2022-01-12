library(here)
library(data.table)
library(ggplot2)
library(purrr)

# Load functions
source(here("R", "R_growth_approx.R"))

# r to r mapping with strength intervention
r <- c(
  rep(0.3, 5), rep(0.1, 5), 0.1 - cumsum(1:15) * 0.005, rep(0.2, 5),
  rep(0, 5), rep(0.2, 5)
)

# scenarios
scenarios <- CJ(
  alpha = c(0.6, 0.8, 1, 1.2, 1.4),
  G = c(4.6, 3.2),
  G_v = c(0.75, 1, 1.25),
  k_v = c(0.75, 1, 1.25)
)
scenarios[, id := 1:.N]
scenarios[, gt_type := ifelse(G == 4.6, "Intrinsic", "Household")]
scenarios[, k := ifelse(G == 4.6, (3.1 / 4.6) ^ 2, (2.1 / 3.22) ^ 2)]

# simulations
sims <- map(
  split(scenarios, by = "id"),
  ~ data.table(
      time = 1:length(r),
      r = r,
      r_sim = r_to_r_diff_gt(
        r, .$G[[1]], .$k[[1]], .$G[[1]] * .$G_v[[1]], .$k[[1]] * .$k_v[[1]],
        .$alpha[[1]]
      )
    )
)
sims <- rbindlist(sims, idcol = "id")
sims <- merge(scenarios, sims[, id := as.integer(id)], by = "id")

fwrite(
  sims, here("data", "simulations", "uk-scenario-simulations.csv")
)

plot_syn_scenarios(sims[gt_type %in% "Household"])

plot_syn_scenarios <- function(sims) {
  
}
sims[gt_type %in% "Household"] |>
  ggplot() +
  aes(x = time, y = r_sim, col = factor(alpha), group = factor(alpha)) +
  geom_hline(yintercept = 0, linetype = 2, col = "grey") +
  geom_line() +
  geom_line(aes(y = r), col = "black") +
  theme_bw() +
  theme(legend.position = "bottom") +
  facet_grid(k_v ~ G_v) +
  labs(x = "Day", y = "Daily growth rate", color = "Transmission advantage")