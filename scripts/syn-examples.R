library(here)
library(data.table)
library(ggplot2)
library(purrr)

# Load functions
source(here("R", "R_growth_approx.R"))
source(here("R", "simulations.R"))

# r to r mapping with strength intervention
r <- c(
  rep(0.3, 5), rep(0.1, 5), 0.1 - cumsum(1:15) * 0.005, rep(0.2, 5),
  rep(0, 5), rep(-0.1, 5), rep(-0.2), rep(0.2, 5)
)

# scenarios
scenarios <- CJ(
  alpha = c(0.6, 0.8, 1, 1.2, 1.4),
  G = c(4.6, 3.2),
  G_v = c(0.75, 1, 1.25),
  G_sd_v = c(0.75, 1, 1.25)
)
scenarios[, id := 1:.N]
scenarios[, gt_type := ifelse(G == 4.6, "Intrinsic", "Household")]
scenarios[, G_sd := ifelse(G == 4.6, 3.1, 2.1)]
scenarios[, G_sd_v := round(G_sd * G_sd_v, 1)][, G_v := round(G * G_v, 1)]

# simulations
sims <- map(
  split(scenarios, by = "id"),
  ~ data.table(
      time = 1:length(r),
      r = r,
      r_sim = r_to_r_diff_gt_sd_param(
        r, .$G[[1]], .$G_sd[[1]], .$G_v[[1]], .$G_sd_v[[1]],
        .$alpha[[1]]
      )
    )
)
sims <- rbindlist(sims, idcol = "id")
sims <- merge(scenarios, sims[, id := as.integer(id)], by = "id")

# save simulations
fwrite(
  sims, here("data", "simulations", "uk-scenario-simulations.csv")
)

# plot simulations for household gt assumption
plot_syn_scenarios(sims[gt_type %in% "Household"])
