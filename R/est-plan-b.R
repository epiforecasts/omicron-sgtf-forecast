est_plan_b <- function(pp_samples, by = c()) {
  plan_b_diff <- pp_samples[
    date == "2021-12-9" | date == "2021-12-17"
  ]

  plan_b_diff <- dcast(plan_b_diff, ... ~ date, value.var = "value")
  plan_b_diff[, `:=`(
      Additive = `2021-12-09` - `2021-12-17`,
      Multiplicative = `2021-12-17` / `2021-12-09`
    )
  ][,
    `2021-12-09` := NULL][,
    `2021-12-17` := NULL]
  plan_b_diff <- melt(
    plan_b_diff, measure.vars = c("Additive", "Multiplicative"),
    variable.name = "Effect")
  round_quantile <- function(value, p, digits = 2) {
    q <- quantile(value, probs = p, na.rm = TRUE)
    q <- round(q, digits)
    return(q)
  }

  by_cols <- c(by, "Effect")

  plan_b_diff <- plan_b_diff[, .(
    q5 = round_quantile(value, 0.05),
    q20 = round_quantile(value, 0.20),
    q50 = round_quantile(value, 0.50),
    q80 = round_quantile(value, 0.80),
    q95 = round_quantile(value, 0.95)
  ), by = by_cols]
  return(plan_b_diff[])
}