
gt_load_model <- function(model = here::here("stan/generation-time.stan"),
                          include = here::here("stan")) {
  model <- cmdstanr::cmdstan_model(model, include_paths = include)
  return(model)
}

gt_prior <- function(type = "household", source = "hart2021") {
  type <- match.arg(type, choices = c("household", "intrinsic"))
  source <- match.arg(source, choices = c("hart2021"))

  if (type %in% "intrinsic" & source %in% "hart2021") {
    gt <- list(
      # From Hart et al.
      # https://www.medrxiv.org/content/10.1101/2021.10.21.21265216v1
      # Assuming symmetric normal which is incorrect but an approximation
      mean_mean = 4.6,
      mean_sd = 0.36,
      sd_mean = 3.1,
      sd_sd = 0.18,
      source = "hart2021",
      doi = "10.1101/2021.10.21.21265216v1"
    )
  }else if (type %in% "household" & source %in% "hart2021") {
    gt <- list(
      # From Hart et al.
      # https://www.medrxiv.org/content/10.1101/2021.10.21.21265216v1
      # Assuming symmetric normal which is incorrect but an approximation
      mean_mean = 3.2,
      mean_sd = 0.46,
      sd_mean = 2.4,
      sd_sd = 0.33,
      source = "hart2021",
      doi = "10.1101/2021.10.21.21265216v1"
    )
  }
  return(gt)
}

gt_dt <- function(growth, by = c(),
                  gt = gt_prior(type = "intrinsic", source = "hart2021")) {
  if (length(by) > 0) {
    form <- as.formula(
      paste0(
        paste(by, collapse = " + ", sep = " + "),
        "+ date ~ type"
      )
    )
  }else{
    form <- as.formula("date ~ type")
  }
  growth_sd_wide <- data.table::dcast(
    growth, form, value.var = "sd"
  )
  growth_sd_wide <- growth_sd_wide[!is.na(Omicron)]

  growth_mean_wide <- data.table::dcast(
    growth, form, value.var = "mean"
  )
  growth_mean_wide <- growth_mean_wide[!is.na(Omicron)]

  # Data for stan
  stan_dt <- list(
    t = nrow(growth_mean_wide),
    voc_r = growth_mean_wide$Omicron,
    voc_sd2 = growth_sd_wide$Omicron^2,
    nvoc_r_mean = growth_mean_wide$`non-Omicron`,
    nvoc_r_sd = growth_sd_wide$`non-Omicron`,
    gt_mean_mean = gt$mean_mean,
    gt_mean_sd = gt$mean_sd,
    gt_sd_mean = gt$sd_mean,
    gt_sd_sd = gt$sd_sd
  )
  return(stan_dt)
}

gt_inits <- function(data) {
  function() {
    data <- list(
      nvoc_r = purrr::map2(
        data$nvoc_r_mean, data$nvoc_r_sd, ~ rnorm(1, .x, .y * 0.1)
      ),
      gt_mean = rnorm(1, data$gt_mean_mean, data$gt_mean_sd * 0.1),
      gt_sd = rnorm(1, data$gt_sd_mean, data$gt_sd_sd * 0.1),
      voc_gt_mean_mod = rnorm(1, 1, 0.01),
      voc_gt_sd_mod = rnorm(1, 1, 0.01),
      sigma = rnorm(1, 0.1, 0.01),
      ta = rnorm(1, 1, 0.1)
    )
    data$voc_gt_mean <- data$gt_mean
    data$voc_gt_sd <- data$gt_sd
    return(data)
  }
}

gt_summarise_posterior <- function(fit,
                                   vars = c("gt_mean", "gt_sd",
                                            "voc_gt_mean_mod", "voc_gt_sd_mod",
                                            "voc_gt_mean", "voc_gt_sd", "ta",
                                            "sigma"), ...) {
  posterior <- fit$summary(variables = vars, ...)
  posterior <- data.table::as.data.table(posterior)
  return(posterior[])
}

gt_summarise_growth_pp <- function(fit, growth, by = c()) {
  r_pp <- fit$summary(
    variables  = "pp_voc_r", posterior::quantile2,
    .args = list(probs = c(0.05, 0.2, 0.8, 0.95))
  )
  r_pp <- data.table::as.data.table(r_pp)[, type := "Posterior prediction"]
  cols <- c(by, "date")
  r_pp <- cbind(growth[type %in% "Omicron", ..cols], r_pp)
  r_pp <- rbind(
    growth[type %in% "Omicron"][, type := "Estimate"][!is.na(mean)],
    r_pp, fill = TRUE, use.names = TRUE
  )
  return(r_pp[])
}

gt_estimate <- function(growth, model, by = c(), gt, ...) {

  # Data for stan
  stan_dt <- gt_dt(growth, by = by, gt = gt)

  # Set initial conditions based on priors
  # Fit model (initially a little stroppy)
  fit <- model$sample(
    data = stan_dt, init = gt_inits(stan_dt), ...
  )

  # summarise variables of interest
  summary <- gt_summarise_posterior(fit)
  # summmarise posterior predictions
  r_pp <- gt_summarise_growth_pp(fit, growth, by = by)
  out <- data.table::data.table(
    gt_dt = list(stan_dt),
    fit = list(fit),
    summary = list(summary),
    pp = list(r_pp)
  )
  return(out[])
}


gt_plot_pp <- function(r_pp) {
    ggplot2::ggplot(r_pp) +
    ggplot2::aes(x = date, y = median, fill = type) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = q5, ymax = q95), alpha = 0.3) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = q20, ymax = q80), alpha = 0.3) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
    ggplot2::labs(fill = "Growth rate source", y = "Growth rate") +
    ggplot2::scale_fill_brewer(palette = "Dark2")
}
