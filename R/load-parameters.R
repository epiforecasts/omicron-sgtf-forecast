# Load parameters for VOC model
load_sgtf_parameters <- function() {
  parameters <- list(

    voc_label = "Omicron",

    # Prior for the growth rate modification of the VOC compared to the 
    # original.
    voc_scale = c(0.21, 0.1),

    # Use a 2 strain model
    strains = 2,

    # Prior for initial growth rate.
    r_init = c(0, 0.1),

    # Use a daily scaling (i.e known)
    scale_r = 1,

    # Use weekly piecewise constant growth rate
    r_step = 3,

    # Account for overdispersion
    overdispersion = TRUE,

    # daily time scale
    timespan = 1,

    # forecast
    horizon = 14,

    # should forcast be extrapolated
    r_forecast = FALSE,

    # holidays to adjust for (as dates)
    holidays = c()
  )

  parameters$voc_scale <- parameters$voc_scale * parameters$timespan
  return(parameters)
}

# Load parameters bias model
load_bias_parameters <- function() {
  parameters <- load_sgtf_parameters()
  parameters$voc_scale <- c(0, 0.2)
  parameters$voc_label <- "SGT-result"
  return(parameters)
}