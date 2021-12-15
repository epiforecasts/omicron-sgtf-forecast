# Load parameters for VOC model
parameters <- list(

  voc_label = "Omicron",

  # Prior for the growth rate modification of the VOC compared to the original.
  voc_scale = c(0.21, 0.2),

  # Use a 2 strain model
  strains = 2,

  # Prior for initial growth rate.
  r_init = c(0, 0.25),

  # Use a time scale based on generation time, rather than weekly.
  scale_r = 1,

  # Use weekly piecewise constant growth rate
  r_step = 7,

  # Account for overdispersion
  overdispersion = TRUE,

  # daily time scale
  timespan = 1,

  # forecast
  horizon = 1
)
