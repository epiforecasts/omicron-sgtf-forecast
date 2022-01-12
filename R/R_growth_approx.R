#' Convert Growth Rates to Reproduction numbers.
#'
#' @description `r lifecycle::badge("questioning")`
#' See [here](https://www.medrxiv.org/content/10.1101/2020.01.30.20019877v3.full.pdf)
#' for justification. Now handled internally by stan so may be removed in future updates if
#' no user demand.
#' @param r Numeric, rate of growth estimates
#' @param gamma_mean Numeric, mean of the gamma distribution
#' @param gamma_sd Numeric, standard deviation of the gamma distribution
#' @return Numeric vector of reproduction number estimates
#' @export
#' @examples
#' growth_to_R(0.2, 4, 1)
growth_to_R <- function(r, gamma_mean, gamma_sd) {
  k <- (gamma_sd / gamma_mean)^2
  R <- (1 + k * r * gamma_mean)^(1 / k)
  return(R)
}

#' Convert Reproduction Numbers to Growth Rates
#'
#' @description `r lifecycle::badge("questioning")`
#' See [here](https://www.medrxiv.org/content/10.1101/2020.01.30.20019877v3.full.pdf)
#' for justification. Now handled internally by stan so may be removed in future updates if
#' no user demand.
#' @param R Numeric, Reproduction number estimates
#' @inheritParams growth_to_R
#' @return Numeric vector of reproduction number estimates
#' @export
#' @examples
#' R_to_growth(2.18, 4, 1)
R_to_growth <- function(R, gamma_mean, gamma_sd) {
  k <- (gamma_sd / gamma_mean)^2
  r <- (R^k - 1) / (k * gamma_mean)
  return(r)
}

r_to_r <- function(r, G, k, alpha) {
  palpha <- alpha^k
  r_n <- palpha * r + (palpha - 1)/(k * G)
  return(r_n)
}

r_to_r_diff_gt <- function(r, G, k, G_v, k_v, alpha) {
  r_n <- alpha^k_v * (1 + r * k * G) ^ (k_v / k) - 1
  r_n <- r_n / (k_v * G_v)
  return(r_n)
}

sd_to_k <- function(G_sd, G) {
  k <- (G_sd / G)^2
  return(k)
}

k_to_sd <- function(k, G) {
  G_sd <- sqrt(k) * G
  return(G_sd)
}
