#' Calculate Evapotranspiration using Penman-Monteith FAO56
#'
#' @param irradiance at crop surface in MJ m^-2 day^-1
#' @param temperature mean daily temperature in °C
#' @param ea vapour pressure in kPa
#' @param u_2 wind speed at 2 m in m s^-1
#' @param pressure air pressure in kPa
#' @param G soil heat flux in MJ m^-2 day^-1. Default is 0
#'
#' @returns reference evapotranspiration in mm m^-2 d^-1
#'
#' @references ISBN 92-5-104219-5
#'@export


ET_FAO56 <- function(irradiance,
                     temperature,
                     ea,
                     u_2,
                     pressure,
                     ...){

  gamma <- pressure_gamma(pressure)
  DELTA <- temp_DELTA(temperature)
  delta_e <- (temp_esat(temperature) - ea)


  calculate_ET_FAO56(DELTA = DELTA,
                     Rn = irradiance,
                     t = temperature,
                     gamma = gamma,
                     u_2 = u_2,
                     delta_e = delta_e,
                     ...)

}


### HELPERS --------------------------------------------------------------------
calculate_ET_FAO56 <- function(DELTA,
                               Rn,
                               G = 0,
                               t,
                               gamma,
                               u_2,
                               delta_e
                               ){

  ET0 <- (0.408 * DELTA * (Rn-G) + 900/(t + 273.15) * gamma * u_2 * delta_e) / (DELTA + gamma * (1 + 0.34 * u_2))
  ET0
}

temp_esat <- function(t){
  # t in °C
  # esat in kPa
  esat <- 0.6108 * exp(17.27 * t / (t + 237.3))
  esat
}

temp_DELTA <- function(t){
  # t in °C
  # DELTA in kPa/°C
  DELTA <- (2503.16 * exp((17.27 * t) / (237.3 + t))) / (237.3 + t)^2
  DELTA
}

pressure_gamma <- function(p){
  # p in kPa
  # gamma in kPa/°c

  gamma <- 0.665*10^-3 * p
  gamma
}
