#' MODIS: TMIN_scalar
#'
#' @description This function calculates the TMIN_scalar coefficient for the MODIS GPP model.
#'
#' @rdname TMIN_scalar
#'
#' @param temperature vector of mean daily temperature in °C
#' @param TMIN_min daily minimum temperature at which epsilon = epsilon_max
#' @param TMIN_max daily minimum temperature at which epsilon = 0
#'
#' @references
#' \insertRef{Running2015}{remotepp} \cr
#'
#' @export

calc_TMIN_scalar <- function(temperature,
                      TMIN_min = -8,
                      TMIN_max = 12.02){

  TMIN_scalar <- (temperature - TMIN_min) / (TMIN_max - TMIN_min)
  TMIN_scalar[temperature < TMIN_min] <- 0
  TMIN_scalar[temperature > TMIN_max] <- 1

  TMIN_scalar
}
