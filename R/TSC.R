#' Temperature Stress Coefficient (TSC)
#'
#' @description This function calculates the temperature Stress Coefficients (TSC) for
#'   the CASA model.
#'
#' @rdname TSC
#'
#' @param temperature vector of mean daily temperature in °C
#' @param temperature_opt the mean temperature of the month, where NDVI is highest in °C
#'
#' @references
#' \insertRef{Potter1993}{remotepp} \cr
#'
#' @export

calc_TSC1 <- function(temperature,
                 temperature_opt){


  TSC1_out <- 0.8 + 0.02 * temperature_opt - 0.0005 * temperature_opt^2
  TSC1_out <- rep(TSC1_out, length(temperature))

  temperature_mean_month <- zoo::rollmean(temperature, 31, fill = "expand")
  TSC1_out [temperature_mean_month < -10] <- 0

  TSC1_out
}

#' @rdname TSC
#' @export

calc_TSC2 <- function(temperature,
                 temperature_opt){

  TSC2_out <- 1.1814 / (1 + exp(0.2 * (temperature_opt - 10 - temperature))) * (1 / (1+exp(0.3*(-temperature_opt - 10 + temperature))))
  TSC2_out
}


### HELPERS --------------------------------------------------------------------
calc_temperature_opt <- function(NDVI, temperature){
  stopifnot("NDVI must have the same number of layers as entries in temperature!" = length(temperature) == raster::nlayers(NDVI))

  NDVI_mean <- sapply(1:raster::nlayers(NDVI), function(i) raster::cellStats(NDVI[[i]], mean, nar.rm = TRUE))
  NDVI_mean_rollmean <- zoo::rollmean(NDVI_mean, 31, fill = "expand", na.rm = TRUE)
  temperature_rollmean <- zoo::rollmean(temperature, 31, fill = "expand", na.rm = TRUE)

  temperature_opt <- temperature_rollmean[which.max(NDVI_mean_rollmean)]
  temperature_opt
}
