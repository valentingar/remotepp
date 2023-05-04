#' Convert PAR from mol to J
#'
#' @param x photons in mol
#' @param wavelength wavelength in nm
#'
#' @returns energy in J
#'
#'
#'@export

energy_per_photon <- function(x,
                              wavelength){
  hc <- 1.98644586*10^(-25) # J

  x <- x * hc / (wavelength / 1000 / 1000/ 1000)
  x
}
