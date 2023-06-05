#' Convert PAR
#'
#'
#' @param x PAR in µmol s^-1 (or µmol s^-1 m^-2; ...)
#'
#' @returns \eqn{R_S} in W (or W m^-2; J s^-1 m^-2; ...). This is calculated
#'   using a simple factor of 4.57.
#'   nm, the range of PAR.  \cr \cr \eqn{R_S {[}\frac{J}{s}{]} =
#'   \frac{1}{4.57}{[}\frac{J}{µmol}{]} \cdot PPFD {[}\frac{µmol}{s}{]}}
#'
#'
#' @references \insertRef{Reis2020}{remotepp} \cr
#'
#' @export

PPFD_to_irradiance <- function(x){
  1/4.57 * x
}
