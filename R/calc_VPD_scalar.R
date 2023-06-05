#' MODIS: VPD_scalar
#'
#' @description This function calculates the VPD_scalar coefficient for the MODIS GPP model.
#'
#' @rdname VPD_scalar
#'
#' @param D_e vector of mean daily vapor pressure deficit in kPa
#' @param VPD_min The daylight average vapor pressure deficit at which epsilon = epsilon_max [Pa]
#' @param VPD_max The daylight average vapor pressure deficit at which at which epsilon = 0 [Pa]
#'
#' @references
#' \insertRef{Running2015}{remotepp} \cr
#'
#' @export

calc_VPD_scalar <- function(D_e,
                            VPD_min = -650,
                            VPD_max = 5300){

  VPD <- D_e/1000

  VPD_scalar <- (VPD - VPD_max) / (VPD_min - VPD_max)
  VPD_scalar[VPD < VPD_min] <- 1
  VPD_scalar[VPD > VPD_max] <- 0

  VPD_scalar
}
