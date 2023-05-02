#'The CASA model
#'
#'@description Calculate the net primary productivity (NPP) using the
#'  Carnegie-Ames-Stanford-Approach (CASA).
#'
#'@rdname CASA
#'
#'@param PAR Incoming photosyntheticly active radiation (PAR) in MJ m^-2 d^-1
#'@param FAPAR Fraction of absorbed PAR (0,1) as a raster stack with each layer
#' representing one day. If missing, is calculated using `calc_FAPAR()`
#'@param WSC Water stress coefficient. If missing, is calculated using `calc_WSC()`
#'@param TSC1 Temperature stress coefficient 1. If missing, is calculated using `calc_TSC1()`
#'@param TSC2 Temperature stress coefficient 2. If missing, is calculated using `calc_TSC2()`
#'@param epsilon_max The maximum light use efficiency (LUE) in g-C MJ^{-1}
#'
#'@inheritDotParams calc_WSC
#'@inheritDotParams calc_TSC1
#'@inheritDotParams calc_TSC2
#'@inheritDotParams calc_FAPAR
#'
#' @returns a RasterStack of NPP in g-C m^-2 d^-1 with each layer representing one day.
#'
#'
#' @references
#' \insertRef{Potter1993}{remotepp} \cr
#'
#'@export

CASA <- function(PAR,
                 FAPAR = NULL,
                 WSC = NULL,
                 TSC1 = NULL,
                 TSC2 = NULL,
                 epsilon_max = 0.608,
                 ...){

  dots <- list(...)

  if ((is.null(TSC1) | is.null(TSC2)) & is.null(dots$temperature_opt)){
    dots$temperature_opt <- calc_temperature_opt(dots$NDVI,
                                                 dots$temperature)
  }
  if (is.null(TSC1)){
    message("Calculating TSC1")
    TSC1 <- do.call(calc_TSC1, dots[names(dots) %in% names(formals(calc_TSC1))])
  }
  if (is.null(TSC2)){
    message("Calculating TSC2")
    TSC2 <- do.call(calc_TSC2, dots[names(dots) %in% names(formals(calc_TSC2))])
  }
  if (is.null(WSC)){
    message("Calculating WSC")
    WSC <- do.call(calc_WSC, dots[names(dots) %in% names(formals(calc_WSC))])
  }
  if (is.null(FAPAR)){
    message("Calculating FAPAR")
    FAPAR <- do.call(calc_FAPAR, dots[names(dots) %in% names(formals(calc_FAPAR))])
  }

  FAPAR <- as(FAPAR, "SpatRaster")


  if (is.numeric(PAR)){
    stopifnot(terra::nlyr(FAPAR) == length(PAR))
  } else {
    stopifnot(terra::nlyr(FAPAR) == length(PAR))
  }

  NPP <- PAR * FAPAR * TSC1 * TSC2 * WSC * epsilon_max
  NPP
}
