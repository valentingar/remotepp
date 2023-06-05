#'The MODIS GPP model
#'
#'@description Calculate the gross primary productivity (NPP) using the MODIS GPP algorithm
#'
#'@rdname CASA
#'
#'@param PAR Incoming photosyntheticly active radiation (PAR) in MJ m^-2 d^-1
#'@param FAPAR Fraction of absorbed PAR (0,1) as a raster stack with each layer
#' representing one day. If missing, is calculated using `calc_FAPAR()`
#'@param TMIN_scalar Temperature stress factor, calculated by `calc_TMIN_scalar()`.
#'@param VPD_scalar Vapor pressure deficit factor, calculated by `calc_VPD_scalar()` .
#'@param epsilon_max The maximum light use efficiency (LUE) in g-C MJ^{-1}
#'@param dates A vector the same length as PAR giving the dates.
#'
#'@inheritDotParams calc_TMIN_scalar
#'@inheritDotParams calc_VPD_scalar
#'@inheritDotParams calc_FAPAR
#'
#' @returns a RasterStack of NPP in g-C m^-2 d^-1 with each layer representing one day.
#'
#'
#' @references
#' \insertRef{Running1999}{remotepp} \cr
#'
#'@export

MODIS <- function(PAR,
                  FAPAR = NULL,
                  TMIN_scalar = NULL,
                  VPD_scalar = NULL,
                  epsilon_max = 0.86,
                  dates = NULL,
                  ...){

  dots <- list(...)

  stopifnot("Please provide dates!" = !is.null(dates))

  n_PAR <- length(PAR)
  stopifnot("Dates must match length of PAR" = length(dates) == n_PAR)

  if (is.null(TMIN_scalar)){
    message("Calculating TMIN_scalar")
    TMIN_scalar <- do.call(calc_TMIN_scalar, dots[names(dots) %in% names(formals(calc_TMIN_scalar))])
  }
  if (is.null(VPD_scalar)){
    message("Calculating VPD_scalar")
    VPD_scalar <- do.call(calc_VPD_scalar, dots[names(dots) %in% names(formals(calc_VPD_scalar))])
  }
  if (is.null(FAPAR)){
    message("Calculating FAPAR")
    FAPAR <- do.call(calc_FAPAR, dots[names(dots) %in% names(formals(calc_FAPAR))])
  }

  FAPAR <- as(FAPAR, "SpatRaster")


  if (is.numeric(FAPAR)){
    n_FAPAR <- length(FAPAR)
  } else {
    n_FAPAR <- terra::nlyr(FAPAR)
  }

  n_TMIN_scalar <- length(TMIN_scalar)
  n_VPD_scalar <- length(VPD_scalar)

  stopifnot("Lengths don't match! Check input to be the same temporal resolution and length!" =
              length(unique(c(n_PAR, n_FAPAR, n_TMIN_scalar, n_VPD_scalar))) == 1)

  GPP <- PAR * FAPAR * TMIN_scalar * VPD_scalar * epsilon_max
  names(GPP) <- dates

  rpp_MODIS(GPP = GPP,
            PAR = PAR,
            FAPAR = FAPAR,
            TMIN_scalar = TMIN_scalar,
            VPD_scalar = VPD_scalar,
            epsilon_max = epsilon_max,
            dates)
}

