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
#'@param dates A vector the same length as PAR giving the dates.
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
                 dates = NULL,
                 ...){

  dots <- list(...)

  stopifnot("Please provide dates!" = !is.null(dates))

  n_PAR <- length(PAR)
  stopifnot("Dates must match length of PAR" = length(dates) == n_PAR)

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


  if (is.numeric(FAPAR)){
    n_FAPAR <- length(FAPAR)
  } else {
    n_FAPAR <- terra::nlyr(FAPAR)
  }

  if (is.numeric(WSC)){
    n_WSC <- length(WSC)
  } else {
    n_WSC <- terra::nlyr(WSC)
  }

  n_TSC1 <- length(TSC1)
  n_TSC2 <- length(TSC2)
  n_WSC <- length(WSC)

  stopifnot("Lengths don't match! Check input to be the same temporal resolution and length!" =
              length(unique(c(n_PAR, n_FAPAR, n_TSC1, n_TSC2, n_WSC))) == 1)

  NPP <- PAR * FAPAR * TSC1 * TSC2 * WSC * epsilon_max
  names(NPP) <- dates

  rpp_CASA(NPP = NPP,
           PAR = PAR,
           FAPAR = FAPAR,
           TSC1 = TSC1,
           TSC2 = TSC2,
           WSC = WSC,
           epsilon_max = epsilon_max,
           dates)
}
