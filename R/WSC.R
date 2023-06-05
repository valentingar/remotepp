#' Water Stress Coefficient (WSC)

#' @description This function calculates the Water Stress Coefficient (WSC) for
#'   the CASA model.
#'
#' @param wsc_method The method to be used for WSC calculation. Different
#'   approaches require different input data. The following approaches are
#'   implemented:
#' \describe{
#'
#' \item{deficit}{Requires the vapour pressure deficit \strong{D_e} in kPa. \cr
#'   \eqn{WSC = 1.2 \cdot exp(-0.35 \cdot D_e) - 0.2}. \cr Tang et al. (2014)}
#'
#' \item{SIMI}{Remote sensinsg data driven. Requires bands \strong{SWIR1}
#' (1.628 - 1.652 µm) and \strong{SWIR2} (2.105 - 2.155 µm) to be provided.
#'   WSC is calculated per pixel. The min and max of SIMI are also calculated
#'   per pixel. \cr
#'   \eqn{SIMI = 0.7071 \cdot sqrt(SWIR1^2 + SWIR2^2)} \cr
#'   \eqn{NSIMI = (SIMI - min(SIMI)) / (max(SIMI) - min(SIMI))} \cr
#'   \eqn{WSC = 0.5 + 0.5 * NSIMI} \cr
#'   Wu et al. (2022)}
#'
#' \item{LSWI}{Remote sensinsg and meteorological data driven. Requires bands
#' \strong{SWIR1} (1.628 - 1.652 µm) and \strong{NIR} to be provided, as well as \strong{prec}. \cr
#' WSC is calculated per pixel. The max of LSWI is also calculated per pixel.
#' A rolling sum of the last 30 days is applied to prec to get the monthly
#' precipitation. \cr
#'   \eqn{LSWI = (NIR - SWIR1) / (NIR + SWIR1)} \cr
#'   \eqn{WSC_{vpm,in} = (1 - (1 + LSWI) / (1 + LSWI_{max})) + 0.5} \cr
#'   \eqn{WSC = prec / prec_{max} \cdot WSC_{vpm,in} + 0.5} \cr
#'   Bao et al. (2015)}
#'
#' }
#' @param D_e vector of mean daily vapor pressure deficit in kPa
#' @param SWIR1 A raster stack where each layer is one time point.
#' @param SWIR2 A raster stack where each layer is one time point.
#' @param NIR A raster stack where each layer is one time point.
#' @param prec vector of daily precipitation in mm
#'
#' @returns A vector or RasterStack of the water stress coefficient with one
#'   entry (layer) per day.
#'
#' @references
#' \insertRef{Tang2014}{remotepp} \cr
#' \insertRef{Bao2016}{remotepp} \cr
#' \insertRef{Wu2022}{remotepp} \cr
#'
#' @importFrom Rdpack reprompt
#'
#' @export

calc_WSC <- function(wsc_method,
                D_e = NULL,
                SWIR1 = NULL,
                SWIR2 = NULL,
                NIR = NULL,
                prec = NULL){

  wsc_method <- match.arg(wsc_method, c("deficit", "SIMI", "LSWI"))

  if(wsc_method == "deficit"){
    ignore_argument_message(SWIR1, "SWIR1")
    ignore_argument_message(SWIR2, "SWIR2")
    ignore_argument_message(NIR, "NIR")
    ignore_argument_message(prec, "prec")

    WSC_out <- WSC_deficit(D_e)
  } else if (wsc_method == "SIMI"){
    ignore_argument_message(D_e, "D_e")
    ignore_argument_message(NIR, "NIR")
    ignore_argument_message(prec, "prec")

    WSC_out <- WSC_SIMI(SWIR1, SWIR2)
  } else if (wsc_method == "LSWI"){
    ignore_argument_message(D_e, "D_e")
    ignore_argument_message(SWIR2, "SWIR2")

    WSC_out <- WSC_LSWI(SWIR1, NIR, prec)
  }

  WSC_out
}


### HELPERS --------------------------------------------------------------------

### wsc_method ###

WSC_deficit <- function(D_e){
  # D_e in kPa

  WSC_out <-
    1.2 * exp(-0.35 * D_e) - 0.2

  WSC_out
}

WSC_SIMI <- function(SWIR1, SWIR2){

  stopifnot("SWIR1 and SWIR2 dont have the same number of layers!" = raster::nlayers(SWIR1) == raster::nlayers(SWIR2))
  stopifnot("resolution of SWIR1 and SWIR2 dont match" = raster::res(SWIR1) == raster::res(SWIR2))
  stopifnot("extent of SWIR1 and SWIR2 dont match" = raster::extent(SWIR1) == raster::extent(SWIR2))

  SWIR1 <- as(SWIR1, "SpatRaster")
  SWIR2 <- as(SWIR2, "SpatRaster")

  SIMI <- SIMI(SWIR1, SWIR2)
  SIMI_min <- min(SIMI) # pixel-wise min of timeseries
  SIMI_max <- max(SIMI) # pixel-wise max of timeseries
  NSIMI <- (SIMI - SIMI_min) / (SIMI_max - SIMI_min)
  WSC_out <-
    0.5 + 0.5*(1 -NSIMI)
  WSC_out
}

WSC_LSWI <- function(SWIR1, NIR, prec){

  stopifnot("SWIR1 and NIR dont have the same number of layers!" = raster::nlayers(SWIR1) == raster::nlayers(NIR))
  stopifnot("resolution of SWIR1 and NIR dont match" = raster::res(SWIR1) == raster::res(NIR))
  stopifnot("extent of SWIR1 and NIR dont match" = raster::extent(SWIR1) == raster::extent(NIR))
  stopifnot("length of prec does not match number of layers!" = raster::nlayers(SWIR1) == length(prec))

  prec_rollsum <- zoo::rollsum(prec, 31,
                               align = "right",
                               fill = "extend" )
  prec_max <- max(prec_rollsum)

  LSWI <- LSWI(SWIR1, NIR)
  LSWI_max <- max(LSWI)

  WSC_out <- prec_rollsum/prec_max * (1 - ((1 + LSWI) / (1 + LSWI_max)) + 0.5) + 0.5
  WSC_out
}

### INDICES ###

SIMI <- function(SWIR1, SWIR2){
  SIMI <- 0.7071 * sqrt(SWIR1^2 + SWIR2^2)
  SIMI
}

LSWI <- function(SWIR1, NIR){
  LSWI <- (NIR - SWIR1) / (NIR + SWIR1)
  LSWI
}


