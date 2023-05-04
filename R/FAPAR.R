#' FAPAR: Fraction of absorbed PAR
#'
#'
#'
#'
#'
#' @param NDVI A raster(stack) of NDVI values
#' @param FAPAR_max constant, see Wang et al. (2017)
#' @param FAPAR_min constant, see Wang et al. (2017)
#'
#'
#' @references
#'
#' \insertRef{Wang2017}{remotepp} \cr
#'
#'
#'@export

calc_FAPAR <- function(NDVI,
                  FAPAR_max = 0.95,
                  FAPAR_min = 0.001,
                  cores = 1){

  NDVI <- as(NDVI, "SpatRaster")

  NDVI_quantiles <- terra::app(NDVI,
                               quantile,
                               probs = c(0.05,0.95),
                               na.rm = TRUE,
                               cores = cores)



  FAPAR_out <- (NDVI - NDVI_quantiles[[1]]) * (FAPAR_max - FAPAR_min) / (NDVI_quantiles[[2]] - NDVI_quantiles[[1]]) + FAPAR_min
  FAPAR_out

}


