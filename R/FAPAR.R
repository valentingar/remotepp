#' FAPAR: Fraction of absorbed PAR
#'
#'
#'
#'
#'
#' @param NDVI A raster(stack) of NDVI values
#' @param vegetation_mask A raster layer with classification of vegetation types.
#'
#' @param FAPAR_min constant, see Wang et al. (2017)
#' @param FAPAR_max constant, see Wang et al. (2017)
#'
#' @param quantile_min numeric (0,1) for NDVI_min calculation, see Wang et al. (2017)
#' @param quantile_max numeric (0,1) for NDVI_max calculation, see Wang et al. (2017)
#'
#' @references
#'
#' \insertRef{Wang2017}{remotepp} \cr
#'
#' @importFrom terra values<-
#'
#'@export

calc_FAPAR <- function(NDVI,
                  vegetation_mask = NULL,
                  FAPAR_min = 0.001,
                  FAPAR_max = 0.95,
                  quantile_min = 0.05,
                  quantile_max = 0.95){

  NDVI <- as(NDVI, "SpatRaster")

  if (is.null(vegetation_mask)){
    vegetation_mask <- NDVI[[1]]
    values(vegetation_mask) <- 1
  } else {
    vegetation_mask <- as(vegetation_mask, "SpatRaster")
  }



  df <- as.data.frame(c(vegetation_mask,
                        NDVI))

  NDVI_quantiles <-
    split(df[,-1], df[,1]) %>%
    lapply(quantile,
           probs = c(quantile_min, quantile_max),
           na.rm = TRUE) %>%
    do.call(rbind, .) %>%
    t()

  # NDVI_quantiles <-
  # terra::zonal(NDVI,
  #              vegetation_mask,
  #              fun = function(x, ...) x) %>%
  #   apply(MARGIN = 1, FUN = function(x) quantile(unlist(x[-1]), #first entry is zone identifyer
  #                                                probs = c(quantile_min,
  #                                                             quantile_max),
  #                                                na.rm = TRUE))

  NDVI_min <- NDVI_max <- vegetation_mask
  terra::values(NDVI_min) <- NDVI_quantiles[1, terra::values(vegetation_mask)]
  terra::values(NDVI_max) <- NDVI_quantiles[2, terra::values(vegetation_mask)]

  FAPAR_out <- (NDVI - NDVI_min) * (FAPAR_max - FAPAR_min) / (NDVI_max - NDVI_min) + FAPAR_min
  FAPAR_out

}


