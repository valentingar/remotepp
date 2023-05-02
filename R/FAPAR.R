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
                  FAPAR_min = 0.001){

  NDVI_min <- raster::raster(NDVI)
  NDVI_max <- NDVI_min

  block_size <- raster::blockSize(NDVI_min)

  NDVI <- raster::readStart(NDVI)
  NDVI_min <- raster::writeStart(NDVI_min, raster::rasterTmpFile())
  NDVI_max <- raster::writeStart(NDVI_max, raster::rasterTmpFile())


  for (i in seq_along(block_size$row)){
    current_row <- raster::getValues(NDVI,
                                     row = block_size$row[i],
                                     nrows = block_size$nrows[i])

    lowq <- apply(current_row, MARGIN = 1,
                 function(x) quantile(x, c(0.05), na.rm = TRUE))
    highq <- apply(current_row, MARGIN = 1,
                   function(x) quantile(x, c(0.95), na.rm = TRUE))

    NDVI_min <- raster::writeValues(NDVI_min,
                                    lowq,
                                    block_size$row[i])
    NDVI_max <- raster::writeValues(NDVI_max,
                                    highq,
                                    block_size$row[i])

  }

  NDVI <- raster::readStop(NDVI)
  NDVI_min <- raster::writeStop(NDVI_min)
  NDVI_max <- raster::writeStop(NDVI_max)

  FAPAR_out <- (NDVI - NDVI_min) * (FAPAR_max - FAPAR_min) / (NDVI_max - NDVI_min) + FAPAR_min
  FAPAR_out

}


