#' NDVI
#'
#' @description
#' Calculate the normalised difference vegetation index of a raster layer.
#'
#' @param input_raster A raster stack or the path to a raster file.
#'
#' @param red_band integer indicating the layer of `input_raster` that holds the
#' red band
#'
#' @param ni_band integer indicating the layer of `input_raster` that holds the
#' near infrared band
#'
#' @export

ndvi <- function(input_raster,
                 red_band,
                 ni_band){

  UseMethod("ndvi")
}

#' @export
ndvi.character <- function(input_raster,
                           red_band,
                           ni_band){

  input_raster <- raster::stack(input_raster)
  .Class <- class(raster::stack())
  NextMethod()
}

#' @export
ndvi.RasterStack <- function(input_raster,
                           red_band,
                           ni_band){
  calcualte_ndvi(input_raster,
                 red_band,
                 ni_band)
}


calcualte_ndvi <- function(input_raster,
                  red_band,
                  ni_band){

  output_raster <-
    (input_raster[[ni_band]] - input_raster[[red_band]]) / (input_raster[[ni_band]] + input_raster[[red_band]])

  names(output_raster) <- "NDVI"
  output_raster
}
