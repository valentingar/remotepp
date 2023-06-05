#' rpp_MODIS
#'
#' The class rpp_MODIS is the output from running the `MODIS()` model. It is a named list that includes the
#' output layer GPP, as well as all the other parameters derived from the input.
#'
#' @param x a MODIS model list
#' @param GPP `terra::rast()` of the GPP in g-C m^-2 d^-1 with each layer representing one day.
#' @inheritParams MODIS
#'
#' @export
#### HELPER ####
rpp_MODIS <- function(GPP = terra::rast(),
                      PAR = c(),
                      FAPAR = terra::rast(),
                      TMIN_scalar = c(),
                      VPD_scalar = c(),
                      epsilon_max = numeric(),
                      dates = c()) {

  x <- new_rpp_MODIS(
    x = list(
      GPP = GPP,
      PAR = PAR,
      FAPAR = FAPAR,
      TMIN_scalar = TMIN_scalar,
      VPD_scalar = VPD_scalar,
      epsilon_max = epsilon_max,
      dates = dates
    )
  )
  x <- validate_rpp_MODIS(x)
  x
}

#### CONSTRUCOR ####
new_rpp_MODIS <- function(x,
                         WSC_type) {
  x <- structure(
    x,
    class = c("rpp_MODIS", "list")
  )
  x
}

#### VALIDATOR ####
validate_rpp_MODIS <- function(x) {
  # add tests to check your object is created correctly here
  x
}


### METHODS ###
#' @export
print.rpp_MODIS <- function(
    x,
    ...) {
  n_layers <- length(names(x$GPP))
  cat("A MODIS Model output with", n_layers, "layers: \n")
  n_print <- min(c(10, n_layers))
  cat(x$dates[1:n_print])
  if(n_layers > n_print){
    cat("...")
  }
  cat("\n")
  cat("extent: ")
  cat(terra::ext(x$GPP)[1:4], "(xmin xmax ymin ymax) \n")
  cat("dimension: ")
  cat(dim(x$GPP)[1:2], "(x y) \n")
  cat("resolution: ")
  cat(terra::res(x$GPP)[1:2], "(x y) \n")
}



