#' rpp_CASA
#'
#' The class rpp_CASA is the output from running the `CASA()` model. It is a named list that includes the
#' output layer NPP, as well as all the other parameters derived from the input.
#'
#' @param x a CASA model list
#' @param NPP `terra::rast()` of the NPP in g-C m^-2 d^-1 with each layer representing one day.
#' @inheritParams CASA
#'
#' @export
#### HELPER ####
rpp_CASA <- function(
    NPP = terra::rast(),
    PAR = c(),
    FAPAR = terra::rast(),
    TSC1 = c(),
    TSC2 = c(),
    WSC = c(),
    epsilon_max = numeric()) {


  if(class(WSC) == class(terra::rast())){
    WSC_type = "raster"
  } else {
    WSC_type = "numeric"
  }


  x <- new_rpp_CASA(
    x = list(
      NPP = NPP,
      PAR = PAR,
      FAPAR = FAPAR,
      TSC1 = TSC1,
      TSC2 = TSC2,
      WSC = WSC,
      epsilon_max = epsilon_max
      ),
    WSC_type = WSC_type
  )
  x <- validate_rpp_CASA(x)
  x
}

#### CONSTRUCOR ####
new_rpp_CASA <- function(x,
                         WSC_type) {
  x <- structure(
    x,
    class = c("rpp_CASA", "list"),
    WSC_type = WSC_type
  )
  x
}

#### VALIDATOR ####
validate_rpp_CASA <- function(x) {
  # add tests to check your object is created correctly here
  x
}


### METHODS ###
#' @export
print.rpp_CASA <- function(
    x,
    ...) {
  n_layers <- length(names(x$NPP))
  cat("A CASA Model output with", n_layers, "layers: \n")
  n_print <- min(c(10, n_layers))
  cat(names(x$NPP)[1:n_print])
  if(n_layers > n_print){
    cat("...")
  }
  cat("\n")
  cat("extent: ")
  cat(terra::ext(x$NPP)[1:4], "(xmin xmax ymin ymax) \n")
  cat("dimension: ")
  cat(dim(x$NPP)[1:2], "(x y) \n")
  cat("resolution: ")
  cat(terra::res(x$NPP)[1:2], "(x y) \n")
}

WSC_type <- function(x){
  UseMethod("WSC_type")
}
WSC_type.rpp_CASA <- function(x){
  attr(x, "WSC_type")
}


