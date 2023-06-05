#' Save and load a rpp_MODIS model result
#'
#' @description Save a MODIS model result to disk. This saves the object as an
#' RDS and the raster layers as tifs to a specified folder.
#'
#' @param x A `MODIS()` model result of class rpp_MODIS
#'
#'
#'
#'
#' @export
saveMODIS <- function(x,
                     file_path,
                     overwrite = FALSE){
  UseMethod("saveMODIS")
}

#' @export
saveMODIS.rpp_MODIS <- function(x,
                              file_path,
                              overwrite = FALSE){

  file_path <- paste0(tools::file_path_sans_ext(file_path), ".RDS")

  dir_path <- paste0(dirname(file_path), "/")
  stopifnot(dir.exists(dir_path))

  GPP_path <- paste0(dir_path, "GPP.tif")
  FAPAR_path <- paste0(dir_path, "FAPAR.tif")

  if (!overwrite && any(file.exists(GPP_path, FAPAR_path))){
    stop("Some file(s) exist and overwrite is FALSE.")
  }

  GPP <- terra::writeRaster(x$GPP, GPP_path, overwrite = overwrite)
  GPP <- GPP_path
  FAPAR <- terra::writeRaster(x$FAPAR, FAPAR_path, overwrite = overwrite)
  FAPAR <- FAPAR_path


  x <- list(GPP = GPP,
            PAR = x$PAR,
            FAPAR = FAPAR,
            TMIN_scalar = x$TMIN_scalar,
            VPD_scalar = x$VPD_scalar,
            epsilon_max = x$epsilon_max,
            dates = x$dates
  )

  saveRDS(x, file_path)
  loadMODIS(file_path)
}

#' @export
loadMODIS <- function(file_path){
  UseMethod("loadMODIS")
}

#' @export
loadMODIS.character <- function(file_path){

  file_path <- paste0(tools::file_path_sans_ext(file_path), ".RDS")

  x <- readRDS(file_path)

  GPP <- terra::rast(x$GPP)
  FAPAR <- terra::rast(x$FAPAR)

  rpp_MODIS(GPP = GPP,
           PAR = x$PAR,
           FAPAR = FAPAR,
           TMIN_scalar = x$TMIN_scalar,
           VPD_scalar = x$VPD_scalar,
           epsilon_max = x$epsilon_max,
           dates = x$dates)
}
