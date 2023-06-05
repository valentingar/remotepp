#' Save and load a rpp_CASA model result
#'
#' @description Save a CASA model result to disk. This saves the object as an
#' RDS and the raster layers as tifs to a specified folder.
#'
#' @param x A `CASA()` model result of class rpp_CASA
#'
#'
#'
#'
#' @export
saveCASA <- function(x,
                     file_path,
                     overwrite = FALSE){
  UseMethod("saveCASA")
}

#' @export
saveCASA.rpp_CASA <- function(x,
                              file_path,
                              overwrite = FALSE){

  file_path <- paste0(tools::file_path_sans_ext(file_path), ".RDS")

  dir_path <- paste0(dirname(file_path), "/")
  stopifnot(dir.exists(dir_path))

  NPP_path <- paste0(dir_path, "NPP.tif")
  FAPAR_path <- paste0(dir_path, "FAPAR.tif")
  WSC_path <- paste0(dir_path, "WSC.tif")

  write_WSC <- WSC_type(x) == "raster"

  if (!overwrite && any(file.exists(NPP_path, FAPAR_path, WSC_path) * c(T, T, write_WSC))){
    stop("Some file(s) exist and overwrite is FALSE.")
  }

  NPP <- terra::writeRaster(x$NPP, NPP_path, overwrite = overwrite)
  NPP <- NPP_path
  FAPAR <- terra::writeRaster(x$FAPAR, FAPAR_path, overwrite = overwrite)
  FAPAR <- FAPAR_path

  if(write_WSC){
    WSC <- terra::writeRaster(x$WSC, WSC_path, overwrite = overwrite)
  } else {
    WSC <- x$WSC
  }

  x <- list(NPP = NPP,
            PAR = x$PAR,
            FAPAR = FAPAR,
            TSC1 = x$TSC1,
            TSC2 = x$TSC2,
            WSC = WSC,
            epsilon_max = x$epsilon_max,
            dates = x$dates,
            write_WSC = write_WSC
            )

  saveRDS(x, file_path)
  loadCASA(file_path)
}

#' @export
loadCASA <- function(file_path){
  UseMethod("loadCASA")
}

#' @export
loadCASA.character <- function(file_path){

  file_path <- paste0(tools::file_path_sans_ext(file_path), ".RDS")

  x <- readRDS(file_path)

  NPP <- terra::rast(x$NPP)
  FAPAR <- terra::rast(x$FAPAR)
  if(x$write_WSC){
    WSC <- terra::rast(x$WSC)
  } else {
    WSC <- x$WSC
  }

  rpp_CASA(NPP = NPP,
           PAR = x$PAR,
           FAPAR = FAPAR,
           TSC1 = x$TSC1,
           TSC2 = x$TSC2,
           WSC = WSC,
           epsilon_max = x$epsilon_max,
           dates = x$dates)
}
