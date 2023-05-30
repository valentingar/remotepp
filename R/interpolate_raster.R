#' interpolate raster
#'
#' @description
#' Interpolate a raster timeseries from file to file.
#'
#'
#'
#' @export
interpolate_raster <- function(input_files,
                               in_dates,
                               out_dates,
                               out_dir,
                               interpolation_function = function(xin, y, xout) (approx(xin, y, xout, ties = mean))$y,
                               interpolation_arguments = list(), # further arguments passed to interpolation function()
                               overwrite = FALSE,
                               cores = max(c(parallelly::availableCores() - 1, 1))
){

  stopifnot("Not all files exist!" = all(sapply(input_files, file.exists)))
  stopifnot("input_files must be character!" = is.character(input_files))
  stopifnot("in_dates must be the same length as input_files!" =
              length(input_files) == length(in_dates))
  stopifnot("out_dates must be within range of in_dates!" = all(out_dates >= min(in_dates)) & all(out_dates <= max(in_dates)) )

  in_dates_sort_index <- sort(as.numeric(in_dates), index.return = TRUE)$ix

  input_files <- input_files[in_dates_sort_index]
  in_dates <- in_dates[in_dates_sort_index]

  out_files_final <- paste0(out_dir, out_dates, ".tif")

  if(overwrite == TRUE){
    file.remove(out_files_final[file.exists(out_files_final)])
  }

  n_layers <- raster::nlayers(raster::stack(input_files[1]))
  layer_names <- names(raster::stack(input_files[1]))

  out_files_tmp <- sapply(1:n_layers,
                          raster::rasterTmpFile)
  out_files_tmp <- gsub(".grd", ".tif", out_files_tmp) # terra cannot handle large .grd??

  on.exit(raster::removeTmpFiles(h = 0))

  p <- progressr::progressor(n_layers)

  # loop over layers
 purrr::map(1:n_layers, function(current_layer){

  out_file <- out_files_tmp[current_layer]

  current_in_complete <- raster::stack(input_files, bands = current_layer)
  current_in_complete <- as(current_in_complete, "SpatRaster")

  current_out <- terra::app(current_in_complete,
                            make_interpolation_function_safe(interpolation_function),
                            filename = out_file,
                            overwrite = TRUE,
                            xin = in_dates,
                            xout = out_dates,
                            cores = cores)

  p()

  })

  furrr::future_map(1:length(out_files_final), function(i){
    #current_out <- raster::stack(out_files_tmp[1, ], bands = i)
    #current_out <- terra::rast(out_files_tmp[1, ], lyrs = rep(i, ncol(out_files_tmp)))
    #names(current_out) <- layer_names
    #raster::writeRaster(current_out, out_files_final[i])

    current_out <- terra::rast(lapply(out_files_tmp, function(x) terra::rast(x, lyrs = i)))
    names(current_out) <- layer_names
    #current_out
    terra::writeRaster(current_out, out_files_final[i])
  })

  #NULL
}


### helper ---------------------------------------------------------------------

make_interpolation_function_safe <- function(interpolation_function){
  function(xin, y, xout, ...){
    tryCatch(interpolation_function(xin, y, xout), error = function(cond) rep(NA, length(xout)))
  }
}
