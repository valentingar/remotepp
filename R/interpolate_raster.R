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
                               interpolation_function = function(x, y, xout) (approx(x, y, xout, ties = mean))$y,
                               interpolation_arguments = list(), # further arguments passed to interpolation function()
                               overwrite = FALSE
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
                          raster::rasterTmpFile) |>
    matrix(ncol = n_layers)

  on.exit(raster::removeTmpFiles(h = 0))

  # loop over layers
 purrr::map(1:n_layers, function(current_layer){

  out_file <- out_files_tmp[1, current_layer]

  current_in_complete <- raster::stack(input_files, bands = current_layer)

  current_tiles <- terra::makeTiles(terra::rast(current_in_complete),
                                    ceiling(sqrt(prod(dim(current_in_complete)[1:2]) / parallelly::availableCores())),
                                    filename = raster::rasterTmpFile())

  p <- progressr::progressor(length(current_tiles))


  furrr::future_map(current_tiles, function(current_in_filename){

    current_in <- raster::stack(current_in_filename)

    current_in <- raster::readStart(current_in)

    current_brick <- raster::brick(nrows = nrow(current_in),
                                   ncols = ncol(current_in),
                                   nl = length(out_dates),
                                   crs = raster::crs(current_in))
    raster::extent(current_brick) <- raster::extent(current_in)

    block_size <- raster::blockSize(current_brick)

    current_out_filename <- paste0(tools::file_path_sans_ext(current_in_filename),
                                   "_out.",
                                   tools::file_ext(current_in_filename))

    current_out <- raster::writeStart(current_brick,
                                      filename = current_out_filename,
                                      overwrite = TRUE)




    for (i in seq_along(block_size$row)) {
      # read values for block
      current_row <- raster::getValues(current_in,
                                       row = block_size$row[i],
                                       nrows = block_size$nrows[i])

      current_row <- t(apply(current_row,
                             FUN = function(x){

                               tryCatch(x <-
                                          do.call(
                                            interpolation_function,
                                            c(list(x = in_dates,
                                                   y = x,
                                                   xout =  out_dates),
                                              interpolation_arguments)
                                          ),
                                        error = function(cond) rep(NA, length(out_dates)))

                             },
                             MARGIN = 1))

      current_out <- raster::writeValues(current_out,
                                         current_row,
                                         block_size$row[i])



    }

    current_out <- raster::writeStop(current_out)
    on.exit(current_in <- raster::readStop(current_in))

    p()

    NULL



  })

  #terra::vrt(paste0(tools::file_path_sans_ext(current_tiles),
  #                  "_out.",
  #                  tools::file_ext(current_tiles)),
  #           filename = out_file,
  #           overwrite = TRUE)

  current_interpolated <-
  do.call(raster::merge,
          lapply(paste0(tools::file_path_sans_ext(current_tiles),
                        "_out.",
                        tools::file_ext(current_tiles)),
                 raster::stack))

  raster::writeRaster(current_interpolated,
                      filename = out_file,
                      overwrite = TRUE)


  })

  purrr::map(1:length(out_files_final), function(i){
    #current_out <- raster::stack(out_files_tmp[1, ], bands = i)
    current_out <- terra::rast(out_files_tmp[1, ], lyrs = rep(i, ncol(out_files_tmp)))
    names(current_out) <- layer_names
    terra::writeRaster(current_out, out_files_final[i])
  })

  NULL
}
