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
                               interpolation_function = function(x,y,xout) (approx(x,y,xout))$y,
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

  out_files_tmp <- sapply(1:(n_layers * length(out_dates)),
                          raster::rasterTmpFile) |>
    matrix(ncol = n_layers)

  p <- progressr::progressor(length(out_files_tmp))

  on.exit(raster::removeTmpFiles(h = 0))

  # loop over layers
  for(current_layer in 1:n_layers){

  out_files <- out_files_tmp[ ,current_layer]

  current_in <- raster::stack(input_files, bands = current_layer)
  current_in <- raster::readStart(current_in)

  for (j in seq_along(out_files)){
    current_out <- raster::raster(current_in[[1]])

    current_out <- raster::writeStart(raster::raster(current_in[[1]]),
                                      filename = out_files[j],
                                      overwrite = TRUE)

    block_size <- raster::blockSize(current_out)

    on.exit(current_out <- raster::writeStop(current_out))


    for (i in seq_along(block_size$row)) {
      # read values for block
      current_row <- raster::getValues(current_in,
                                       row = block_size$row[i],
                                       nrows = block_size$nrows[i])

      current_row <-
        t(
          sapply(1:nrow(current_row), function(current_cell) {

            tryCatch(x <-
                       do.call(
                         interpolation_function,
                         c(list(x = in_dates,
                                y = current_row[current_cell, ],
                                xout =  out_dates[j]),
                           interpolation_arguments)
                       ),
                     error = function(cond) NA)

          }))

      current_out <- raster::writeValues(current_out,
                                         current_row,
                                         block_size$row[i])



    }

    current_out <- raster::writeStop(current_out)

    p()

  }

  on.exit(current_in <- raster::readStop(current_in))

  }

  lapply(1:nrow(out_files_tmp), function(i){
    current_out <- terra::rast(out_files_tmp[i, ])
    names(current_out) <- layer_names
    raster::writeRaster(current_out, out_files_final[i])
  })

  NULL
}
