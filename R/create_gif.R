#' Create a gif from a time series of raster files
#'
#' @description
#' Create an animated gif file of a time series of raster data.
#'
#' @inheritParams map_d2d
#' @param plot_FUN function used to plot the data
#' @param out_file character path to the output file to be generated.
#'
#'
#' @export

create_gif <- function(input_files,
                       plot_FUN,
                       out_file){

  stopifnot("out_file must be a .gif" = tools::file_ext(out_file) == "gif")


  first_file <- raster::stack(input_files[1])

  current_dir <- tempdir()
  current_height = nrow(first_file)
  current_width = ncol(first_file)

  rm(first_file)

  p <- progressr::progressor(length(input_files))

  purrr::map(input_files,
             create_png_for_gif,
             plot_FUN,
             current_dir,
             current_height,
             current_width,
             p)

  message("finished plotting, creating gif...")

  filenames_bare <- tools::file_path_sans_ext(basename(input_files))

  imgs <- paste0(current_dir,"/", filenames_bare , ".png")
  img_list <- lapply(imgs, magick::image_read)

  ## join the images together
  img_joined <- magick::image_join(img_list)

  ## animate at 2 frames per second
  img_animated <- magick::image_animate(img_joined, fps = 20)

  ## view animated image
  img_animated

  ## save to disk
  magick::image_write(image = img_animated,
                      path = out_file)

  on.exit(unlink(current_dir, recursive = TRUE))




}


create_png_for_gif <- function(current_file,
                               plot_FUN,
                               current_dir,
                               current_height,
                               current_width,
                               p){
  current_raster <- raster::stack(current_file)
  current_filename <- tools::file_path_sans_ext(basename(current_file))

  png(filename = paste0(
    current_dir, "/", current_filename, ".png"),
      width = current_width,
      height = current_height)

  plot_FUN(current_raster)

  dev.off()

  p()

}
