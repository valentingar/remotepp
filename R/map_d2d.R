#' Apply a function disk to disk

#' @description
#' Apply a function to a list of files and store again on disk.
#' Use parallel processing by running `future::plan(future::sequential)` beforehand.
#'
#' @param input_files character list of files
#' @param read_FUN function to read in a file from `input_files`
#' @param FUN function to apply to each file in `input_files`
#' @param save_FUN function to save the object. Must match the format `f(object, path)`
#' @param out_dir character path where to store the output
#' @param overwrite logical
#' @param ... additional arguments passed to FUN
#'
#'
#' @export

map_d2d <- function(input_files,
                       read_FUN,
                       change_FUN,
                       save_FUN,
                       out_dir,
                       overwrite,
                       ...){


  stopifnot("out_dir does not exist!" = dir.exists(out_dir))
  stopifnot("Not all input_files exist!" = all(file.exists(input_files)))


  out_names <- basename(input_files)
  out_files <- paste0(out_dir, "/", out_names)

  stopifnot("Output files exist and overwrite is FALSE" =
              (!any(file.exists(out_files)) | overwrite))

  if (overwrite & any(file.exists(out_files))){
    lapply(out_files, file.remove)
  }


  furrr::future_map2(
    input_files,
    out_files,
    function(input_file,
             out_file) {

    current_object <- read_FUN(input_file)

    current_object <- change_FUN(current_object)

    save_FUN(current_object, out_file)

    rm(current_object)

  })




}
