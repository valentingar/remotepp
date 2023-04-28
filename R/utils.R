
ignore_argument_message <- function(arg, arg_name){
  if (!is.null(arg)){
    message(paste0("ignoring not needed arguemnt ", arg_name))
  }
}
