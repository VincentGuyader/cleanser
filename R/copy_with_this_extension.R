#' Create a copy of a file with the correct extension
#'
#' @param file a file containing the data
#' @param correct_extension character the correct extension
#' @param ... further arguments to pass down to methods
#'
#' @return Nothing; this function is used for its side effect (create a temporary copy of the file
#' with the correct extension)
#' @export
with_this_extension <- function(file, correct_extension, ...){
  
  if (tools::file_ext(file) == correct_extension){
    return(file)
  }
  
  out <- tempfile(fileext = glue(".{correct_extension}"), ...) # les '...' sont utiles pour changer le tmpdir, pour débogage par exemple
  file.copy(file, to = out, copy.mode = TRUE, copy.date = FALSE)
  out
}
