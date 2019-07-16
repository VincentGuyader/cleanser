#' Corrects file extension 
#'
#' @param mime mime type
#' @param ... not used
#' @param file A file to read in
#' @importFrom mime mimemap
#' @return Correct file name
#' @description This function uses get_content_type() to append the correct file extension to the file name
#' the user wants to read in. This is useful in cases where the file extension is wrong, or where
#' the file extension was removed by mistake. 
#' @examples
#' \dontrun{
#' file <- system.file("dataset","wrong_is_xlsx.csv",
#' package = "cleanser")
#' correct_extension(file)
#' }
correct_extension <- function(file, mime =get_content_type(file), ...){

  #TODO virer qd corrigé
  if (mime[1] %in% c("application/msword")) {
    # message("correction doc -> xls")
    mime <- "application/vnd.ms-excel"
    }
  if (mime[1] %in% c("application/vnd.openxmlformats-officedocument.wordprocessingml.document")) {
    if ( ! app_prod()){
    message("correction doc -> xls")
    }
    mime <- "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    }

  correct_extension <- names(mime::mimemap[
    which(mime::mimemap %in% mime)[1]
    ])
  if (is.na(correct_extension)){
    
    correct_extension <-"unknow"
  }
  
  
    if (tools::file_ext(file) == correct_extension) {
    return(file)
    }

  # si pas bonne extension on en fait une copie avec la bonne extention , dont on retourne le chemin
  # out <- tempfile(fileext = glue(".{correct_extension}"), ...) # les '...' sont utiles pour changer le tmpdir, pour débogage par exemple
  # file.copy(file, to = out, copy.mode = TRUE, copy.date = FALSE)
  out <-  with_this_extension(file = file, correct_extension = correct_extension)
  out
}