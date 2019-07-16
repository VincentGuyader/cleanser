#' Safely read in data
#' @description These functions read in data safely by wrapping import function around tryCatch
#' @param ... arguments to pass down to methods
#' @importFrom readODS read_ods
#' @rdname safe_read
safe_read_ods <- function(...){
  
  # adapter n_max pour calculer range
  
    tryCatch(readODS::read_ods(...), 
           error = function(e) data.frame(), 
           warning = function(w) data.frame())
  
}
#' @importFrom readxl read_excel
safe_read_excel <- function(...){
  tryCatch(readxl::read_excel(...), 
           error = function(e) data.frame(), 
           warning = function(w) data.frame())
  
}
safe_read.table <- function(...){
  tryCatch(read.table(...), 
           error = function(e) data.frame(), 
           warning = function(w) data.frame())
  
}
#' @importFrom readr read_rds
safe_read_rds <- function(...){
  tryCatch(readr::read_rds(...), 
           error = function(e) data.frame(), 
           warning = function(w) data.frame())
  
}
