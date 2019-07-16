#' Checks if a file is a csv file
#' @param type_mime character The type mime of the file to check
#' @export
is_csv <- function(type_mime){
  any(type_mime %in% c("text/csv",
                       "text/comma-separated-values",
                       "text/plain",
                       "text/tab-separated-values"))
}

#' Checks if a file is an excel file
#' @param type_mime character The type mime of the file to check
#' @export
is_excel <- function(type_mime){

   any(type_mime %in% c("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                     "application/vnd.ms-excel",
                     "application/msword" # TODO : a virer qd ca sera regle dans get_content_type
                     ))
}

#' Checks if a file is a rds file
#' @param type_mime character The type mime of the file to check
#' @export
is_rds <- function(type_mime){

   any(type_mime %in% c("application/octet-stream"))
}

#' Checks if a file is an ods file
#' @param type_mime character The type mime of the file to check
#' @export
is_ods <- function(type_mime){
   any(type_mime %in% c("application/vnd.oasis.opendocument.spreadsheet"))
}