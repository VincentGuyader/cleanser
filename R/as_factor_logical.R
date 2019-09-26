#' Title
#'
#' @param x 
#' @param ... 
#'
#' @return
#' @export
#' @method as_factor logical 
#' @S3method as_factor logical
as_factor.logical <- function(x,...){
  as_factor(as.character(x))
}
