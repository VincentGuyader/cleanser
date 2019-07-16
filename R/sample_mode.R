#' Returns the mode of a vector
#' @param x A list of values
#' @return Returns the mode of x, preserving the type of x
#' @export
#' @examples
#' x <- c(3,3,3,4,5)
#' sample_mode(x)
sample_mode <- function(x){
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
