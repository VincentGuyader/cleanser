#' not in
#'
#' @param x a value to 
#' @param table a vector to parse
#'
#' @return a booleen
#' @export
#'
#' @examples
#' "a" %ni% letters
#' "coucou" %ni% letters
#'
`%ni%` <- function(x, table) {
  !(match(x, table, nomatch = 0L) > 0L)}
